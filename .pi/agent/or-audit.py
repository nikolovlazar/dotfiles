#!/usr/bin/env python3
"""
or-audit.py — OpenRouter provider-jurisdiction audit & model-config sync for pi.

Why
  Keeps prompts and code away from providers under PRC jurisdiction, per my
  own data-handling policy, and prefers zero-data-retention endpoints. This is
  a personal policy, not a judgment about any company or country: edit
  CN_WATCHLIST and use --geo / --watch to fit your own.

What it does
  scan      Scan ~/.pi/agent/sessions/**/*.jsonl for models you actually used
            (openrouter provider only) and OpenRouter generation IDs.
  audit     For each generation: GET /api/v1/generation → who served it, flag
            providers on the watchlist or matching the HQ filter.
  coverage  For each model: GET /models/{author}/{slug}/endpoints → which CN
            providers serve it → compare with the model's openRouterRouting
            override in model.json → report gaps (unignored CN slugs).
  sync      (--sync) Fix the gaps: merge missing CN slugs into each model's
            ignore list; add data_collection:"deny" + zdr:true when safe (only
            if >=1 endpoint stays eligible under the filters). Never removes
            your edits; backs up model.json before writing.

Modes / flags
  (default)    coverage + audit report (read-only)
  --sync       report + fix model.json
  --probe      tiny live request for --model (default $PI_MODEL or default
               model in settings.json) using current prefs → who serves it
  --probe-all  probe every openrouter model found in history
  --models     explicit comma-separated model list (skip history scan)
  --limit N    audit only the last N generations (default 500)
  --no-gens    skip the generation audit
  --geo,--watch  extra HQ codes / slugs to flag (in addition to built-ins)
  --config     path to model.json (default ~/.pi/agent/model.json)

Auth   OPENROUTER_API_KEY env var, else the OAuth token in ~/.pi/agent/auth.json.
Exit   0 nothing matched · 1 watchlist gap/flag found (or remaining after sync) ·
       2 operational error.

Default PRC-jurisdiction watchlist. Best-effort, not authoritative: membership
is my own judgment and may be incomplete, outdated, or wrong for some entries;
some entries are model labs rather than inference hosts; OpenRouter's
self-reported domicile may say SG or be empty, so this is a watchlist, not an
HQ filter. A clean result means nothing matched, not that no such provider
served a request:
  z-ai siliconflow alibaba moonshotai minimax stepfun
  deepseek tencent baidu xiaomi nex-agi streamlake
HQ filter (geo) flags providers whose self-reported headquarters matches
(default CN; extend with --geo, e.g. --geo CN,HK). The field is often empty.
"""

import argparse
import concurrent.futures
import json
import os
import re
import shutil
import sys
import time
import urllib.error
import urllib.request
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path

HOME = Path.home()
AUTH_PATH = HOME / ".pi/agent/auth.json"
SETTINGS_PATH = HOME / ".pi/agent/settings.json"
SESSIONS_DIR = HOME / ".pi/agent/sessions"
DEFAULT_CONFIG = HOME / ".pi/agent/model.json"
API = "https://openrouter.ai/api/v1"

# pi/OpenRouter generation ids: gen-<epoch>-<base62 hash>
GEN_RE = re.compile(r"\bgen-\d{8,12}-[A-Za-z0-9]{16,26}\b")

GEO_DEFAULT = {"CN"}
CN_WATCHLIST = {
    "z-ai", "siliconflow", "alibaba", "moonshotai", "minimax", "stepfun",
    "deepseek", "tencent", "baidu", "xiaomi", "nex-agi", "streamlake",
}


def get_token():
    tok = os.environ.get("OPENROUTER_API_KEY")
    if tok:
        return tok
    return json.loads(AUTH_PATH.read_text())["openrouter"]["access"]


def api_get(path, token, timeout=30):
    req = urllib.request.Request(
        API + path, headers={"Authorization": f"Bearer {token}"}
    )
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return json.load(r)


def api_post(path, token, body, headers=None, timeout=60):
    h = {"Authorization": f"Bearer {token}", "Content-Type": "application/json"}
    h.update(headers or {})
    req = urllib.request.Request(API + path, data=json.dumps(body).encode(), headers=h)
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return json.load(r)


def load_providers(token):
    """(by_display_name, by_slug) provider records from GET /providers."""
    try:
        data = api_get("/providers", token)["data"]
    except Exception:
        return {}, {}
    by_name, by_slug = {}, {}
    for p in data:
        by_name[p["name"]] = p
        by_slug[p["slug"]] = p
    return by_name, by_slug


def load_zdr_names(token):
    """Set of endpoint names ("Provider | model-...") that are ZDR."""
    try:
        return {e["name"] for e in api_get("/endpoints/zdr", token)["data"]}
    except Exception:
        return set()


def scan_history():
    """( {(provider, model): msg_count}, [gen ids] ) across all session files."""
    models, gens = Counter(), []
    files = sorted(SESSIONS_DIR.rglob("*.jsonl"), key=lambda p: p.stat().st_mtime)
    for f in files:
        text = f.read_text(errors="replace")
        gens.extend(GEN_RE.findall(text))
        for line in text.splitlines():
            if '"modelId"' not in line and '"model"' not in line:
                continue
            try:
                d = json.loads(line)
            except ValueError:
                continue
            if d.get("modelId"):
                models[(d.get("provider"), d["modelId"])] += 1
            msg = d.get("message")
            if isinstance(msg, dict) and msg.get("model"):
                models[(d.get("provider") or msg.get("provider"), msg["model"])] += 1
    return models, list(dict.fromkeys(gens))  # dedupe gens, keep order


def endpoints_for(token, model):
    """Endpoints serving `model`, or None if the model isn't on OpenRouter."""
    try:
        return api_get(f"/models/{model}/endpoints", token)["data"]["endpoints"]
    except urllib.error.HTTPError as e:
        if e.code == 404:
            return None
        raise


def flag_of(slug, hq, geo, watch):
    flags = []
    if slug in CN_WATCHLIST:
        flags.append("watchlist-cn")
    if slug in watch:
        flags.append("watchlist")
    if hq and str(hq).upper() in geo:
        flags.append(f"hq-{str(hq).upper()}")
    return ",".join(flags)


def cn_slugs_serving(eps, by_name, geo, watch):
    """Deduped, sorted slugs of watchlist-flagged providers among endpoints `eps`."""
    slugs = set()
    for e in eps:
        rec = by_name.get(e.get("provider_name")) or {}
        slug = rec.get("slug")
        if slug and flag_of(slug, rec.get("headquarters"), geo, watch):
            slugs.add(slug)
    return sorted(slugs)


def routing_for(config, model):
    """Current openRouterRouting override for `model` (or None)."""
    prov = (config.get("providers") or {}).get("openrouter") or {}
    override = (prov.get("modelOverrides") or {}).get(model) or {}
    return (override.get("compat") or {}).get("openRouterRouting")


def merged_routing(existing, cn_slugs, eps, zdr_names, by_name):
    """Merge CN slugs into `existing` routing; return (new or None, notes).

    Conservative: keeps every existing key/value, only adds. zdr/data_collection
    defaults are applied only when the user hasn't set them; zdr:true only when
    at least one endpoint remains eligible under ignore+ZDR filters.
    """
    notes = []
    if existing is None:
        existing = {}
    new_ignore = sorted(set(existing.get("ignore") or []) | set(cn_slugs))
    eligible = [
        e for e in eps
        if (by_name.get(e.get("provider_name")) or {}).get("slug") not in set(new_ignore)
        and e.get("name") in zdr_names
    ]
    merged = dict(existing)
    merged["ignore"] = new_ignore
    if "data_collection" not in merged:
        merged["data_collection"] = "deny"
        notes.append("set data_collection=deny")
    if "zdr" not in merged:
        if eligible:
            merged["zdr"] = True
            notes.append("set zdr=true")
        else:
            notes.append("NOT setting zdr (no endpoint survives ignore+ZDR filters)")
    if merged == existing:
        return None, notes
    return merged, notes


def fetch_generation(gid, token):
    try:
        return gid, api_get(f"/generation?id={gid}", token).get("data"), None
    except urllib.error.HTTPError as e:
        return gid, None, f"HTTP {e.code}"
    except Exception as ex:  # noqa: BLE001
        return gid, None, f"{type(ex).__name__}: {ex}"


def audit_generations(token, gen_ids, by_name, geo, watch, limit):
    """Fetch each generation record; return (rows, failures, flagged_rows)."""
    if limit:
        gen_ids = gen_ids[-limit:]
    rows, failures, flagged = [], [], []
    with concurrent.futures.ThreadPoolExecutor(max_workers=8) as ex:
        futs = [ex.submit(fetch_generation, g, token) for g in gen_ids]
        for fut in concurrent.futures.as_completed(futs):
            gid, data, err = fut.result()
            if err or not data:
                failures.append((gid, err or "no data"))
                continue
            name = data.get("provider_name") or "?"
            rec = by_name.get(name) or {}
            slug = rec.get("slug", "?")
            fl = flag_of(slug, rec.get("headquarters"), geo, watch)
            row = {
                "created_at": data.get("created_at", "?"),
                "model": data.get("model", "?"),
                "provider": name,
                "slug": slug,
                "flags": fl,
                "attempts": len(data.get("provider_responses") or []),
            }
            rows.append(row)
            if fl:
                flagged.append(row)
    rows.sort(key=lambda r: str(r["created_at"]))
    return rows, failures, flagged


def default_model():
    m = os.environ.get("PI_MODEL")
    if m:
        return m
    try:
        s = json.loads(SETTINGS_PATH.read_text())
        m = s.get("defaultModel")
        if m and "/" in m:
            return m
    except Exception:
        pass
    return None


def probe_model(token, model, config, by_name, geo, watch):
    """One tiny live request with the model's current routing prefs."""
    prefs = routing_for(config, model)
    body = {
        "model": model,
        "max_tokens": 16,
        "messages": [{"role": "user", "content": "Reply with the single word: pong"}],
    }
    if prefs:
        body["provider"] = prefs
    resp = api_post(
        "/chat/completions", token, body, {"X-OpenRouter-Metadata": "enabled"}
    )
    md = resp.get("openrouter_metadata") or {}
    cands = (md.get("endpoints") or {}).get("available") or []
    flagged = []
    for c in cands:
        name = c["provider"]
        rec = by_name.get(name) or {}
        slug = rec.get("slug")
        if slug and flag_of(slug, rec.get("headquarters"), geo, watch):
            flagged.append((name, slug, rec.get("headquarters")))
    print(f"probe {model}:")
    print(f"  prefs      : {json.dumps(prefs) if prefs else '(none — default routing!)'}")
    print(f"  served by : {resp.get('provider')} ({md.get('summary')})")
    print(f"  candidates : {len(cands)}")
    if flagged:
        for name, slug, hq in flagged:
            print(f"  !! FLAGGED candidate still eligible: {name} (slug={slug}, HQ={hq})")
        return 1
    print("  none matched the watchlist/HQ filter (best-effort)")
    return 0


def main():
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    ap.add_argument("--sync", action="store_true", help="fix model.json gaps")
    ap.add_argument("--probe", action="store_true", help="live-probe one model")
    ap.add_argument("--probe-all", action="store_true", help="live-probe all models")
    ap.add_argument("--models", help="explicit comma-separated model list")
    ap.add_argument("--model", help="model for --probe (default $PI_MODEL)")
    ap.add_argument("--limit", type=int, default=500, help="audit last N generations")
    ap.add_argument("--no-gens", action="store_true", help="skip generation audit")
    ap.add_argument("--geo", default="", help="extra HQ codes to flag")
    ap.add_argument("--watch", default="", help="extra slugs to flag")
    ap.add_argument("--config", default=str(DEFAULT_CONFIG), help="model.json path")
    args = ap.parse_args()

    geo = {g.strip().upper() for g in args.geo.split(",") if g.strip()} | GEO_DEFAULT
    watch = {s.strip() for s in args.watch.split(",") if s.strip()}

    try:
        token = get_token()
        config = json.loads(Path(args.config).read_text())
    except Exception as ex:  # noqa: BLE001
        print(f"error: {ex}", file=sys.stderr)
        sys.exit(2)

    # ---- discover models -------------------------------------------------
    if args.models:
        model_list = [m.strip() for m in args.models.split(",") if m.strip()]
        gen_ids = []
    else:
        print("scanning session history ...")
        models_counter, gen_ids = scan_history()
        model_list = sorted(
            {m for (provider, m), _ in models_counter.items() if provider == "openrouter"}
        )
        skipped = sorted({m for (provider, m) in models_counter if provider != "openrouter"})
        if skipped:
            print(f"  skipping non-openrouter providers: {', '.join(sorted(skipped))}")
        print(f"  openrouter models used: {len(model_list)}; generations: {len(gen_ids)}")

    if args.probe or args.probe_all:
        targets = model_list if args.probe_all else [args.model or default_model()]
        targets = [t for t in targets if t]
        if not targets:
            print("no probe target (set --model or $PI_MODEL)", file=sys.stderr)
            sys.exit(2)
        by_name, _ = load_providers(token)
        rc = 0
        for m in targets:
            try:
                rc = max(rc, probe_model(token, m, config, by_name, geo, watch))
            except Exception as ex:  # noqa: BLE001
                print(f"probe {m} failed: {ex}", file=sys.stderr)
                rc = max(rc, 2)
        sys.exit(rc)

    # ---- coverage + sync -------------------------------------------------
    by_name, by_slug = load_providers(token)
    zdr_names = load_zdr_names(token)
    print(f"providers: {len(by_slug)} loaded; ZDR endpoints known: {len(zdr_names)}\n")

    print("== model coverage ==")
    gaps_before = []      # (model, missing slugs)
    to_write = {}         # model -> new routing (only changed)
    for model in model_list:
        try:
            eps = endpoints_for(token, model)
        except Exception as ex:  # noqa: BLE001
            print(f"  {model}: endpoint fetch failed ({ex})")
            continue
        if eps is None:
            print(f"  {model}: not served on OpenRouter (or delisted) — skipped")
            continue
        cn = cn_slugs_serving(eps, by_name, geo, watch)
        existing = routing_for(config, model)
        have = set((existing or {}).get("ignore") or [])
        missing = [s for s in cn if s not in have]
        status = "ok" if not missing else f"GAP: {missing}"
        print(
            f"  {model}: endpoints={len(eps)} cn-serving={cn or '[]'} "
            f"config-ignore={sorted(have) or '[]'} -> {status}"
        )
        if missing:
            gaps_before.append((model, missing))
        if args.sync and missing:
            new_routing, notes = merged_routing(
                existing, cn, eps, zdr_names, by_name
            )
            if new_routing:
                to_write[model] = new_routing
                for n in notes:
                    print(f"    sync: {n}")

    if args.sync and to_write:
        cfg_path = Path(args.config)
        backup = cfg_path.with_suffix(
            cfg_path.suffix + f".bak-{datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ')}"
        )
        shutil.copy2(cfg_path, backup)
        prov = config.setdefault("providers", {}).setdefault("openrouter", {})
        overrides = prov.setdefault("modelOverrides", {})
        for model, routing in to_write.items():
            compat = overrides.setdefault(model, {}).setdefault("compat", {})
            compat["openRouterRouting"] = routing
        cfg_path.write_text(json.dumps(config, indent=2) + "\n")
        print(f"\nmodel.json updated ({len(to_write)} model(s)); backup: {backup}")
    elif args.sync:
        print("\nsync: model.json already covers everything — no changes")

    # ---- generation audit -------------------------------------------------
    flagged_rows = []
    if not args.no_gens and gen_ids:
        print(f"\n== generation audit (last {args.limit} of {len(gen_ids)}) ==")
        rows, failures, flagged_rows = audit_generations(
            token, gen_ids, by_name, geo, watch, args.limit
        )
        for r in rows:
            t = str(r["created_at"]).replace("T", " ")[:19]
            print(
                f"  {t}  {r['model'][:28]:<28} {r['provider']:<16} "
                f"(slug={r['slug']:<14} att={r['attempts']}) {r['flags'] or '-'}"
            )
        if failures:
            print(f"  unfetchable: {len(failures)} (e.g. {failures[0]})")

    # ---- summary ----------------------------------------------------------
    remaining = [
        (m, miss) for (m, miss) in gaps_before
        if not (m in to_write and all(s in to_write[m]["ignore"] for s in miss))
    ]
    print("\n== summary ==")
    print(f"models checked     : {len(model_list)}")
    print(f"gaps found          : {len(gaps_before)}")
    if args.sync:
        print(f"gaps fixed by sync  : {len(gaps_before) - len(remaining)}")
    for m, miss in remaining:
        hint = "use --sync" if not args.sync else "manual review needed"
        print(f"  !! {m} unignored CN slugs: {miss} ({hint})")
    print(f"flagged generations : {len(flagged_rows)}")
    for r in flagged_rows:
        print(f"  !! {r['created_at']} served by {r['provider']} ({r['flags']})")
    sys.exit(1 if (remaining or flagged_rows) else 0)


if __name__ == "__main__":
    main()