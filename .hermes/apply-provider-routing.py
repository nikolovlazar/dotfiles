#!/usr/bin/env python3
"""Sync OpenRouter provider routing from Pi's model.json into Hermes' config.yaml.

Source of truth: ~/.pi/agent/model.json (providers.openrouter.modelOverrides.*.
compat.openRouterRouting). Generates a `provider_routing.models` block in
~/.hermes/config.yaml between BEGIN/END markers; re-running replaces it in place.
Nothing else in config.yaml is touched; a .bak is written first. Stdlib only.

Mapping: ignore -> ignore, only -> only, data_collection -> data_collection.
`zdr` has no Hermes equivalent and is skipped.

Usage: apply-provider-routing.py [--check]   (--check: exit 1 if out of sync)
"""
import json, re, shutil, sys
from pathlib import Path

HOME = Path.home()
SRC = HOME / ".pi/agent/model.json"
DST = HOME / ".hermes/config.yaml"
BEGIN = "# >>> provider_routing (generated from ~/.pi/agent/model.json by apply-provider-routing.py; do not edit) >>>"
END = "# <<< provider_routing <<<"
KEYS = ("only", "ignore")


def build():
    ov = json.loads(SRC.read_text())["providers"]["openrouter"]["modelOverrides"]
    lines = ["provider_routing:", '  data_collection: "deny"', "  models:"]
    for model, v in sorted(ov.items()):
        r = (v.get("compat") or {}).get("openRouterRouting") or {}
        body = [f"      {k}: {json.dumps(r[k])}" for k in KEYS if r.get(k)]
        if r.get("data_collection"):
            body.append(f"      data_collection: {json.dumps(r['data_collection'])}")
        if body:
            lines += [f"    {json.dumps(model)}:"] + body
    return f"{BEGIN}\n" + "\n".join(lines) + f"\n{END}\n"


def main():
    block = build()
    cfg = DST.read_text()
    pat = re.compile(re.escape(BEGIN) + r".*?" + re.escape(END) + r"\n?", re.S)
    if pat.search(cfg):
        new = pat.sub(lambda _: block, cfg)
    elif re.search(r"^provider_routing:", cfg, re.M):
        sys.exit("config.yaml has an unmanaged provider_routing: remove it first")
    else:
        new = cfg.rstrip("\n") + "\n\n" + block
    if new == cfg:
        print("in sync")
        return 0
    if "--check" in sys.argv:
        print("OUT OF SYNC")
        return 1
    shutil.copy2(DST, DST.with_suffix(".yaml.bak-routing"))
    DST.write_text(new)
    print("updated", DST)
    return 0


sys.exit(main())
