# Feature flags

Manage Railway feature flags (Signals): typed defaults, targeting rules, and runtime reads via the SDK or GraphQL.

## What they are

Railway feature flags are **project- or workspace-scoped** configuration values resolved at read time from a registry (not environment variables). Each flag has:

- A **type**: `bool`, `string`, `number`, or `json`
- A **default** value used when no rule matches
- Optional **targeting rules** (attribute comparisons and rollout percentages)

Project flags are managed in **Project Settings → Feature Flags**. Workspace flags are visible there read-only.

Runtime apps read **one scope** at a time (`project:<id>` or `workspace:<id>`) via the [TypeScript SDK](https://github.com/railwayapp/railway-ts-sdk) or GraphQL — flags from other scopes are separate registries.

## Agent / MCP workflow

Prefer **Remote MCP** tools when OAuth-scoped project access is enough:

| Tool | Access | Purpose |
|---|---|---|
| `list-feature-flags` | viewer | List project flags; includes workspace flags when present |
| `get-feature-flag` | viewer | Inspect one flag (`scope`: `project` or `workspace`) |
| `set-feature-flag` | admin | Create a flag or update its default |
| `delete-feature-flag` | admin | Delete a project-scoped flag |

Always pass explicit `projectId` (from a Railway URL or `railway status --json`).

```text
List feature flags for project 6adb5ae3-0e3a-4ead-b42c-1fd36f217ffb
```

```text
Set feature flag checkout-v2 on project 6adb5ae3-0e3a-4ead-b42c-1fd36f217ffb to true (bool)
```

For targeting rules and rollouts, use the CLI commands below when MCP has no matching rule tool. The dashboard or GraphQL (`signalRuleSet`) remains a fallback.

## CLI defaults, rules, and rollouts

CLI 5.24+ exposes `railway flag` (alias `railway flags`). Use `--scope project:<id>` for explicit targeting; `--project` is not a flag option. On 5.26.2+, omitted scope can come from the project token or linked project.

```bash
railway flag list --scope project:<project-id> --json
railway flag list --scope project:<project-id> --full
railway flag set checkout-v2 true --scope project:<project-id>
railway flag set theme blue --type string --scope project:<project-id>
railway flag set checkout-v2 true --scope project:<project-id> --when 'plan == "enterprise"' --rule-id enterprise
railway flag set checkout-v2 true --scope project:<project-id> --when 'bucket(key) < 0.25' --rule-id rollout-25
railway flag unset checkout-v2 --scope project:<project-id> --rule-id enterprise
railway flag delete checkout-v2 --scope project:<project-id>
```

`set` changes the default unless `--when` is supplied. Rules accept a CEL expression subset or raw JSON; `bucket(key)` gives deterministic percentage targeting using the evaluation context's key. Use a stable `--rule-id` when updating a rule, and list/read back the rules afterward. `unset` removes one rule; `delete` removes the entire flag. CLI mutations target project flags; do not assume workspace mutation support.

Types are inferred unless `--type bool|string|number|json` is given. `--force` permits replacing a flag's type and **clears its rules**; use it only when that replacement is intended.

## GraphQL fallback

For API operations beyond these commands, inspect the public GraphQL API (`signals`, `signalCreate`, `signalDefaultSet`, `signalRuleSet`, `signalDelete`) using [request.md](request.md). Owners use `project:<projectId>` or `workspace:<workspaceId>`; access remains subject to the API's scope and permission checks.

```bash
railway api \
  'query projectSignals($owner: String!) {
    signals(owner: $owner) { name type default rules version }
  }' \
  --variables '{"owner":"project:<projectId>"}'
```

## Runtime SDK

In deployed services, use the Railway SDK flags module:

```typescript
import { flags } from "railway";

await flags.init({ scope: { projectId: process.env.RAILWAY_PROJECT_ID! } });

const enabled = flags.getBoolean("checkout-v2", {
  targetingKey: userId,
  attributes: { plan: "enterprise" },
});
```

Poll interval defaults are suitable for most apps; flags refresh when registry versions change.

## Do not confuse with

- **Environment variables** — static per-environment config (`railway variable set`). Use flags when you need typed defaults, targeting, and central registry semantics.
- **Priority Boarding / account toggles** — internal beta switches on your Railway account, unrelated to project feature flags.
- **Platform feature flags** — Railway staff tooling only.

## Dashboard

Human-friendly CRUD: open the project → **Settings → Feature Flags**. Workspace-scoped flags appear in a read-only section when they exist.

## Validated against

- Docs: [Feature flags](https://docs.railway.com/feature-flags), [CLI flags](https://docs.railway.com/cli/flag)
- CLI source (v5.49.1): [flag.rs](https://github.com/railwayapp/cli/blob/v5.49.1/src/commands/flag.rs), [signals.rs](https://github.com/railwayapp/cli/blob/v5.49.1/src/controllers/signals.rs)
