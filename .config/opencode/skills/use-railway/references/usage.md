# Usage and spending limits

Use `railway usage` (CLI 5.25+) to explain billed resource usage and project/service costs. CLI 5.26+ adds explicit workspace versus Railway Agent spending-limit targets. For live CPU, memory, network, or latency measurements, use [operate.md](operate.md).

## Read usage

Resolve the workspace from the user's request or `railway whoami --json`. These commands are workspace-scoped, not scoped by the current service or environment:

```bash
railway usage --workspace <workspace> --json
railway usage --workspace <workspace> --period previous --json
railway usage --workspace <workspace> --period 2026-08 --json
railway usage projects --workspace <workspace> --json
railway usage projects --workspace <workspace> --limit 10 --json
railway usage projects --workspace <workspace> --project <project> --period current --json
```

`--period` accepts `current`, `previous`, or `YYYY-MM` and applies only to summaries and project breakdowns. Report the returned billing-period dates with cost comparisons. `projects --project` returns a service breakdown and cannot be combined with `--limit`. Human output defaults to the top 25 projects; JSON returns all unless limited. Deleted resources can still contribute to the period's costs.

Distinguish current usage from an estimated end-of-period bill. Estimates can be unavailable and are not final invoices. Use the returned cost fields rather than recomputing them from hardcoded prices; do not treat missing values as zero.

## Inspect and change limits

```bash
railway usage limit status --workspace <workspace> --target workspace --json
railway usage limit status --workspace <workspace> --target agent --json
railway usage limit set --workspace <workspace> --target workspace --soft 75 --hard 125 --json
railway usage limit set --workspace <workspace> --target agent --soft 7.50 --hard 20 --json
```

Read the target's existing limits before changing them. `set` requires `--target`; `status` without a target reports both. `workspace` controls resource/compute usage; `agent` controls Railway Agent usage. Do not infer that the agent target caps cloud VM compute costs or another coding harness's provider bill.

Soft limits are email alerts; hard limits enforce a spending cap and can interrupt the corresponding service. Set only the target and amounts the user requested. Inputs are dollars: workspace limits require whole dollars; agent limits accept cents. Omitted fields preserve existing values, but setting an agent soft limit without any existing hard limit requires supplying a hard limit too. Follow CLI validation for supported ranges.

```bash
railway usage limit update --workspace <workspace> --soft 75 --json
railway usage limit remove --workspace <workspace> --yes --json
```

`update` and `remove` are workspace compute-limit operations, not generic agent-target operations. Removing limits removes the spending protection; `--yes` skips confirmation and belongs only on an authorized removal. `--period` is invalid on all limit commands. After any mutation, reread `limit status` for the same workspace and target and report the resulting limits.

## Troubleshoot

- **Wrong totals**: check the workspace, returned billing-period boundaries, selected project, and whether deleted resources contributed.
- **Missing estimate**: report actual usage and the unavailable estimate separately.
- **Limit command rejected**: check target, dollar precision, required hard limit, and CLI version; do not retry with a different spending target.
- **Permission denied**: verify billing access to the workspace; project access alone does not establish permission to change spending limits.

## Validated against

- Docs: [Usage CLI](https://docs.railway.com/cli/usage)
- CLI source (v5.49.1): [usage.rs](https://github.com/railwayapp/cli/blob/v5.49.1/src/commands/usage.rs)
