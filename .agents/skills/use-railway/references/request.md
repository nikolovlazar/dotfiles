# Request

Official documentation and community endpoints. GraphQL operations without a dedicated CLI command or MCP tool.

## Official documentation

Primary sources for authoritative Railway information:

- **Full LLM docs**: `https://docs.railway.com/api/llms-docs.md`
- **LLM summary**: `https://railway.com/llms.txt`
- **Templates**: `https://railway.com/llms-templates.md`
- **Changelog**: `https://railway.com/llms-changelog.md`
- **Blog**: `https://blog.railway.com/llms-blog.md`
- **Direct doc pages**: `https://docs.railway.com/<path>` (for example, `cli/up`, `networking/domains`, `observability/logs`)

Tip: append `.md` to any `docs.railway.com` page URL to get a markdown version suitable for LLM consumption.

Common doc paths:

| Topic | Path |
|---|---|
| Agent setup | `agents`, `ai/agent-skills`, `ai/mcp-server` |
| CLI reference | `cli`, `cli/<command>` |
| Projects | `projects` |
| Deployments | `deployments` |
| Volumes | `volumes` |
| Variables | `variables` |
| Infrastructure as Code | `infrastructure-as-code`, `infrastructure-as-code/reference` |
| Public networking | `networking/public-networking` |
| Private networking | `networking/private-networking` |
| Domains, CDN, WAF | `networking/domains`, `networking/cdn`, `networking/waf` |
| Outbound networking | `networking/outbound-networking`, `networking/static-outbound-ips` |

Fetch official docs first for product behavior questions. Use Central Station only when you need community evidence, prior incidents, or implementation anecdotes.


## Central Station (community)

Search and browse Railway's community platform for prior discussions, issue patterns, and field solutions.

### Recent threads

```bash
curl -s 'https://station-server.railway.com/gql' \
  -H 'content-type: application/json' \
  -d '{"query":"{ threads(first: 10, sort: recent_activity) { edges { node { slug subject status upvoteCount createdAt topic { slug displayName } } } } }"}'
```

Filter by topic with the `topic` parameter (`"questions"`, `"feedback"`, `"community"`, `"billing"`):

```bash
curl -s 'https://station-server.railway.com/gql' \
  -H 'content-type: application/json' \
  -d '{"query":"{ threads(first: 10, sort: recent_activity, topic: \"questions\") { edges { node { slug subject status topic { displayName } upvoteCount } } } }"}'
```

Sort options: `recent_activity` (default), `newest`, `highest_votes`.

### Search threads

```bash
curl -s 'https://station-server.railway.com/gql' \
  -H 'content-type: application/json' \
  -d '{"query":"{ threads(first: 10, search: \"<search-term>\") { edges { node { slug subject status } } } }"}'
```

### LLM data export

Bulk search alternative, fetches all public threads with full content:

```bash
curl -s 'https://station-server.railway.com/api/llms-station'
```

### Read a full thread

```bash
curl -s 'https://station-server.railway.com/api/threads/<slug>?format=md'
```

Thread URLs follow the format: `https://station.railway.com/{topic_slug}/{thread_slug}`

Community threads are anecdotal. Always pair with official docs when the answer informs an operational decision.


## GraphQL with the CLI

Use `railway api` (CLI 5.28+) for API operations that dedicated commands and MCP tools cannot express. It uses the CLI's configured authentication and supports normal token refresh. Inspect the live schema before guessing fields or input shapes:

```bash
railway api search projectUpdate --kind mutation
railway api describe ProjectUpdateInput
railway api describe Mutation.projectUpdate
railway api schema --compact
railway api '<query>' --variables '{"id":"<resource-id>"}'
railway api --file query.graphql --variables @variables.json
railway api --file query.graphql --raw-var id=<resource-id> --var enabled=true
```

`--var` parses JSON values when possible; `--raw-var` keeps strings. Queries can also come from stdin, with variables provided separately. Use `--operation-name` for documents containing multiple operations. Output is JSON by default; `--compact` changes formatting and there is no `--json` flag. HTTP errors and GraphQL `errors` fail the command; do not add `--allow-errors` when deciding whether a mutation succeeded. Query resource state before retrying an uncertain mutation.

For an older CLI that cannot be upgraded, the bundled `scripts/railway-api.sh '<query>' '<variables-json>'` remains a compatibility fallback. The database analysis scripts (`dal.py`, `analyze-postgres.py`) still call this helper directly, so it must stay in the plugin even when agents use `railway api`. It reads `user.token` from `~/.railway/config.json`; it does not implement the CLI's environment-token selection or OAuth refresh. It expects query first and variables second, not a query on stdin, and callers must inspect its response for GraphQL errors. Keep the skill telemetry prefix on `railway api` calls just like other CLI calls.

For the full API schema, see: https://docs.railway.com/api/llms-docs.md

## Project mutations

There is no dedicated project command for these setting updates (rename, PR deploys, visibility). Use GraphQL:

```bash
railway api \
  'mutation updateProject($id: String!, $input: ProjectUpdateInput!) {
    projectUpdate(id: $id, input: $input) { id name isPublic prDeploys botPrEnvironments }
  }' \
  --variables '{"id":"<project-id>","input":{"name":"new-name","prDeploys":true}}'
```

Common `ProjectUpdateInput` fields: `name`, `isPublic`, `prDeploys`, `botPrEnvironments`.


## Service mutations

Use `railway add` to create services and GraphQL to rename them or change icons:

```bash
railway api \
  'mutation updateService($id: String!, $input: ServiceUpdateInput!) {
    serviceUpdate(id: $id, input: $input) { id name icon }
  }' \
  --variables '{"id":"<service-id>","input":{"name":"new-name"}}'
```

`ServiceUpdateInput` fields: `name`, `icon` (image URL, animated GIF, or devicons URL like `https://devicons.railway.app/postgres`).

Get the service ID from `railway service list --json`.


## Service creation via GraphQL

Prefer `railway add` for most cases. Use GraphQL for programmatic or advanced use:

```bash
railway api \
  'mutation createService($input: ServiceCreateInput!) {
    serviceCreate(input: $input) { id name }
  }' \
  --variables '{"input":{"projectId":"<project-id>","name":"my-service","source":{"image":"nginx:latest"}}}'
```

`ServiceCreateInput` fields:

| Field | Type | Description |
|---|---|---|
| `projectId` | String! | Target project (required) |
| `name` | String | Service name (auto-generated if omitted) |
| `source.image` | String | Docker image (for example, `nginx:latest`) |
| `source.repo` | String | GitHub repo (for example, `user/repo`) |
| `branch` | String | Git branch for repo source |
| `environmentId` | String | Create only in a specific environment |

After creating a service via GraphQL, configure it with a JSON config patch including `isCreated: true` (see [configure.md](configure.md)).


## Metrics queries

Use `railway metrics` for routine metric checks. Use GraphQL only when you need custom measurements, grouping, sample rates, or averaging windows that the CLI doesn't expose.

```bash
railway api \
  'query metrics($environmentId: String!, $serviceId: String, $startDate: DateTime!, $endDate: DateTime, $sampleRateSeconds: Int, $averagingWindowSeconds: Int, $groupBy: [MetricTag!], $measurements: [MetricMeasurement!]!) {
    metrics(environmentId: $environmentId, serviceId: $serviceId, startDate: $startDate, endDate: $endDate, sampleRateSeconds: $sampleRateSeconds, averagingWindowSeconds: $averagingWindowSeconds, groupBy: $groupBy, measurements: $measurements) {
      measurement tags { serviceId deploymentId region } values { ts value }
    }
  }' \
  --variables '{"environmentId":"<env-id>","serviceId":"<service-id>","startDate":"2026-02-19T00:00:00Z","measurements":["CPU_USAGE","MEMORY_USAGE_GB"]}'
```

Available `MetricMeasurement` values: `CPU_USAGE`, `CPU_LIMIT`, `MEMORY_USAGE_GB`, `MEMORY_LIMIT_GB`, `NETWORK_RX_GB`, `NETWORK_TX_GB`, `DISK_USAGE_GB`, `EPHEMERAL_DISK_USAGE_GB`, `BACKUP_USAGE_GB`.

Optional parameters: `endDate` (defaults to now), `sampleRateSeconds`, `averagingWindowSeconds`. Use `groupBy: ["SERVICE_ID"]` without `serviceId` to query all services in an environment at once. Valid `MetricTag` values for `groupBy`: `SERVICE_ID`, `DEPLOYMENT_ID`, `DEPLOYMENT_INSTANCE_ID`, `REGION`.

Get the environment ID from `railway status --json`. Get service IDs from `railway service list --json`.

## Template search

Use the CLI for template search:

```bash
railway templates search redis --verified true --json
railway templates search --category Storage --limit 10 --json
railway templates search --after <cursor> --json
railway templates list --json
railway templates create --project <project> --environment production --json
```

The CLI search command doesn't require authentication and supports pagination with `pageInfo.endCursor`. Prefer CLI template commands for search, listing owned templates, creating drafts, publishing, unpublishing, and deleting. Use GraphQL only for template workflows the CLI cannot express.

Use GraphQL only when the CLI output isn't enough for the workflow:

```bash
railway api \
  'query templates($query: String!, $verified: Boolean, $recommended: Boolean) {
    templates(query: $query, verified: $verified, recommended: $recommended) {
      edges { node { code name description category } }
    }
  }' \
  --variables '{"query":"redis","verified":true}'
```

| Parameter | Type | Description |
|---|---|---|
| `query` | String | Search term |
| `verified` | Boolean | Only verified templates |
| `recommended` | Boolean | Only recommended templates |
| `first` | Int | Number of results |

Common template codes: `ghost`, `strapi`, `minio`, `n8n`, `uptime-kuma`, `umami`, `postgres`, `redis`, `mysql`, `mongodb`.

Deploy a found template via CLI:

```bash
railway deploy --template <template-code>
```

Manage owned templates via CLI:

```bash
railway templates publish <template-id> --category Other --description "Deploy and Host My App with Railway" --readme-file README.md --json
railway templates update <template-id> --category Other --description "Updated description" --readme-file README.md --json
railway templates unpublish <template-id-or-code> --yes --json
railway templates delete <template-id-or-code> --yes --json
```

### GraphQL template deployment

For deploying into a specific environment or tracking the workflow, use the two-step GraphQL flow:

**Step 1**: Fetch the template config:

```bash
railway api \
  'query template($code: String!) {
    template(code: $code) { id serializedConfig }
  }' \
  --variables '{"code":"postgres"}'
```

**Step 2**: Deploy with `templateDeployV2`:

```bash
railway api \
  'mutation deploy($input: TemplateDeployV2Input!) {
    templateDeployV2(input: $input) { projectId workflowId }
  }' \
  --variables '{"input":{
    "templateId":"<id-from-step-1>",
    "serializedConfig":<config-object-from-step-1>,
    "projectId":"<project-id>",
    "environmentId":"<env-id>",
    "workspaceId":"<workspace-id>"
  }}'
```

`serializedConfig` is the raw JSON object from the template query, not a string. Get `workspaceId` via `railway api 'query { project(id: "<project-id>") { workspaceId } }'`.


## Validated against

- Docs: [api docs](https://docs.railway.com/api/llms-docs.md), [agents.md](https://docs.railway.com/agents), [community.md](https://docs.railway.com/community), [cli/docs.md](https://docs.railway.com/cli/docs), [templates.md](https://docs.railway.com/cli/templates), [metrics.md](https://docs.railway.com/cli/metrics)
- CLI source: [api.rs (v5.49.1)](https://github.com/railwayapp/cli/blob/v5.49.1/src/commands/api.rs), [client.rs (v5.49.1)](https://github.com/railwayapp/cli/blob/v5.49.1/src/client.rs), [docs.rs](https://github.com/railwayapp/cli/blob/v5.23.3/src/commands/docs.rs), [templates.rs](https://github.com/railwayapp/cli/blob/v5.23.3/src/commands/templates.rs), [metrics.rs](https://github.com/railwayapp/cli/blob/v5.23.3/src/commands/metrics.rs)
