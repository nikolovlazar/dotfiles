---
name: manage-todos
description: Create, list, and update repository TODOs in Markdown. Use when the user asks to add/write TODOs, list current TODOs, track progress or feature work, mark TODOs done, blocked, or cancelled, or prompt for blocked/cancelled reasons before changing status.
---

# Manage TODOs

## Overview

Use the repository's existing TODO conventions first. If no convention exists, prefer a root `TODO.md` with stable IDs and simple Markdown sections.

## Canonical Format

Use this default format unless the repository already defines another one:

```md
- [ ] TODO-0001: Short imperative title
  - Status: todo
  - Area: docs
  - Goal: One sentence describing the outcome.
  - Done when: Concrete acceptance criteria.
```

Allowed statuses:

- `todo`: Not started or not currently active.
- `in-progress`: Started but incomplete.
- `blocked`: Cannot proceed until something changes.
- `done`: Completed and verified.
- `cancelled`: Intentionally abandoned.

When marking a TODO `done`, set the checkbox to `[x]`. For every other status, keep `[ ]`.

## Write TODOs

1. Inspect existing TODO files before adding anything. Prefer root `TODO.md`; also check `docs/todos/` if it exists.
2. Reuse the next numeric ID in the existing sequence. If no sequence exists, start at `TODO-0001`.
3. Write the title as a short action, not a vague theme.
4. Include `Area`, `Goal`, and `Done when` unless the existing file uses a leaner format.
5. Keep TODOs small enough to complete in one focused change. Split large features into a parent TODO plus concrete child TODOs only when useful.
6. Avoid hiding important follow-up work only in chat; add it to the TODO file when the user wants persistent tracking.

## List TODOs

When listing TODOs:

1. Read the TODO source directly.
2. Group by status or by the file's sections, whichever is clearer.
3. Include each TODO ID, title, status, and any blocker/cancel reason.
4. Keep the response concise. Do not paste the full file unless the user asks.

## Change Status

When changing a TODO status:

1. Identify the TODO by ID. If the user gives only a title, match conservatively and ask if ambiguous.
2. Update the checkbox and `Status` field together.
3. Move the TODO between sections only if the file already uses status sections.
4. For `done`, add `Completed: YYYY-MM-DD` if the file already tracks dates.
5. For `blocked`, add or update `Blocked reason: ...`.
6. For `cancelled`, add or update `Cancelled reason: ...`.

If the user asks to mark a TODO `blocked` or `cancelled` without a reason, ask for the reason before editing unless they explicitly say to skip it. A short prompt is enough:

```text
What should I record as the blocked reason for TODO-0003?
```

Do not invent blocker or cancellation reasons. If the reason is obvious from the user's message, use it and do not ask.

## Handoff

After editing TODOs, summarize:

- TODOs added or changed.
- Any status reasons recorded.
- Any verification performed, such as Markdown link checks or file review.
