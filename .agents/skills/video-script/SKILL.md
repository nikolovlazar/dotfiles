---
name: video-script
description: "Writes ready-to-film AV scripts with two-column VIDEO|AUDIO beats, hooks, segment breakdowns, B-roll cues, text overlay notes, timing estimates, shot lists, and YouTube publishing metadata (three title variants, description, and tags). Use when a user needs a script for YouTube, TikTok, Instagram Reels, course content, or any video format where a structured script improves production quality."
---

# Video Script

## When to Use This Skill

Use this skill when you need to:
- Write a ready-to-film AV script for YouTube, TikTok, Instagram Reels, or course content
- Structure a talking head, tutorial, or short-form video with proper segments and timing
- Produce a two-column VIDEO|AUDIO script where every beat makes the visual state and audio state explicit
- Turn a topic, outline, or rough notes into a complete script with hook, body, and CTA
- Generate a shot list so the creator knows every setup before they press record

**DO NOT** use this skill for podcast scripts (audio-only, no visual cues needed), written blog posts, or live-stream outlines where scripting kills spontaneity.

---

## Core Principle

EVERY SECOND OF VIDEO MUST EARN THE NEXT SECOND — IF THE VIEWER HAS NO REASON TO KEEP WATCHING, THEY LEAVE.

EVERY ROW OF THE SCRIPT MUST MAKE THE AUDIO STATE EXPLICIT — IF THE CREATOR OR EDITOR HAS TO GUESS WHAT THE AUDIENCE HEARS, THE SCRIPT HAS FAILED.

---

## Video Format Quick Reference

The pacing intervals below are production heuristics, not scientifically established retention rules. Change the visual when it helps the explanation; preserve a readable hold when technical evidence needs time.

| Format | Target Length | Pacing | Initial Relevance | Segments |
|---|---|---|---|---|
| YouTube long-form | 8-15 min | Visual change every 5-8 sec | First 3 sec | 4-6 + intro/outro |
| Short-form (Reel/TikTok/Short) | 30-60 sec | Visual change every 2-3 sec | First 1-2 sec | Hook + 1-3 points + CTA |
| Tutorial | 5-10 min | Match pacing to steps | First 5 sec | Setup + 3-5 steps + recap |
| Talking head | 3-5 min | Cut every 10-15 sec | First 3 sec | 2-3 segments + CTA |

---

## Creator Voice: Lazar

When writing or revising Lazar's own videos, read [Lazar's voice and presentation preferences](references/lazar-voice.md) before drafting. It contains direct corrections and examples from his previous scripts. Apply this profile alongside the AV, teleprompter, and editorial rules below.

- Write friendly, casual, direct narration addressed to the viewer. For Sentry and development topics, assume a developer audience unless the brief says otherwise. Use **150 WPM** for estimates unless this video's delivery is measured or Lazar specifies another pace.
- Build a connected explanation of what the viewer is trying to learn, what the evidence shows, and why the next step follows. Voice includes cadence, vocabulary, transitions, and the angle of the story; matching **you/I** pronouns alone is insufficient.
- Establish the presentation mode from the request and this video's context: a screen walkthrough, a completed investigation, or talking head with prepared overlays. Match navigation language and tense to that mode. Provide concise speaking cues instead of word-for-word narration when Lazar requests an outline for an improvised demo.
- Prioritize current explicit feedback, then previous user corrections and scripts he actually used or recorded. A generated draft is not an approved voice sample just because it is the newest version. Generic examples later in this skill illustrate format, not Lazar's voice.
- Before delivering, read the hook and connected narration aloud. Check that the transitions sound like something he would say to the viewer, that the technical terms are clear, and that the claims match the demonstrated evidence. Use the reference's calibration checklist.

---

## Default Opening Format for Lazar

Use this sequence for every new or revised Lazar video unless he explicitly requests a different format. Do not ask him to reconfirm it each time:

1. **Hook — 20–30 seconds.** Earn attention in the first few seconds, then build a reason to continue through a recognizable situation, a meaningful question or gap, and a concrete payoff. A common problem the intended audience recognizes is the preferred starting point when it fits the topic; choose another architecture when it is stronger. At 150 WPM this is roughly 50–75 spoken words before allowing for deliberate pauses.
2. **Title-card bumper — 2–3 seconds.** Use his existing premade MOGRT and change only its labels. State the exact title text in VIDEO. It is a breather after the hook, with **no narration**. Specify silence or the asset's known audio sting in AUDIO; do not invent a new animation, filename, or soundtrack. Default to three seconds of silence when no asset audio is specified.
3. **Body.** Immediately begin fulfilling the hook's promise; do not greet the viewer or repeat the introduction. Choose the first body beat from the active subject and supply any prerequisite context before its demonstration or explanation.

These durations and the bumper are Lazar's production preference, not a universal retention finding. The first-seconds guidance in the format tables describes **initial relevance**, not the duration of the whole hook. If a later brief explicitly requests a shorter opening, follow it. If a short total runtime cannot fit the established format, surface that concrete tradeoff rather than silently dropping the bumper or compressing the hook.

Read [Video hooks: evidence, architectures, examples, and selection](references/video-hooks.md) whenever creating or revising an opening. Apply the selection and validation process in every AV script, even when the user does not separately ask for a hook.

---

## Phase 1: Brief

Gather these details from the request and existing video context. Ask only for missing information that materially changes the script; do not repeat questions the user has already answered.

1. **Topic** — the specific angle, not just the broad subject
2. **Format** — YouTube long-form, short-form reel, tutorial, or talking head (default: YouTube long-form)
3. **Target length** — use quick reference defaults if not specified
4. **Key points** — 3-5 things the video must cover, in priority order
5. **CTA** — what should the viewer do after watching? (subscribe, visit link, comment, try something)
6. **Tone** — default: conversational and direct, like explaining to a smart friend; for Lazar, apply the creator voice profile above
7. **Audience** — name a concrete persona: role, relevant experience or stack, responsibility, and the problem or goal that motivates this viewer. For development content, infer a specific developer persona from the subject; “developers investigating performance” is too broad. For other subjects, infer the relevant audience instead of defaulting to a fixed profession. Ask only when audience differences materially change the script.
8. **Opening** — use Lazar's established hook → MOGRT bumper → body format. Record the selected hook architecture and exact title-card text; infer them when the angle is clear, or offer concrete choices as described below.

Present the brief back to the user:

```
## Script Brief

**Topic:** 5 tools every solopreneur needs to save 10+ hours per week
**Format:** YouTube long-form (8-10 minutes)
**Key points:**
1. Project management (Notion)
2. Scheduling (Cal.com)
3. Email automation (Kit)
4. Design (Canva)
5. AI writing assistant (Claude)
**CTA:** Subscribe + link to free tool comparison PDF in description
**Tone:** Conversational, enthusiastic but not hype-y
**Audience:** Solo founders and freelancers in their first 2 years
```

**GATE: Resolve any material uncertainty in the brief before structuring. When the user has already supplied or authorized the brief, proceed without requesting redundant confirmation.**

---

## Phase 2: Structure

Build the script skeleton before writing full dialogue. This prevents rambling and keeps every segment purposeful.

### Hook Architecture

Before writing full narration, read [the researched hook playbook](references/video-hooks.md). It documents the evidence available as of September 2026, its limits, general architectures, adaptable opener templates, and illustrations across different subjects. It has no active video; derive all subject details from the current brief. The types below are practical editorial choices, not a scientific ranking of guaranteed winners.

| Hook type | Suitable starting point |
|---|---|
| Recognized problem → concrete payoff | A real frustration or diagnostic blind spot the audience can recognize; preferred default for Lazar when relevant |
| Result/proof first → how | A verified result or visually legible finding that raises a useful question |
| Specific question → investigation or explanation | One answerable question that the demonstration can resolve |
| Assumption correction → demonstration | A real misconception with a fair, supported correction |
| Mini story → unresolved next step | An actual incident, or a clearly hypothetical scenario, with a relevant turning point |
| Contrast → useful distinction | Two interpretations, pieces of evidence, or workflows worth comparing |
| Bounded prediction/challenge → reveal | An understandable puzzle whose answer the viewer can predict and the video can show |
| Direct useful promise → route | A clear search-led task or a useful capability the viewer wants to reproduce |

#### Design the opening sequence

- Identify the audience's situation, the question or outcome they care about, and the actual payoff. Check any supplied title/thumbnail promise. Establish broad relevance before narrowing to the demo's details.
- Privately compare at least three **different architectures**, using the playbook's suitability and failure criteria. Do not offer three paraphrases of the same sentence. A common problem is often a good angle, but a routine task is not automatically difficult: do not invent pain, inflate stakes, or manufacture an incident.
- Choose a delivery emotion appropriate to the premise: for example, thoughtful concern, curiosity, surprise, relief, or calm confidence. Give a brief progression or phrase cue that helps the creator perform it; keep it consistent with the voice and evidence. Emotion is delivery direction, not permission to exaggerate the story.
- Develop the chosen opener into a complete 20–30-second hook for Lazar: early relevance, a specific reason to continue, enough concrete detail or proof to earn trust, and a clear bridge across the bumper. Do not pad a weak opening to reach the time target or reveal every answer before the demonstration.
- Keep the promise inside the feature's actual behavior and the example's evidence. Keep observations, interpretations, and demonstrated outcomes distinct; a useful finding is not automatically a completed fix or a proven cause. Label hypothetical scenarios and proposed fixes honestly.

#### Choose, or ask with examples

**Choose and proceed** when the brief and evidence make one architecture clearly suitable. State the type, one brief reason, and delivery emotion with useful phrase cues alongside the draft; the user can revise it. Do not add an approval stop when drafting is already authorized.

**Offer a choice** when the user asks to explore hooks, when materially different angles are similarly strong, or when audience motivation is unclear. Provide the recommended type first and a **topic-specific opener for every type you are offering**, plus its promised payoff or tradeoff. Never ask the user to choose between abstract labels alone. When they ask to compare all types, give an opener for each of the eight types; otherwise shortlist the genuinely suitable candidates. Let them choose before committing dependent narration, and continue independent research or body preparation while waiting.

#### Validate hook → bumper → first body beat

- Read the entire sequence aloud at the creator's pace. Check recognition, specificity, truthful payoff, natural vocabulary, point of view, tense, and the reason to keep watching across the title card.
- Pair the hook with a specific visual purpose while leaving treatment to the editor. Introduce enough context for the viewer to understand the promised demonstration or explanation.
- Make the 2–3-second MOGRT bumper a separate AV beat with exact labels and an explicit audio state. Keep title-card silence out of teleprompter pause cues.
- Begin the body with action or evidence that fulfills the promise. Avoid a second greeting, agenda, or recap of the hook. Use the opening step appropriate to the subject, without importing a previous video's workflow.
- Never open with a greeting or an agenda such as “Hey guys, welcome to my channel” or “In this video, I'm going to…”. A useful direct promise is specific, not a generic feature inventory.
- If the hook is rejected, reconsider the audience motivation and architecture before swapping adjectives. After publication, use the playbook's retention review to refine later hooks; do not invent performance guarantees or channel benchmarks.

### Outline Template

For Lazar, fill this topic-independent structure from the active brief. Timings illustrate a 25-second hook and three-second bumper; calculate the actual narration and holds.

```
## Script Outline

**HOOK** (0:00–0:25)
- Architecture: [selected type and reason]
- Viewer motivation: [recognizable situation, question, goal, or result]
- Payoff: [specific outcome the body can deliver]

**TITLE CARD** (0:25–0:28)
- Existing premade MOGRT; title text: [labels for the active subject]
- No narration; three seconds of silence unless an existing sting is specified

**BODY: FIRST PROMISED STEP** (from 0:28)
- [necessary setup, example, baseline, or first action]

**BODY: DEVELOP THE CONTENT**
- [demonstrate, explain, compare, or continue the story]
- [show supporting evidence and clarify relevant limits]

**PAYOFF + CTA**
- Deliver the hook's promise and give one useful next action
```

For another creator, follow that creator's brief and opening structure. Replace the semantic slots with the active subject; do not treat a prior script as the default content.

**GATE: Resolve material outline or hook choices before full narration. A request to rewrite an existing script with a clear angle authorizes drafting; do not create redundant approval gates.**

---

## Phase 3: Write

Write the complete script as an **AV script** — a two-column table per segment where every row is a single beat. The VIDEO column describes what is on screen; the AUDIO column says exactly what is heard. Silence, SFX, and continuing narration must be marked explicitly — never leave the audio state ambiguous.

### AV Script Table Format

Each segment is its own markdown table. Every row is one beat (one discrete unit where the visual state and the audio state are both stable for a brief moment). Start each table fresh:

```
## SEGMENT X: TITLE (MM:SS–MM:SS)

| VIDEO | AUDIO |
|---|---|
| What is on screen for this beat. | What the audience hears for this beat. |
```

Put the runtime range in the segment header so the creator and editor can scan pacing at a glance.

Keep every generated table compact: use exactly one space between a cell's content and its surrounding pipes, and never pad cells to align columns. In Markdown, use only three dashes per column (`|---|---|`); in Org, use `|---+---|`. Keep each row on one source line; let the editor preview wrap long cells. Apply this to AV, shot-list, overlay, captions, evidence, and other supporting tables. In Org tables, insert a short horizontal rule between body rows when they need visual separation.

Match markup to the output format: Markdown uses `**bold**`, `*italic*`, and backticks; Org uses `*bold*`, `/italic/`, `=code=`, and `[[URL][label]]` links. Write silence and hold directions as plain text inside table cells so their formatting markers do not clutter the editable source. Do not emit HTML `<br>` tags or Markdown escapes such as `\~` in Org output. Preserve the actual words, durations, and audio-state labels.

### AUDIO Cell Rules — Read This Twice

Every AUDIO cell MUST be one of these three things (or a combination of them):

1. **A narrator line.** The exact words spoken, formatted as `**NARRATOR:** "..."`. Use `**HOST:**`, `**GUEST:**`, `**VO:**`, etc. when there is more than one speaker.
2. **Explicit silence with duration, in plain text.** `Silent beat, ~1 second.` or `Hold ~2 seconds, silent, to let the line land.`. Always give the duration. Optionally state the rhetorical purpose ("to let the caption land," "for effect," "pause for laugh").
3. **SFX or music.** `**SFX:** outro stinger.` or `Soft sting to land the title.`. Can be combined with a narrator line or with silence in the same cell.

DO NOT write vague stage-direction notes in the AUDIO column. Forbidden phrases include:

- ❌ "Narration continues over B-roll — no new line" → is there narration or not? Which line? State it, or fold this beat into the row whose narration it belongs to.
- ❌ "Brief hold, no new line" → is there silence? music? for how long? Make it explicit: `Silent hold, ~1 second.`
- ❌ "Same audio as above" → restate it or merge the rows.
- ❌ "Continues" / "Ongoing" / "No change" → these are notes, not audio descriptions.

Rule of thumb: if the cleanest way to describe the audio is "it is continuing from the previous row," this beat does not deserve its own row. Fold the visual into the row whose narration it syncs with (see next section).

### Scene Changes Mid-Sentence

When a visual changes in the middle of a narrator line, describe the scene change **in the VIDEO cell of the same row as the narration** — do NOT split a single narrator line across two rows just because the visual changes.

Use sync phrases like "As the narrator says X," "On the word Y," or "Synced to the phrase Z" so the editor knows when to trigger the visual.

✅ Right — visual change described in VIDEO cell, narration stays in one row:

```
| Cut back to talking head, medium shot. As the narrator says "one place," text overlay appears: *"One place for your references."* | **NARRATOR:** "I keep the references in one place, so I can find them when I need them. Let me show you how I organize the folder." |
```

❌ Wrong — narrator line split across two rows, second row's audio is a stage-direction note:

```
| Cut back to talking head, medium shot. | **NARRATOR:** "I keep the references in one place, so I can find them when I need them. Let me show you how I organize the folder." |
| Text overlay appears: *"One place for your references."* | *Narration continues, no new line.* |
```

If multiple visual events happen during a single narration line (cut + caption + B-roll), chain them in the VIDEO cell in order, each with its own sync phrase.

### Editorial Direction and Terminology

Apply these principles to every script and revision, including VIDEO cells, shot lists, asset notes, and editor handoffs:

- **Be specific about the story, flexible about the treatment.** Identify the source visual, what the viewer should notice, and the narration cue it supports. Leave framing, transitions, graphic design, animation, and visual emphasis to the editor unless the user has specified them or they are essential to understanding the scene.
- Prefer **“draw attention to”** when the goal is emphasis. Do not prescribe an arrow, zoom, dimming, underline, or highlight effect unless requested; these are possible treatments the editor can choose.
- Use familiar editorial terms accurately: **frame hold** for a frozen video frame, **cropped still** for a static image showing part of a frame, **graphic overlay** or **framed overlay** for graphics layered over footage, **picture-in-picture inset** for a smaller video within the main composition, and **lower-third** for a graphic in the lower part of the frame. Avoid ambiguous descriptions such as “over the camera” or “partial stills.”
- Phrase optional treatments as suggestions (“could,” “consider,” “one option”), while keeping asset references, story continuity, narration, and explicit user requirements clear. Creative freedom should not make the audio state or intended subject ambiguous.
- When footage is supplied, reference verified filenames and distinguish existing footage from assets still to be produced. Use the recorded delivery and phrase cues to guide the edit; do not present earlier estimated timestamps as measured cut points.
- Review all editor-facing notes for consistent terminology and remove unnecessary prescriptions. The examples below illustrate possible treatments, not mandatory effects or camera moves.

Example: “On ‘European backend,’ draw attention to the bolded `eu-west` row during the existing frame hold. The visual treatment is up to you.”

### VIDEO Cell Rules

- State the shot type: talking head, screen recording, animated card, B-roll overlay, title card, end screen.
- State required framing changes (wide, medium, tight); otherwise offer framing as a suggestion or leave it to the editor. When the shot holds, write "Same shot."
- When a text overlay or caption is burned in during this beat, include the exact text in quotes on its own line within the cell.
- Keep each Markdown table cell on one source line. Separate visual directions with complete sentences and bold labels such as **Text overlay:**. Use separate rows for discrete thoughts or changes in audio state; keep a continuous narrator sentence in one row with visual sync cues. Do not rely on HTML line-break tags or literal newlines inside table cells.
- Be specific about screen recordings — name the app, the view, the interaction, and any text the viewer should notice.

### Tense and Point-of-View Consistency

Apply this check when generating, revising, or regenerating any script or teleprompter copy:

- Establish who is speaking and who each passage refers to. Use **you/your** for viewer guidance, **I/my** for the presenter’s own actions or examples, and **we/our** only when the shared group or activity is clear. Do not switch between them within the same explanation without a reason.
- Make a change in perspective explicit. For example, keep “You type your question… You click the suggestion…” throughout a viewer-facing explanation, then bridge into the presenter’s demonstration with “Let me show you an example from my app.” Returning to a viewer-facing CTA is a natural, deliberate shift.
- Choose the time frame for the demonstration and carry it through connected actions. Use present tense for a walkthrough (“I open… I select…”), and past tense for a completed investigation (“I opened… I selected…”). An introduction such as “Here’s how I did it” establishes a past-tense account; do not drift into “I’ll open…” midway through it.
- Preserve meaningful differences in time: general product capabilities can remain present tense, and proposed next steps should remain future or conditional. Never turn a proposed action into a claim that it happened just to make the grammar uniform.
- Read across sentence and segment boundaries after each edit. Check pronoun referents, tense, and transitions, then synchronize the AV narration and teleprompter wording while preserving valid pause cues. Consistency means a coherent viewpoint and timeline, not forcing every sentence into one pronoun or tense.

### Script Writing Rules

- **Write exactly what the narrator will say.** No summaries, no "talk about X here," no paraphrases.
- **One idea per segment.** If a segment covers two ideas, split it.
- **One beat per row.** A beat ends when the shot changes, the narrator starts a new discrete thought, or the audio state changes (narration → silence, silence → SFX, etc.).
- **Use purposeful visual progression.** Treat changes every 5-8 seconds for long-form or every 2-3 seconds for short-form as optional pacing heuristics, not universal retention rules. Change the shot, evidence, or overlay when it adds information; let technical evidence remain readable. Review rows and phrase cues for rhythm.
- **Segment headers** use the form `## SEGMENT X: TITLE (MM:SS–MM:SS)`.
- **Every AUDIO cell** starts with `**NARRATOR:**` (or speaker label), or is a plain-text direction describing silence/SFX, or is a `**SFX:**` line. There is no fourth option.

### Opening AV Example: Hook → Bumper → Body

This is a topic-independent AV template, not finished dialogue. Replace every bracketed semantic slot using the active brief, then calculate timing from the completed narration. A hook may use any suitable architecture from the playbook.

```
## HOOK (00:00–00:25)

| VIDEO | AUDIO |
|---|---|
| [Visual that establishes the relevant situation, result, or question; identify its purpose.] | **NARRATOR:** "[Complete opening narration: establish relevance, develop the chosen architecture, and promise the outcome the body can deliver.]" |

## TITLE CARD (00:25–00:28)

| VIDEO | AUDIO |
|---|---|
| Existing premade MOGRT. Set title text to "[title labels for the active subject]". | No narration, ~3 seconds; silence. |

## BODY: FIRST PROMISED STEP (from 00:28)

| VIDEO | AUDIO |
|---|---|
| [First relevant setup, action, example, or evidence from the active brief.] | **NARRATOR:** "[Begin delivering the promised content without repeating the introduction.]" |
```

For additional AV row patterns or an explicitly different creator/short-form brief, consult [the retained formatting examples](references/av-format-examples.md). Their shorter openings do not override Lazar's default format.

### Teleprompter Copy and Pause Cues

Preserve the document title, existing frontmatter, compact header metadata, and Flow at the top. After that header, place `## Teleprompter Copy`, followed by `## AV Script`. Put recording notes, publishing metadata, and evidence after the AV script. Reordering these two script sections must not remove or relocate the header below them. Apply this order to new scripts and revisions unless the user requests otherwise.

Whenever generating or regenerating a teleprompter copy, **always include intentional `[PAUSE]` cues**. Treat this as part of writing the copy, not an optional finishing step.

- Place cues within a paragraph where an extra beat helps the viewer anticipate a reveal or process a key finding, such as a hook, important number, contrast, or recap transition. Do not add `[PAUSE]` at a segment ending or immediately before or after a line or paragraph break: those boundaries already create natural pauses when reading from a teleprompter. Keep title-card holds and other boundary silence in the AV directions, not as teleprompter pause cues.
- Use the literal marker `[PAUSE]` at the intended break. It is a silent delivery instruction, not spoken dialogue or caption text. Keep duration guidance in edit notes: roughly half a second for a short beat, longer for a significant finding.
- Preserve valid internal cues when regenerating. Remove cues made redundant by line, paragraph, or segment breaks. Reassess placement when wording or visuals change, and retain any user-directed timing.
- Keep the spoken wording identical to the AV narration; removing pause markers from the teleprompter copy should leave the same dialogue. Represent corresponding pauses in the AV AUDIO cells as explicit silent beats with approximate durations, following the AUDIO cell rules.
- Exclude `[PAUSE]` and other delivery instructions from spoken word counts and captions. Include pause durations in the runtime estimate, counting each pause only once when it overlaps an existing silent beat or title card.

### Timing Validation

After writing, count spoken narrator words and validate against targets. NARRATOR lines are always wrapped in double quotes after the `**NARRATOR:**` label, so extract them specifically:

```bash
# Extract narrator dialogue and count words
grep -oE '\*\*NARRATOR:\*\* "[^"]+"' script.md | sed 's/\*\*NARRATOR:\*\* //' | tr -d '"' | wc -w
```

- **Speaking pace:** 130-160 words per minute (conversational, not rushed); **Lazar: 150 WPM** unless a different pace is specified or measured for this video
- **Lazar's hook:** 20–30 seconds including deliberate pauses, approximately 50–75 spoken words at 150 WPM before pauses. Calculate the full opening from actual words rather than treating the initial relevance window as its complete duration.
- **Lazar's title card:** add the 2–3-second MOGRT bumper once, separately from spoken time. The body begins immediately afterward.
- **YouTube 8-15 min:** 1,040-2,400 words of spoken dialogue
- **Short-form 30-60 sec:** 65-160 words
- **Tutorial 5-10 min:** 650-1,600 words
- **Talking head 3-5 min:** 390-800 words

> **Pace varies by creator.** 130-160 WPM is a baseline for natural, conversational delivery. High-energy creators (short-form, fast-cut YouTube) may speak at 170-190 WPM; calm tutorial presenters may sit at 110-130 WPM. If the creator's style is known, adjust word count targets accordingly. When unsure, ask: "Read one paragraph aloud and time yourself — how many seconds did it take?"

If word count is more than 15% over target, trim the weakest segment. If more than 15% under, add depth to existing segments — never add filler.

**GATE: Present the complete AV script. Do not finalize until the user approves content, tone, and length.**

---

## Phase 4: Deliver

Once approved, deliver the final package with these components.

### 1. Final Script File

Write the complete script package to a file if the user requests it. Default filename: `video-script.md`. Preserve the header and frontmatter at the top, then deliver the teleprompter copy followed by the AV script; each AV segment is a two-column table. For Lazar, use this layout:

```
# [Working video title]

**Runtime:** ~[MM:SS] at [WPM] WPM
**Spoken word count:** [count]
**Audience:** [specific persona: role, context, and relevant task]
**Tone:** [brief voice guidance]
**Hook architecture:** [selected type]. **Delivery emotion:** [emotion and a useful progression or phrase cue]
**Selling point:** [one sentence naming the actual benefit and evidence]

**Flow:**

1. [Hook]
2. [Title-card bumper]
3. [First body step]
4. [Remaining ordered steps through payoff and CTA]

## Teleprompter Copy

[Complete spoken narration with intentional internal pause cues]

---

## AV Script

[Timed AV segment tables]
```

Calculate timing and spoken words from narration and explicit holds. Use `~` for estimated runtime. Omit Draft, Format, and Opening format header fields. Keep format and production constraints in the brief or relevant AV beat; do not duplicate the standing format in metadata. Make Flow an ordered list. Choose a concrete persona rather than a description broad enough to fit all developers. Keep the selling point specific to the demonstrated outcome.

#### Recording notes and evidence

Keep recording notes **as short as possible**: normally zero to three short bullets, only for essential information that changes recording or navigation and is not already clear from the AV rows. Omit the section if nothing remains. Do not repeat durations, MOGRT instructions, narration, routine gear checks, or general caveats. Put scene-specific navigation in VIDEO. Preserve verified evidence and sources in their own section; technical claim substantiation belongs there, not in a long recording checklist. Apply the same brevity to production handoff notes under any heading; do not move omitted reminders into a second notes section. When shortening a revision, retain essential details such as an unusual occurrence selection that prevents recording the wrong event.

### 2. YouTube SEO / Publishing Package

For every complete YouTube script, include a `## YouTube SEO` section by default, unless the user explicitly excludes metadata. When revising a complete script, update the package if its angle, claims, content, or payoff changes. An isolated line edit need not regenerate an unrelated package. Read [YouTube SEO: research and practical guidance](references/youtube-seo.md) before drafting it. This is a global reference with no fixed subject or active video.

Use the actual script or finished cut, the concrete viewer persona, and the demonstrated payoff. Choose one or two candidate search phrases from that intent and supporting research. If channel search-term data or live keyword research is unavailable, do not invent search volumes, keyword difficulty, or popularity. Coordinate the titles with any supplied thumbnail and the hook's promise.

Deliver:

1. **Exactly three titles for A/B testing.** Make them materially different hypotheses while preserving the same truthful content promise. A useful default is searchable subject/product, viewer problem/outcome, and curiosity/story; choose other structures when the subject fits them better. Put the recommended default first with brief angle labels outside the title. Keep essential words early, channel branding later, and avoid unsupported results, numbers, superlatives, or uniqueness. Stay within the platform's title limit; no exact shorter length guarantees performance.
2. **One copy-ready description.** Lead with what this specific video teaches or demonstrates. Explain the actual content and payoff in natural language, then add a focused next step and useful supplied or verified links. Feature the core topic naturally; do not add keyword inventories or unrelated boilerplate. Optional relevant hashtags can go at the end; a small set is a readability choice, not a ranking formula. Keep research citations and editorial testing notes outside the publishable copy.
3. **Tags.** Provide one concise comma-separated line for YouTube's Tags field, using relevant subjects, tools, and genuine alternate spellings. Tags have a minor discovery role; do not fill the character budget or paste this list into the description.

The package should look like:

```
## YouTube SEO

### Three titles for A/B testing
1. [Recommended default — actual title]
2. [Actual title for a distinct angle]
3. [Actual title for a distinct angle]

### Description
[Complete publishable description with optional verified links and relevant hashtags]

### Tags
[Comma-separated relevant tags]
```

Do not invent chapter timestamps from AV planning estimates. Only include publishing chapters when the final cut or reliable timecoded evidence supports them. Provide chapter labels separately if useful. Add thumbnail copy or a testing plan only when requested or necessary to resolve a concrete packaging choice; do not inflate the default package with an SEO essay.

When explaining or setting up tests, use the current reference and verify actual eligibility: YouTube's native tool supports up to three title-only or paired title/thumbnail options, uses watch time rather than CTR alone, and may return no clear winner. A recommendation is an editorial choice, not a measured winning variant. For title-only comparison, keep the thumbnail fixed. For ineligible formats, offer editorial title alternatives without implying native A/B testing is available. Generating this package does not authorize uploading, publishing, or modifying an external channel.

### 3. Shot List Summary

Append a table listing every unique camera setup and visual asset:

```
## Shot List

| # | Shot Type | Description | Duration | Segment |
|---|---|---|---|---|
| 1 | Talking head | Selected 20–30-second hook; evidence or overlays only as needed | 20–30s | Hook |
| B | Existing animation | Premade MOGRT; exact title labels from the script | 2–3s | Title-card bumper |
| 2 | Wide talking head | Speaker at desk | 52s | Segment 1 |
| 3 | Screen recording | Notion dashboard — kanban, calendar | 30s | Segment 2 B-roll |
| 4 | Screen recording | Cal.com booking page | 15s | Segment 3 B-roll |
| 5 | Screen recording | Kit automation flow | 30s | Segment 4 B-roll |
| 6 | Screen recording | Canva template + export | 20s | Segment 5 B-roll |
| 7 | Screen recording | Claude outline workflow | 20s | Segment 6 B-roll |
| 8 | Wide talking head | Same framing as hook | 45s | Recap + CTA |
| 9 | End screen | Subscribe button + suggested video | 15s | Outro |

**Total setups:** 3 camera positions + 5 screen recordings + existing MOGRT title card + 1 end screen
**Estimated filming time:** 45-60 minutes (with retakes)
```

### 4. Text Overlay List

Extract every burned-in text overlay referenced in VIDEO cells into a standalone table with timestamps and placement notes for the editor.

### 5. Captions List

Extract every burned-in caption into a standalone table with timestamps. Note the caption strategy for the editor:

- **Short-form (Reels/TikTok/Shorts):** Burn in captions — most viewers watch on mute, captions are non-negotiable.
- **YouTube long-form:** Auto-generated captions are acceptable, but manually style the hook, payoff, and CTA captions for visual impact.
- **Tutorial:** Auto-generated is fine; flag any technical terms that auto-captions are likely to misspell.

### 6. Pre-Filming Checklist

Use these as internal review prompts. Append a filming checklist only when the user requests one; do not automatically duplicate AV directions or routine setup reminders in the deliverable.

```
## Pre-Filming Checklist

- [ ] Hook architecture fits the audience, evidence, and title/thumbnail promise
- [ ] Lazar's full hook is 20–30 seconds at his selected pace, with early relevance
- [ ] Existing MOGRT follows the hook; exact labels, 2–3-second duration, and audio state are specified
- [ ] First body beat fulfills the promise without a second introduction
- [ ] Script printed or on teleprompter/tablet
- [ ] All screen recordings captured and labeled
- [ ] Camera framing set for each shot type (wide, medium, tight)
- [ ] Audio levels tested (lapel mic or shotgun mic positioned)
- [ ] Lighting consistent across all talking head shots
- [ ] B-roll footage list reviewed — nothing missing
- [ ] Text overlay list sent to editor (or saved for self-editing)
- [ ] Captions strategy decided: burned-in (Reels/Shorts), auto-generated (YouTube), or manual SRT
- [ ] Captions list exported and shared with editor
- [ ] Technical-term correction list shared with editor if applicable
- [ ] CTA link/resource is live and tested before publishing
```

---

## Anti-Patterns

**NEVER do these when writing AV scripts:**

- **Vague audio cells.** Every AUDIO cell is a narrator line, explicit silence with duration, or SFX. Stage-direction notes like "Narration continues over B-roll," "Brief hold, no new line," or "Same audio as above" are forbidden — they force the creator and editor to guess what the audience hears.
- **Splitting a narrator line across two rows.** If a visual changes in the middle of a sentence, describe the change in the VIDEO cell of the same row and use a sync phrase ("As the narrator says X…"). Do not break the line in half.
- **Essay-style writing.** Scripts are spoken language. Short sentences. Contractions. Fragments are fine. If it sounds stiff read aloud, rewrite it.
- **Skipping or underbuilding the hook.** "Let me introduce myself" is not a hook. Earn attention in the first seconds, then sustain relevance and a credible payoff throughout Lazar's 20–30-second opening.
- **Empty curiosity or fabricated pain.** A vague secret, unsupported shocking claim, or invented crisis does not earn trust. The body must answer the specific question or deliver the promised outcome.
- **Breaking the opening sequence.** Do not put Lazar's bumper before the hook, omit it, narrate through it, redesign the existing MOGRT, or restart the introduction afterward.
- **Forgetting visual purpose.** Specify the evidence or idea each visual supports. Avoid long stretches that do not progress, and avoid decorative interruptions that make technical evidence harder to read. No fixed cut interval guarantees attention.
- **Skipping captions on short-form.** Most Reels, TikToks, and Shorts are watched on mute. If the key message isn't readable, it doesn't land. Every punchline, stat, and CTA needs a burn-in caption.
- **Walls of dialogue.** No segment should run longer than 90 seconds without a shot change. Break it up.
- **Vague B-roll descriptions.** "B-roll: something relevant" is useless. Be specific: "Screen recording of Notion kanban board with 3 tasks in the Today column."
- **Multiple CTAs.** One video, one CTA. Do not ask them to subscribe AND follow AND buy AND join.
- **No timing estimates.** A script without timing is a guess. Every segment header carries `(MM:SS–MM:SS)`.
- **Writing for readers, not speakers.** Use "you'll" not "you will," "can't" not "cannot." Read it out loud. If you stumble, simplify.

---

## Recovery

- **Vague topic** ("make a video about marketing"): Ask "What one thing about marketing should the viewer know after watching?" Narrow until you have a concrete angle.
- **No clear CTA**: Ask "What one thing do you want the viewer to do after watching — subscribe, follow, visit a link, try something, or leave a comment?" If they still can't decide, default to "subscribe and comment below." Note they can swap it before publishing.
- **No CTA idea at all**: Use "subscribe + comment with [relevant question tied to the video topic]" as a placeholder. This keeps engagement active and gives the creator something to replace once they have a resource or offer ready.
- **Script runs too long**: Cut the weakest segment first. Reduce examples from 2 to 1 per segment. Never speed up pacing — trim content instead.
- **Script runs too short**: Add depth to existing segments (specific examples, a brief story, a "common mistake" callout). Never add filler segments.
- **Unknown format**: Ask for target length, platform, and audience. Build using the same Phase 1-4 workflow.
- **Ambiguous audio cell slips through**: Find the row, ask "what does the audience actually hear in this beat?" If the answer is "the previous line continues," fold this row's visual into the previous row's VIDEO cell with a sync phrase, and delete this row. If the answer is "nothing," replace the cell with `Silent hold, ~X seconds.`. If the answer is "SFX only," write the SFX line.
- **Voice or flow is rejected**: Revisit the creator's direct corrections and previously used scripts. For Lazar, reread `references/lazar-voice.md`. Rework the story, cadence, and transitions where needed; do not limit the revision to pronouns or adjective swaps.
- **If 3 revision attempts fail**: **Stop and reassess.** First use available previous scripts and direct feedback. If those do not provide enough evidence, ask the user to record a 2-minute voice memo explaining what they want. Use it as source material for tone, pacing, and vocabulary. Restart from Phase 2.

---

## Quick Reference: Script Math

| Format | Words | Segments | Visual Changes | Initial Relevance |
|---|---|---|---|---|
| YouTube 8-15 min | 1,040-2,400 | 4-6 + intro/outro | Every 5-8 sec | 3 seconds |
| Short-form 30-60 sec | 65-160 | 1-3 + hook/CTA | Every 2-3 sec | 1-2 seconds |
| Tutorial 5-10 min | 650-1,600 | 3-5 steps + recap | Every 5-8 sec | 5 seconds |
| Talking head 3-5 min | 390-800 | 2-3 + CTA | Every 10-15 sec | 3 seconds |

**Speaking pace:** 130-160 words per minute for natural, conversational delivery. Lazar's planning baseline is 150 WPM. **His full opening is a 20–30-second hook plus a separate 2–3-second MOGRT bumper**; the initial-relevance timings above are not full-hook limits. Use actual words and silence durations (see Timing Validation).
