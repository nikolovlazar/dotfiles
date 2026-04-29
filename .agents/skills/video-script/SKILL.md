---
name: video-script
description: "Writes ready-to-film AV scripts with two-column VIDEO|AUDIO beats, hooks, segment breakdowns, B-roll cues, text overlay notes, timing estimates, and shot lists. Use when a user needs a script for YouTube, TikTok, Instagram Reels, course content, or any video format where a structured script improves production quality."
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

| Format | Target Length | Pacing | Hook Window | Segments |
|--------|-------------|--------|-------------|----------|
| YouTube long-form | 8-15 min | Visual change every 5-8 sec | First 3 sec | 4-6 + intro/outro |
| Short-form (Reel/TikTok/Short) | 30-60 sec | Visual change every 2-3 sec | First 1-2 sec | Hook + 1-3 points + CTA |
| Tutorial | 5-10 min | Match pacing to steps | First 5 sec | Setup + 3-5 steps + recap |
| Talking head | 3-5 min | Cut every 10-15 sec | First 3 sec | 2-3 segments + CTA |

---

## Phase 1: Brief

Gather these details before writing anything. Ask for anything the user does not provide.

1. **Topic** — the specific angle, not just the broad subject
2. **Format** — YouTube long-form, short-form reel, tutorial, or talking head (default: YouTube long-form)
3. **Target length** — use quick reference defaults if not specified
4. **Key points** — 3-5 things the video must cover, in priority order
5. **CTA** — what should the viewer do after watching? (subscribe, visit link, comment, try something)
6. **Tone** — default: conversational and direct, like explaining to a smart friend
7. **Audience** — default: entrepreneurs and solo creators

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

**GATE: Do not proceed to Phase 2 until the user confirms or adjusts the brief.**

---

## Phase 2: Structure

Build the script skeleton before writing full dialogue. This prevents rambling and keeps every segment purposeful.

### Hook Formula (First 3 Seconds)

Pick the hook type that fits the topic best:

| Hook Type | Structure | Best For |
|-----------|-----------|----------|
| **Pattern interrupt** | Say/show something unexpected that breaks the scroll | Short-form, entertainment |
| **Bold claim** | State a specific, provocative result or opinion | Long-form, authority content |
| **Direct question** | Ask a question the target audience cannot ignore | Tutorials, how-to content |

**Hook rules:**
- **NEVER** start with "Hey guys, welcome to my channel" — instant skip
- **NEVER** start with "In this video, I'm going to..." — show, do not preview
- The hook must work with NO context — assume the viewer knows nothing about you

### Outline Template

Build and present this structure:

```
## Script Outline

**HOOK** (0:00-0:08) — Bold claim: "These 5 tools replaced my entire team."

**Segment 1: The Problem** (0:08-1:00)
- Why solopreneurs waste 15+ hours/week on tasks tools can handle
- Transition: "Let me show you what I use instead."

**Segment 2: Tool #1 — Notion** (1:00-2:30)
- What it replaces, one specific workflow, result
- B-roll: screen recording of Notion dashboard

**Segment 3: Tool #2 — Cal.com** (2:30-3:45)
- What it replaces, one specific workflow, result
- B-roll: screen recording of booking page

**Segment 4: Tool #3 — Kit** (3:45-5:15)
- What it replaces, one specific automation, result

**Segment 5: Tool #4 — Canva** (5:15-6:30)
- What it replaces, one specific design use case, result

**Segment 6: Tool #5 — Claude** (6:30-7:45)
- What it replaces, one specific prompt workflow, result

**RECAP + CTA** (7:45-8:30)
- Quick recap of all 5 tools
- CTA: subscribe + free comparison PDF in description
```

**GATE: Do not proceed to Phase 3 until the user approves the outline.**

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

### AUDIO Cell Rules — Read This Twice

Every AUDIO cell MUST be one of these three things (or a combination of them):

1. **A narrator line.** The exact words spoken, formatted as `**NARRATOR:** "..."`. Use `**HOST:**`, `**GUEST:**`, `**VO:**`, etc. when there is more than one speaker.
2. **Explicit silence with duration.** `*Silent beat, ~1 second.*` or `*Hold ~2 seconds, silent, to let the line land.*`. Always give the duration. Optionally state the rhetorical purpose ("to let the caption land," "for effect," "pause for laugh").
3. **SFX or music.** `**SFX:** outro stinger.` or `*Soft sting to land the title.*`. Can be combined with a narrator line or with silence in the same cell.

DO NOT write vague stage-direction notes in the AUDIO column. Forbidden phrases include:

- ❌ "Narration continues over B-roll — no new line" → is there narration or not? Which line? State it, or fold this beat into the row whose narration it belongs to.
- ❌ "Brief hold, no new line" → is there silence? music? for how long? Make it explicit: `*Silent hold, ~1 second.*`
- ❌ "Same audio as above" → restate it or merge the rows.
- ❌ "Continues" / "Ongoing" / "No change" → these are notes, not audio descriptions.

Rule of thumb: if the cleanest way to describe the audio is "it is continuing from the previous row," this beat does not deserve its own row. Fold the visual into the row whose narration it syncs with (see next section).

### Scene Changes Mid-Sentence

When a visual changes in the middle of a narrator line, describe the scene change **in the VIDEO cell of the same row as the narration** — do NOT split a single narrator line across two rows just because the visual changes.

Use sync phrases like "As the narrator says X," "On the word Y," or "Synced to the phrase Z" so the editor knows when to trigger the visual.

✅ Right — visual change described in VIDEO cell, narration stays in one row:

```
| Cut back to talking head, medium shot. <br> As the narrator says "trace-connected," burn-in caption appears: *"Every metric is trace-connected."* | **NARRATOR:** "What makes them different — every metric event is trace-connected. One click from a point on a chart, and you're inside the trace that produced it." |
```

❌ Wrong — narrator line split across two rows, second row's audio is a stage-direction note:

```
| Cut back to talking head, medium shot. | **NARRATOR:** "What makes them different — every metric event is trace-connected. One click from a point on a chart, and you're inside the trace that produced it." |
| Burn-in caption appears: *"Every metric is trace-connected."* | *Narration continues, no new line.* |
```

If multiple visual events happen during a single narration line (cut + caption + B-roll), chain them in the VIDEO cell in order, each with its own sync phrase.

### VIDEO Cell Rules

- State the shot type: talking head, screen recording, animated card, B-roll overlay, title card, end screen.
- State the framing when it changes (wide, medium, tight). When the shot holds, write "Same shot."
- When a text overlay or caption is burned in during this beat, include the exact text in quotes on its own line within the cell.
- Use `<br>` to separate distinct visual elements within a single cell (shot description + overlay + caption).
- Be specific about screen recordings — name the app, the view, the interaction, and any text the viewer should notice.

### Script Writing Rules

- **Write exactly what the narrator will say.** No summaries, no "talk about X here," no paraphrases.
- **One idea per segment.** If a segment covers two ideas, split it.
- **One beat per row.** A beat ends when the shot changes, the narrator starts a new discrete thought, or the audio state changes (narration → silence, silence → SFX, etc.).
- **Visual change every 5-8 seconds** for long-form, **every 2-3 seconds** for short-form. Count rows and shot-change markers to check pacing.
- **Segment headers** use the form `## SEGMENT X: TITLE (MM:SS–MM:SS)`.
- **Every AUDIO cell** starts with `**NARRATOR:**` (or speaker label), or is wrapped in italic asterisks describing silence/SFX, or is a `**SFX:**` line. There is no fourth option.

### Example 1: YouTube Long-Form — "5 Tools Every Solopreneur Needs" (8 min)

Below is the hook, one full segment, and the recap/CTA. Apply the same AV table format to every segment in the full script.

```
## HOOK (0:00–0:08)

| VIDEO | AUDIO |
|---|---|
| Medium shot. Speaker holds up phone showing a grid of app icons. <br> Text on screen: *"5 Tools That Replaced My Team"* <br> Burn-in caption: *"5 Tools That Replaced My Team"* | **NARRATOR:** "These five tools replaced my entire team." |
| Same shot. | **NARRATOR:** "I run a one-person business that does six figures a year. No VA. No contractor. Just me and these five tools." |
| Same shot. | **NARRATOR:** "By the end of this video, you'll have the exact setup I use — plus a free cheat sheet in the description." |

---

## SEGMENT 1: THE PROBLEM (0:08–1:00)

| VIDEO | AUDIO |
|---|---|
| Cut to talking head, wide shot. | **NARRATOR:** "Here's the thing nobody tells you about going solo." |
| Same shot. <br> As the narrator lists the roles, text on screen builds up one by one: *"CEO + Marketer + Accountant + Support + Designer + IT"* | **NARRATOR:** "When you start a business by yourself, you don't just do the work — you do everything. You're the CEO, the marketer, the accountant, the customer support rep, the designer, and the IT department." |
| Same shot. | **NARRATOR:** "And most solopreneurs spend 15 or more hours a week on stuff that has nothing to do with making money. Admin. Scheduling. Formatting. Busywork." |
| Cut to B-roll — time-lapse of a messy desk with papers and sticky notes piling up. | **NARRATOR:** "I used to be there. I was working 60-hour weeks and wondering why my revenue wasn't growing. Then I made one change." |
| Cut back to talking head, wide shot. | *Silent beat, ~1 second — pause for effect before the reveal.* |
| Same shot. | **NARRATOR:** "I built a five-tool system that handles 80% of the operational work automatically. Let me show you what I use." |

---

## SEGMENT 2: TOOL #1 — NOTION (1:00–2:30)

| VIDEO | AUDIO |
|---|---|
| Cut to talking head, medium shot. <br> Text on screen: *"Tool #1: Notion"* <br> Burn-in caption: *"Tool #1: Notion"* | **SFX:** subtle whoosh transition. <br> **NARRATOR:** "Tool number one — Notion — and this is the command center." |
| Same shot. | **NARRATOR:** "Before Notion, I had tasks in Apple Notes, project details in Google Docs, and client info in a spreadsheet. Three places to check before I could start actual work." |
| Cut to screen recording — Notion dashboard showing a kanban board, a database view, and a calendar view side by side. <br> Text on screen: *"Tasks + Calendar + Clients + SOPs = 1 workspace"* | **NARRATOR:** "Now everything lives here. My task board, my content calendar, my client database, my SOPs — all in one workspace." |
| Screen recording continues — presenter drags three tasks from the backlog column into the "Today" column. | **NARRATOR:** "Here's my actual workflow: every Monday I open this dashboard, check my weekly tasks, and drag my top three priorities to the 'Today' column. That's it. No more wondering what to do first." |
| Cut back to talking head, medium shot. <br> As the narrator names the savings, text on screen: *"45 min/day → 5 min/day = 3+ hours saved/week"* <br> Burn-in caption: *"45 min → 5 min = 3+ hours saved every week"* | **NARRATOR:** "The result? I cut my planning time from 45 minutes a day to 5 minutes. That's over 3 hours a week back." |

---

(Segments 3–6 follow the same AV table format: a cut + label text at the segment open, screen-recording rows with synced text overlays, and a talking-head close with a stat callout.)

---

## RECAP + CTA (7:45–8:30)

| VIDEO | AUDIO |
|---|---|
| Cut to talking head, wide shot — match the hook's framing. | **NARRATOR:** "So here's the full stack." |
| Same shot. <br> Text on screen: the five tool names appear one at a time, each synced to the narrator saying it. | **NARRATOR:** "Notion for project management. Cal.com for scheduling. Kit for email. Canva for design. Claude for thinking." |
| Same shot. <br> Text on screen: *"5 tools — <$100/mo — 10+ hours saved/week"* <br> Burn-in caption: *"5 tools · <$100/mo · 10+ hours saved/week"* | **NARRATOR:** "Five tools, under a hundred dollars a month total, and they save me 10 or more hours every single week." |
| Cut to B-roll — arrow graphic pointing down toward the description area. | **NARRATOR:** "I put together a free comparison sheet with pricing, links, and my exact setup for each tool. It is in the description below — go grab it." |
| Cut back to talking head, wide shot. <br> Text on screen: *"SUBSCRIBE for more solo business systems"* <br> Burn-in caption: *"SUBSCRIBE for more solo business systems"* | **NARRATOR:** "If this was helpful, hit subscribe. I make videos like this every week about running a one-person business that actually scales." |
| Same shot. | **NARRATOR:** "And if you're already using any of these tools, drop a comment and tell me which one is your favorite. I read every single one." |
| Cut to end screen — subscribe button and suggested video card. | **SFX:** end screen music fades in. |
```

### Timing Validation

After writing, count spoken narrator words and validate against targets. NARRATOR lines are always wrapped in double quotes after the `**NARRATOR:**` label, so extract them specifically:

```bash
# Extract narrator dialogue and count words
grep -oE '\*\*NARRATOR:\*\* "[^"]+"' script.md | sed 's/\*\*NARRATOR:\*\* //' | tr -d '"' | wc -w
```

- **Speaking pace:** 130-160 words per minute (conversational, not rushed)
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

Write the complete AV script to a file if the user requests it. Default filename: `video-script.md`. Every segment is a two-column table; the file begins with a header block stating format, runtime target, word count, audience, and tone, plus a one-line "how to read the table" note for the editor.

### 2. Shot List Summary

Append a table listing every unique camera setup and visual asset:

```
## Shot List

| # | Shot Type | Description | Duration | Segment |
|---|-----------|-------------|----------|---------|
| 1 | Medium shot | Speaker holding phone, app grid visible | 8s | Hook |
| 2 | Wide talking head | Speaker at desk | 52s | Segment 1 |
| 3 | Screen recording | Notion dashboard — kanban, calendar | 30s | Segment 2 B-roll |
| 4 | Screen recording | Cal.com booking page | 15s | Segment 3 B-roll |
| 5 | Screen recording | Kit automation flow | 30s | Segment 4 B-roll |
| 6 | Screen recording | Canva template + export | 20s | Segment 5 B-roll |
| 7 | Screen recording | Claude outline workflow | 20s | Segment 6 B-roll |
| 8 | Wide talking head | Same framing as hook | 45s | Recap + CTA |
| 9 | End screen | Subscribe button + suggested video | 15s | Outro |

**Total setups:** 3 camera positions + 5 screen recordings + 1 end screen
**Estimated filming time:** 45-60 minutes (with retakes)
```

### 3. Text Overlay List

Extract every burned-in text overlay referenced in VIDEO cells into a standalone table with timestamps and placement notes for the editor.

### 4. Captions List

Extract every burned-in caption into a standalone table with timestamps. Note the caption strategy for the editor:

- **Short-form (Reels/TikTok/Shorts):** Burn in captions — most viewers watch on mute, captions are non-negotiable.
- **YouTube long-form:** Auto-generated captions are acceptable, but manually style the hook, payoff, and CTA captions for visual impact.
- **Tutorial:** Auto-generated is fine; flag any technical terms that auto-captions are likely to misspell.

### 5. Pre-Filming Checklist

```
## Pre-Filming Checklist

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

## Example 2: Short-Form Instagram Reel — "One Productivity Tip in 30 Seconds"

```
## Script Brief

**Topic:** The 2-minute rule for beating procrastination
**Format:** Instagram Reel / Short-form (30 seconds)
**Key points:**
1. If a task takes under 2 minutes, do it immediately
2. It eliminates the mental pile-up of tiny tasks
**CTA:** Follow for more productivity tips
**Tone:** High energy, punchy, direct
```

```
## HOOK (0:00–0:03)

| VIDEO | AUDIO |
|---|---|
| Tight shot. Speaker snaps fingers on the first beat. <br> Text on screen: *"THE 2-MINUTE RULE"* <br> Burn-in caption: *"THE 2-MINUTE RULE"* | **SFX:** finger snap. <br> **NARRATOR:** "You're procrastinating wrong." |

---

## POINT (0:03–0:22)

| VIDEO | AUDIO |
|---|---|
| Cut to medium shot. Speaker talking fast and direct. <br> As the narrator says "do it NOW," text on screen slams in: *"< 2 minutes? Do it NOW."* <br> Burn-in caption: *"< 2 minutes? Do it NOW."* | **NARRATOR:** "Here's the rule: if something takes less than two minutes — replying to that email, putting away the dishes, sending that invoice — do it NOW." |
| Same shot. | **NARRATOR:** "Not later. Not after lunch. Right now." |
| Cut to B-roll — rapid montage: typing an email, filing a paper into a folder, clicking "send" on an invoice. Three quick cuts, ~1 second each. | *No narration during the montage, ~3 seconds — let the visuals carry the beat.* |
| Cut back to medium shot. <br> Text on screen: *"Small tasks pile up → big mental drain"* | **NARRATOR:** "Because those tiny tasks pile up in your brain and drain your focus on the stuff that actually matters." |

---

## CTA (0:22–0:30)

| VIDEO | AUDIO |
|---|---|
| Cut to tight shot. Speaker points at camera. <br> Text on screen: *"FOLLOW for daily productivity tips"* <br> Burn-in caption: *"FOLLOW for daily productivity tips"* | **NARRATOR:** "Try it for one day. You'll feel the difference by noon. Follow for more." |
| Cut to end screen — follow button animation. | **SFX:** upbeat stinger. |
```

**Shot list:** 2 camera positions (tight, medium) + 3 B-roll clips (email, filing, invoice at ~1 second each) + 1 end screen. Estimated filming time: 10-15 minutes.

---

## Anti-Patterns

**NEVER do these when writing AV scripts:**

- **Vague audio cells.** Every AUDIO cell is a narrator line, explicit silence with duration, or SFX. Stage-direction notes like "Narration continues over B-roll," "Brief hold, no new line," or "Same audio as above" are forbidden — they force the creator and editor to guess what the audience hears.
- **Splitting a narrator line across two rows.** If a visual changes in the middle of a sentence, describe the change in the VIDEO cell of the same row and use a sync phrase ("As the narrator says X…"). Do not break the line in half.
- **Essay-style writing.** Scripts are spoken language. Short sentences. Contractions. Fragments are fine. If it sounds stiff read aloud, rewrite it.
- **Skipping the hook.** "Let me introduce myself" is not a hook. The first 3 seconds must give the viewer a reason to stay.
- **Forgetting visual cues.** A script with no shot changes, no B-roll, and no on-screen text is just an essay. If there are no visual changes for 15+ seconds of long-form, the viewer is gone.
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
- **Ambiguous audio cell slips through**: Find the row, ask "what does the audience actually hear in this beat?" If the answer is "the previous line continues," fold this row's visual into the previous row's VIDEO cell with a sync phrase, and delete this row. If the answer is "nothing," replace the cell with `*Silent hold, ~X seconds.*`. If the answer is "SFX only," write the SFX line.
- **If 3 revision attempts fail**: **Stop and reassess.** Ask the user to record a 2-minute voice memo explaining what they want. Use that as source material for tone, pacing, and vocabulary. Restart from Phase 2.

---

## Quick Reference: Script Math

| Format | Words | Segments | Visual Changes | Hook Window |
|--------|-------|----------|----------------|-------------|
| YouTube 8-15 min | 1,040-2,400 | 4-6 + intro/outro | Every 5-8 sec | 3 seconds |
| Short-form 30-60 sec | 65-160 | 1-3 + hook/CTA | Every 2-3 sec | 1-2 seconds |
| Tutorial 5-10 min | 650-1,600 | 3-5 steps + recap | Every 5-8 sec | 5 seconds |
| Talking head 3-5 min | 390-800 | 2-3 + CTA | Every 10-15 sec | 3 seconds |

**Speaking pace:** 130-160 words per minute for natural, conversational delivery. Adjust targets if the creator's natural pace is faster or slower (see Timing Validation for guidance).
