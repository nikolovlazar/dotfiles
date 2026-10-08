# Lazar's voice and presentation preferences

Read this when drafting or revising Lazar's personal videos. These preferences come from his direct corrections and scripts used in previous videos. Current instructions for the particular video take precedence. This profile supports different subjects and presentation modes; it does not prescribe one hook or sentence pattern for every video.

## Voice and story

**Match the intended audience.** For development topics, speak to another developer. Keep the language friendly, casual, and direct across subjects. Use contractions and familiar verbs. Explain enough technical context for the viewer to follow the workflow, then let the concrete example carry the explanation. Do not assume that a product term is self-explanatory. Prefer “an application metric to measure checkout duration” to an awkward compound such as “duration application metric.”

**Connect the steps.** Build a workflow with a reason for each move: a question, an observation, a finding to verify, or a next step that follows from the evidence. A segment boundary should not reset the explanation. “I needed to verify that finding, so I went to Sentry and grouped the metric by region” connects motivation to action. A string of isolated declarations or a narrated checklist loses that flow.

**Make it comfortable to say aloud.** Mix concise observations with complete explanatory sentences. Keep the phrasing natural across the whole paragraph. Repeated sentence openings, formal constructions such as “I had the agent do the investigation for me,” and excessive fragments can make otherwise correct copy sound written. Casual delivery does not require invented reactions, constant jokes, slang, or a more energetic persona.

**Give the hook a relevant payoff.** Establish a useful question, capability, or concrete result that a developer would care about before narrowing to the demo's details. Do not invent difficulty around a routine task or use a gloomy setup without a reason to keep watching. Lazar has selected both an evidence-led opening (“Checkout just got over ten times slower… Here’s how I did it”) and a discovery opening (“Did you know you can search…”). Choose the architecture that fits this video's evidence and audience.

## Default opening format

Every new or revised Lazar video uses **a 20–30-second hook → his existing premade 2–3-second MOGRT title-card bumper → body**, unless he explicitly requests another structure. Establish relevance immediately, often through a real problem the intended viewer recognizes, then develop a credible payoff across the full hook. Read [the researched hook playbook](video-hooks.md) to select the architecture or offer topic-specific opener examples for a choice. The bumper has no narration; specify its exact labels and known audio state. The first body beat begins fulfilling the promise. Choose the first body beat from the active subject and supply the context needed to follow it; do not import a previous video's workflow.

This is an explicit production preference established September 29, 2026, rather than a claim that every video audience requires these timings.

## Presentation mode

Default Lazar’s teleprompter scripts to **talking head with prepared overlays**, applying the camera-facing narration rule in the main skill. Record narration and screen captures separately. Use live screen operation language only when Lazar explicitly requests a live walkthrough; an improvised demo outline is also a separate, explicitly requested format.

| Mode | Spoken approach |
|---|---|
| Explicitly requested live screen walkthrough | Use present-tense actions and intentional navigation when the page change matters: “Let's go to the Metrics Explorer page.” Speak as someone guiding the viewer through the workflow. Describe the finding and its significance without narrating every click. |
| Completed investigation | Introductions such as “Here's how I did it” establish past tense. Keep connected actions in that timeline. General capabilities can remain present tense; proposed follow-ups stay conditional. |
| Talking head with prepared overlays (teleprompter default) | Explain the example, behavior, evidence, and significance while separately recorded screen overlays illustrate them. Keep clicks and navigation in VIDEO. Say “The trace contains…” or “The linked issue shows…” rather than “I’m on…” or “Let’s open…”. Every line must work while facing the camera continuously. |
| Improvised demo outline | When requested, give a short ordered list of speaking cues that can be glanced at while filming. Do not turn it into a verbose pseudo-script. Full AV narration remains the default for an AV-script request. |

Use **you** for the viewer's capabilities and guidance, **I** for Lazar's own actions, and **we** for a clearly shared activity. Bridge changes in perspective. Pronoun consistency supports the voice but does not establish it by itself.

## Examples to calibrate wording

| Wording Lazar corrected | Preferred or used wording | What to carry forward |
|---|---|---|
| “Now I'm in the Metrics Explorer page.” | “Let's go to the Metrics Explorer page.” | Applies only to an explicitly requested live walkthrough. For teleprompter narration, use an indirect explanation such as “Metrics Explorer shows the metric grouped by region.” |
| “With the Sentry plugin, I had the agent do the investigation for me.” | “Then I asked Codex to investigate using the Sentry Agent plugin.” | Straightforward action, natural word order, and varied sentence openings. |
| “duration application metric” | “an application metric to measure checkout duration” | Name the thing and explain its purpose in ordinary language. |
| “Here's how I did it” followed by “I'll go group the metric…” | “I needed to verify that finding, so I went to Sentry and grouped the metric by region.” | A connected reason for the step, in the established timeline. |

These excerpts show the cadence of scripts used in prior videos:

> Codex identified the slowdown in our European backend. I needed to verify that finding, so I went to Sentry and grouped the metric by region. Europe spiked; the US stayed steady.

> I opened a slow measurement and followed its linked trace. In that checkout, the request to Stripe took almost four seconds and accounted for most of the delay.

Use the excerpts for language and flow. Their product behavior and telemetry belong to those demonstrations; verify current claims and the selected example rather than reusing historical facts as evidence.

## Technical credibility and delivery

Let the demonstrated developer benefit carry the product story. For PMM-script rewrites, retain the requested overall flow while replacing unnatural wording and correcting unsupported claims. Do not present a common industry capability as a product's unique differentiator. Check current documentation and actual demo data where needed; distinguish observations, inferences, and proposed next steps. A slow request in one trace does not establish a provider incident. Follow-up advice should resolve an open question or suggest a useful response, rather than recommend checking information already visible in the trace.

Use **150 WPM** as Lazar's planning baseline. He timed a take at 158 WPM and said it was slightly faster than usual, then requested 150 WPM. Individual recordings may differ; use the actual delivery and phrase cues when footage is supplied. Trim content to fit rather than assuming he will speak faster. Keep the skill's existing teleprompter rule: pause cues belong within paragraphs where a deliberate pause helps, not at segment or paragraph endings.

## Before delivering or revising

- Read **hook → title card → first body beat** together. Is the payoff relevant, credible, and delivered by the example? Check the complete hook duration, bumper labels/audio, and continuity after the breather.
- Read the narration as connected speech. Does each transition explain why the next step follows? Would it sound natural while addressing the viewer?
- Check the presentation mode, tense, and pronoun referents against the actual visuals and timeline.
- Replace unexplained jargon, awkward noun compounds, repeated constructions, and checklist cadence.
- Verify factual claims separately from matching the voice. Do not replace an inaccurate promotional claim with a different unsupported claim.
- Check the duration at the selected pace and keep AV narration and teleprompter wording synchronized.
- If Lazar rejects the voice, use his new correction to reassess the approach. An unapproved generated revision is not a new approved exemplar.

## Evidence

The main source is the conversation **Write Application Metrics video**: direct requests for friendly viewer-facing wording, smoother workflow transitions, intentional navigation, consistent tense, overlays versus live operation, concise demo outlines, and 150 WPM; plus the recorded script **From a Metric Spike to the Trace**. The pronoun preference also appears in the earlier Sentry Explore script work. The distributed-tracing rewrite feedback adds an explicit reminder to consult those actual examples rather than treating a pronoun rule as a complete voice model.

## Script package preferences

Use the compact header specified in the main skill: runtime with `~`, spoken word count, a specific audience persona, tone, hook architecture plus delivery emotion, selling point, and an ordered Flow list. Omit Draft, Format, and Opening format fields. Include phrase cues when they help perform the hook's emotion.

Keep recording notes to essential, nonduplicated information; normally zero to three short bullets. Duration and title-card instructions already belong in the AV beats. Preserve the verified evidence/source section. For YouTube releases, include the researched publishing package required by the main skill, tailored to the current video's actual content.
