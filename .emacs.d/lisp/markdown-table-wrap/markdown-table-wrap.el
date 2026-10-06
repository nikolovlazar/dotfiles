;;; markdown-table-wrap.el --- Word-wrap GFM pipe tables to fit window width -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/markdown-table-wrap
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: text, markdown, tables

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Rewrite GFM (GitHub Flavored Markdown) pipe tables so they fit a
;; given character width.  Cells that exceed their allocated column
;; width are word-wrapped by emitting additional pipe-table lines.
;; The wrapped text is meant for readable source editing and
;; round-tripping with `markdown-table-wrap-unwrap': wrapped header
;; lines no longer form valid GFM tables, and wrapped body
;; continuation lines and spacer lines are parsed as additional
;; rows by Markdown renderers.
;;
;; The main entry point is `markdown-table-wrap':
;;
;;   (markdown-table-wrap TABLE-TEXT WIDTH)
;;   (markdown-table-wrap TABLE-TEXT WIDTH MAX-CELL-HEIGHT)
;;   (markdown-table-wrap TABLE-TEXT WIDTH MAX-CELL-HEIGHT STRIP-MARKUP)
;;   (markdown-table-wrap TABLE-TEXT WIDTH MAX-CELL-HEIGHT STRIP-MARKUP COMPACT)
;;
;; For rendering the same table at multiple widths (e.g., during
;; window resize), `markdown-table-wrap-batch' parses and measures
;; once, then renders at each width:
;;
;;   (markdown-table-wrap-batch TABLE-TEXT WIDTHS)
;;
;; To merge continuation rows back into logical rows (e.g., before
;; re-wrapping at a new width), use `markdown-table-wrap-unwrap':
;;
;;   (markdown-table-wrap
;;     (markdown-table-wrap-unwrap WRAPPED-TEXT) NEW-WIDTH)
;;
;; Features:
;; - Markup-aware wrapping: bold, italic, links, images, code
;;   (single and double backtick), and strikethrough spans are
;;   kept intact across line breaks.
;; - Graceful degradation: when a column is too narrow for markup
;;   overhead, markers are dropped and inner text is wrapped as
;;   plain text, preserving legibility over formatting.
;; - Waterfill column-width allocation: wider columns get more space
;;   (proportional to sqrt of content width) but the effect is
;;   dampened so narrow columns aren't starved.  Monotonic — widening
;;   the terminal never shrinks any column.
;; - Alignment preservation (left, right, center).
;; - Optional cell-height cap with ellipsis truncation.
;; - Automatic row separators for visual breathing room when wrapping
;;   occurs (opt out with COMPACT).
;; - Code-fence awareness: tables inside ``` or ~~~ blocks are
;;   left untouched.
;; - Unicode-aware width: CJK, combining characters, and VS16
;;   emoji measured correctly for terminal alignment.
;; - Backtick parity guard: cells whose wrapping would produce
;;   odd backtick counts fall back to single-line, preventing
;;   font-lock leaking across rows.
;; - Zero dependencies -- pure Elisp, no `markdown-mode' required.
;;
;; STRIP-MARKUP controls width measurement: nil (default) measures
;; raw string width; non-nil strips inline markup first (for use
;; when `markdown-hide-markup' is enabled).
;;
;; All public functions use the `markdown-table-wrap-' prefix.
;; Internal helpers use `markdown-table-wrap--' (double dash).
;; No `defcustom' is defined -- configuration is passed as
;; parameters.  The consuming package owns its own customization
;; variables and passes values through.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Width Measurement Control

(defvar markdown-table-wrap--strip-markup nil
  "When non-nil, strip inline markdown syntax before measuring width.
This variable controls all width measurement in the package:
`markdown-table-wrap-visible-width', `markdown-table-wrap-cell',
and `markdown-table-wrap--pad-cell' all read it.

The main entry point `markdown-table-wrap' binds this via `let'
based on its STRIP-MARKUP argument.  If you call lower-level public
functions directly (e.g. `markdown-table-wrap-cell'), bind this
variable yourself:

  (let ((markdown-table-wrap--strip-markup t))
    (markdown-table-wrap-cell text width))

Do not `setq' this variable globally.")

;;;; Display Width

(defun markdown-table-wrap--display-width (s)
  "Return display width of S in columns, correcting for VS16 emoji.
For ASCII-only strings, return `length' directly (O(1)).  For
strings containing non-ASCII characters, sum per-character
`char-width' values.  This avoids Emacs's `string-width' which,
in GUI frames, invokes the font-shaping engine
\(`find_automatic_composition') for every multibyte character —
up to 10× slower than a simple table lookup.

VS16 correction: Variation Selector 16 (U+FE0F) promotes certain
narrow characters to 2-wide emoji presentation in terminal
emulators.  When U+FE0F is present, +1 is added for each base
character followed by U+FE0F whose `char-width' is less than 2."
  (if (= (length s) (string-bytes s))
      ;; ASCII-only: every byte is one character, one column.
      (length s)
    ;; Non-ASCII: sum char-width per character.
    ;; Also check for VS16 (U+FE0F) correction in the same pass.
    (let ((len (length s))
          (w 0)
          (i 0)
          (has-vs16 nil))
      (while (< i len)
        (let ((ch (aref s i)))
          (setq w (+ w (char-width ch)))
          (when (= ch #xFE0F)
            (setq has-vs16 t)))
        (setq i (1+ i)))
      ;; VS16 correction pass (rare — only when U+FE0F present)
      (if (not has-vs16)
          w
        (setq i 0)
        (while (< i (1- len))
          (when (and (= (aref s (1+ i)) #xFE0F)
                     (< (char-width (aref s i)) 2))
            (setq w (1+ w)))
          (setq i (1+ i)))
        w))))

(defun markdown-table-wrap--display-width-from-to (s from to)
  "Return display width of S between character positions FROM and TO.
Like `markdown-table-wrap--display-width' but for a substring,
without allocating one.  Does not apply VS16 correction (callers
needing VS16 should use `display-width' on a real substring)."
  (let ((w 0))
    (while (< from to)
      (setq w (+ w (char-width (aref s from)))
            from (1+ from)))
    w))

;;;; Markup Stripping

(defun markdown-table-wrap-strip-markup (s)
  "Return S with markdown inline markup removed, leaving only visible text.
Strip syntax that `markdown-hide-markup' would hide:
- Code: `text` -> text, `` text `` -> text
- Images: ![alt](url) -> alt
- Links: [text](url) -> text
- Bold: **text** -> text
- Italic: *text* -> text
- Strikethrough: ~~text~~ -> text

Code spans are processed first because their content is literal —
`**not bold**` should produce **not bold**, not \"not bold\".
Double-backtick spans (`` `` content `` ``) are handled before
single-backtick to avoid false matches.  CommonMark trims one
leading and one trailing space from double-backtick content."
  (let ((result s)
        (code-spans nil)
        (placeholder-idx 0))
    ;; Phase 1: Extract code spans and replace with placeholders.
    ;; Double-backtick first (`` content ``), then single (` content `).
    ;; Placeholders are \0CODE<n>\0 — cannot appear in markdown.
    (setq result
          (replace-regexp-in-string
           "``\\([^`]\\|`[^`]\\)+``"
           (lambda (match)
             (let* ((inner (substring match 2 -2))  ; strip outer ``
                    (trimmed (if (and (string-prefix-p " " inner)
                                      (string-suffix-p " " inner)
                                      (> (length inner) 1))
                                 (substring inner 1 -1)
                               inner))
                    (ph (format "\x00CODE%d\x00" placeholder-idx)))
               (push (cons ph trimmed) code-spans)
               (setq placeholder-idx (1+ placeholder-idx))
               ph))
           result t t))
    (setq result
          (replace-regexp-in-string
           "`\\([^`]+\\)`"
           (lambda (match)
             (let* ((inner (substring match 1 -1))
                    (ph (format "\x00CODE%d\x00" placeholder-idx)))
               (push (cons ph inner) code-spans)
               (setq placeholder-idx (1+ placeholder-idx))
               ph))
           result t t))
    ;; Phase 2: Strip non-code markup (images, links, bold-italic,
    ;; bold, italic, strikethrough).  Code content is safe behind
    ;; placeholders.
    ;;
    ;; Bold-italic (***text***) is handled FIRST: markdown-mode's
    ;; `markdown-hide-markup' hides only the ** (bold) delimiters,
    ;; keeping the * (italic) markers visible.  So ***text*** displays
    ;; as *text*.  We use placeholders to prevent the italic pass from
    ;; stripping the remaining *.
    (let ((bi-spans nil)
          (bi-idx 0))
      ;; 2a: Bold-italic → placeholder (preserving italic markers)
      (setq result
            (replace-regexp-in-string
             "\\*\\*\\*\\([^*]+\\)\\*\\*\\*"
             (lambda (match)
               (let* ((inner (match-string 1 match))
                      (ph (format "\x00BI%d\x00" bi-idx)))
                 (push (cons ph (concat "*" inner "*")) bi-spans)
                 (setq bi-idx (1+ bi-idx))
                 ph))
             result t t))
      ;; 2a-bis: Protect * in ***text** sequences (split bold-italic).
      ;; When *** opens, markdown-mode treats it as ** (bold, hidden)
      ;; + * (visible content inside the bold span).  After bold
      ;; stripping, the remaining * must not be stripped as italic.
      ;; Pattern: *(** ... **) → protect the leading * with placeholder.
      (let ((star-spans nil)
            (star-idx 0))
        (setq result
              (replace-regexp-in-string
               "\\*\\(\\*\\*[^*]+\\*\\*\\)"
               (lambda (match)
                 (let* ((bold-part (match-string 1 match))
                        (ph (format "\x00STAR%d\x00" star-idx)))
                   (push (cons ph "*") star-spans)
                   (setq star-idx (1+ star-idx))
                   (concat ph bold-part)))
               result t t))
        ;; 2b: Regular markup stripping
        (setq result
              (thread-last result
                (replace-regexp-in-string "!\\[\\([^]]*\\)\\]([^)]*)" "\\1")
                (replace-regexp-in-string "\\[\\([^]]*\\)\\]([^)]*)" "\\1")
                (replace-regexp-in-string "\\*\\*\\([^*]+\\)\\*\\*" "\\1")
                (replace-regexp-in-string "\\*\\([^*]+\\)\\*" "\\1")
                (replace-regexp-in-string "~~\\([^~]+\\)~~" "\\1")))
        ;; 2a-ter: Restore protected * markers
        (dolist (pair star-spans)
          (setq result (replace-regexp-in-string
                        (regexp-quote (car pair)) (cdr pair)
                        result t t))))
      ;; 2c: Restore bold-italic as *text* (italic markers visible)
      (dolist (pair bi-spans)
        (setq result (replace-regexp-in-string
                      (regexp-quote (car pair)) (cdr pair)
                      result t t))))
    ;; Phase 3: Restore code span content from placeholders.
    (dolist (pair code-spans)
      (setq result (replace-regexp-in-string
                    (regexp-quote (car pair)) (cdr pair) result t t)))
    result))

(defun markdown-table-wrap-visible-width (s)
  "Return display width of S, accounting for markup and emoji.
When `markdown-table-wrap--strip-markup' is non-nil, strip markdown
syntax before measuring (for use with `markdown-hide-markup').
Otherwise, measure the raw string width.  In both cases, VS16
emoji correction is applied via `markdown-table-wrap--display-width'."
  (if markdown-table-wrap--strip-markup
      (markdown-table-wrap--display-width
       (markdown-table-wrap-strip-markup s))
    (markdown-table-wrap--display-width s)))

;;;; Internal Helpers

(defun markdown-table-wrap--vsum (vec)
  "Return the sum of all elements in vector VEC."
  (cl-loop for i below (length vec) sum (aref vec i)))

(defun markdown-table-wrap--pad-cell (cell width fmt)
  "Pad CELL to WIDTH using format FMT.
FMT is one of `right', `center', or nil (left-align).
Width measurement respects `markdown-table-wrap--strip-markup'."
  (if (string-empty-p cell)
      ;; Empty cell: visible-width is 0, result is always width spaces.
      (make-string width ?\s)
    (let* ((visible-width (markdown-table-wrap-visible-width cell))
           (padding-needed (max 0 (- width visible-width))))
      (pcase fmt
        ('right (concat (make-string padding-needed ?\s) cell))
        ('center (let ((left-pad (/ padding-needed 2)))
                   (concat (make-string left-pad ?\s)
                           cell
                           (make-string (- padding-needed left-pad) ?\s))))
        (_ (concat cell (make-string padding-needed ?\s)))))))

(defun markdown-table-wrap--tokenize-cell-text (text)
  "Split TEXT into tokens, keeping markdown markup spans intact.
Return a list of strings.  Plain text is split on whitespace.
Markup spans — images `![...](...)`, links `[...](...)`, bold
`**...**', italic `*...*', code \\=`...\\=`', strikethrough
`~~...~~' — are kept as single tokens even when they contain
internal spaces.

Tokens appear in original order.  Empty input returns nil.

Uses `string-match' with a START parameter instead of repeated
`substring' calls, avoiding O(n²) string copying."
  (let ((tokens nil)
        (pos 0)
        (len (length text))
        ;; Matches: `` code ``, ![alt](url), [text](url), ***bold-italic***,
        ;; **text**, ~~text~~, `text`, *text*.  Longer delimiters precede
        ;; shorter: double-backtick before single, bold-italic before bold,
        ;; bold before italic.  Images before links (both use brackets).
        ;; Italic allows internal spaces to match *multi word italic*.
        (span-re (concat
                  "``\\(?:[^`]\\|`[^`]\\)+``"         ; `` code ``
                  "\\|!\\[\\([^]]*\\)\\](\\([^)]*\\))" ; ![alt](url)
                  "\\|\\[\\([^]]*\\)\\](\\([^)]*\\))" ; [text](url)
                  "\\|\\*\\*\\*\\([^*]+\\)\\*\\*\\*"  ; ***bold-italic***
                  "\\|\\*\\*\\([^*]+\\)\\*\\*"        ; **text**
                  "\\|~~\\([^~]+\\)~~"                 ; ~~text~~
                  "\\|`\\([^`]+\\)`"                   ; `text`
                  "\\|\\*\\([^*]+\\)\\*"))             ; *text* (fast path)
        ;; Fallback for italic with nested **bold** (slower regex).
        ;; Only tried when fast italic fails at a * that isn't ** or ***.
        (italic-nested-re
         "\\*\\(\\(?:[^*]\\|\\*\\*[^*]*\\*\\*\\)+\\)\\*"))
    (while (< pos len)
      ;; Skip leading whitespace — match at pos, verify it starts there
      (when (and (string-match "[ \t]+" text pos)
                 (= (match-beginning 0) pos))
        (setq pos (match-end 0)))
      (when (< pos len)
        (let ((span-end nil))
          ;; Try fast span regex first
          (when (and (string-match span-re text pos)
                     (= (match-beginning 0) pos))
            (setq span-end (match-end 0))
            ;; If we matched fast italic (*[^*]+*) but the character
            ;; after the match is also *, the fast regex stopped too
            ;; early (e.g., *text **bold → matched *text *).
            ;; Retry with the slower nested-bold-aware italic regex.
            (when (and (= (aref text pos) ?*)
                       (< (1+ pos) len)
                       (/= (aref text (1+ pos)) ?*)
                       (< span-end len)
                       (= (aref text span-end) ?*)
                       (string-match italic-nested-re text pos)
                       (= (match-beginning 0) pos))
              (setq span-end (match-end 0))))
          (cond
           (span-end
            ;; Markup span — include trailing non-whitespace that is
            ;; not a markup opener or CJK character.
            (while (and (< span-end len)
                        (let ((ch (aref text span-end)))
                          (and (not (memq ch '(?\s ?\t)))
                               (not (memq ch (list ?* ?\` ?~ ?\[ ?!)))
                               (< (char-width ch) 2))))
              (setq span-end (1+ span-end)))
            (push (substring text pos span-end) tokens)
            (setq pos span-end))
           ;; Plain text — consume until whitespace or a real span
           ;; start.  A markup-opener char (* ` ~ ! [) that does NOT
           ;; begin a recognized span (a lone `~`, a bare `[Zig]`
           ;; with no `(url)`, a stray `*` or `!`) is kept as plain
           ;; text rather than split into a 1-char token.  Splitting
           ;; it made `markdown-table-wrap-cell' rejoin tokens with
           ;; spaces and re-wrap, corrupting cells like `~68',
           ;; `~36 hot', and `[raylib]'.  Since every span begins with
           ;; one of those opener chars, we only need to re-check
           ;; `span-re' when we encounter one; a real span embedded
           ;; later in the run (e.g. `foo[bar](url)') still
           ;; tokenizes as a span.
           (t
            (let ((start pos)
                  (ch nil))
              (while (and (< pos len)
                          (progn (setq ch (aref text pos))
                                 (and (not (memq ch '(?\s ?\t)))
                                      (or (not (memq ch '(?* ?\` ?~ ?! ?\[)))
                                          (not (and (string-match span-re text pos)
                                                    (= (match-beginning 0) pos)))))))
                (setq pos (1+ pos)))
              (if (> pos start)
                  (push (substring text start pos) tokens)
                ;; Cannot happen: span-end is nil (no span starts
                ;; here) and the char is not whitespace, so the loop
                ;; must consume at least one char.  Consume one to
                ;; guarantee progress regardless.
                (push (substring text pos (1+ pos)) tokens)
                (setq pos (1+ pos)))))))))
    (nreverse tokens)))

(defun markdown-table-wrap--markup-span-parts (token)
  "If TOKEN is a markdown markup span, return (PREFIX INNER SUFFIX).
PREFIX and SUFFIX are the markup delimiters, INNER is the content.
Return nil if TOKEN is not a recognized markup span.

When the tokenizer attaches trailing non-whitespace characters to
a span (e.g. `[mypy](url))'), those characters are included in
SUFFIX so the span structure is preserved for force-breaking.

Recognized spans (with optional trailing characters):
  `` content ``... -> (\"`` \" \"content\" \" ``...\")
  ![alt](url)...  -> (\"![\" \"alt\" \"](url)...\")
  [text](url)...  -> (\"[\" \"text\" \"](url)...\")
  ***text***...   -> (\"***\" \"text\" \"***...\")
  **text**...     -> (\"**\" \"text\" \"**...\")
  ~~text~~...     -> (\"~~\" \"text\" \"~~...\")
  `text`...       -> (\"`\" \"text\" \"`...\")
  *text*...       -> (\"*\" \"text\" \"*...\")

Bold-italic (***) must precede bold (**) to avoid partial matching."
  (cond
   ;; Double-backtick: `` content `` with optional trailing chars.
   ;; The greedy .* finds the last ` `` ` via backtracking, which is
   ;; correct: the tokenizer produces minimal double-backtick spans
   ;; with no nested `` `` `` ambiguity.
   ((string-match "\\`\\(`` \\)\\(.*\\)\\( ``\\)\\(.*\\)\\'" token)
    (list (match-string 1 token)
          (match-string 2 token)
          (concat (match-string 3 token) (match-string 4 token))))
   ((string-match "\\`!\\[\\([^]]*\\)\\](\\([^)]*\\))\\(.*\\)\\'" token)
    (list "![" (match-string 1 token)
          (concat "](" (match-string 2 token) ")" (match-string 3 token))))
   ((string-match "\\`\\[\\([^]]*\\)\\](\\([^)]*\\))\\(.*\\)\\'" token)
    (list "[" (match-string 1 token)
          (concat "](" (match-string 2 token) ")" (match-string 3 token))))
   ((string-match "\\`\\*\\*\\*\\(.*\\)\\*\\*\\*\\(.*\\)\\'" token)
    (list "***" (match-string 1 token)
          (concat "***" (match-string 2 token))))
   ((string-match "\\`\\*\\*\\(.*\\)\\*\\*\\(.*\\)\\'" token)
    (list "**" (match-string 1 token)
          (concat "**" (match-string 2 token))))
   ((string-match "\\`~~\\(.*\\)~~\\(.*\\)\\'" token)
    (list "~~" (match-string 1 token)
          (concat "~~" (match-string 2 token))))
   ((string-match "\\``\\(.*\\)`\\(.*\\)\\'" token)
    (list "`" (match-string 1 token)
          (concat "`" (match-string 2 token))))
   ((string-match "\\`\\*\\(\\(?:[^*]\\|\\*\\*[^*]*\\*\\*\\)+\\)\\*\\(.*\\)\\'" token)
    (list "*" (match-string 1 token)
          (concat "*" (match-string 2 token))))))

(defun markdown-table-wrap--split-table-row (line)
  "Split table LINE into cells, respecting escaped pipes.
Strip leading and trailing `|', split on unescaped `|',
trim each cell, and preserve `\\|' in cell text."
  (let* ((trimmed (string-trim line))
         (has-escape (string-search "\\" trimmed))
         (safe trimmed))
    (when has-escape
      ;; Protect escaped pipes with a sentinel before splitting.
      (setq safe (replace-regexp-in-string
                  "\\\\|" "\x00PIPE\x00" trimmed)))
    ;; Strip leading/trailing unescaped pipes
    (when (string-prefix-p "|" safe)
      (setq safe (substring safe 1)))
    (when (string-suffix-p "|" safe)
      (setq safe (substring safe 0 -1)))
    ;; Split on unescaped pipes, restore escaped pipes in each cell
    (if has-escape
        (mapcar (lambda (cell)
                  (string-trim
                   (replace-regexp-in-string
                    "\x00PIPE\x00" "\\|" cell t t)))
                (split-string safe "|"))
      (mapcar #'string-trim (split-string safe "|")))))

(defun markdown-table-wrap--odd-backtick-line-p (lines)
  "Return non-nil if any line in LINES has an odd number of backticks.
An odd backtick count on a visual row causes `markdown-mode' to
match its multiline inline-code regex across row boundaries,
leaking `markdown-inline-code-face' into unrelated cells."
  (cl-some (lambda (line)
             (cl-oddp (cl-count ?` line)))
           lines))

(defun markdown-table-wrap--truncate-cell-lines (cell-lines max-height)
  "Truncate CELL-LINES to MAX-HEIGHT, adding \"…\" if truncated.
When MAX-HEIGHT is nil or CELL-LINES is within the limit, return
CELL-LINES unchanged.  Otherwise, return a fresh list of the first
MAX-HEIGHT lines with the last replaced by \"…\"."
  (if (or (null max-height)
          (<= (length cell-lines) max-height))
      cell-lines
    (append (cl-subseq cell-lines 0 (1- max-height))
            (list "…"))))

;;;; Force-Breaking

(defun markdown-table-wrap--closing-delimiter (prefix suffix)
  "Extract the closing markup delimiter from SUFFIX.
PREFIX identifies the markup type.  The closing delimiter is at the
start of SUFFIX; any remaining characters are trailing punctuation
attached by the tokenizer (e.g., commas, semicolons, parens).

For symmetric markers (**, *, ***, \\=`, ~~), the closing delimiter
has the same length as the prefix.  For double-backtick (`` ), the
closing is 3 characters ( ``).  For links and images, the closing
is ](url)."
  (pcase prefix
    ((or "***" "**" "*" "`" "~~")
     (substring suffix 0 (min (length prefix) (length suffix))))
    ("`` "
     (substring suffix 0 (min 3 (length suffix))))
    ((or "[" "![")
     (if (string-match "\\`](\\([^)]*\\))" suffix)
         (match-string 0 suffix)
       suffix))
    (_ suffix)))

(defun markdown-table-wrap--stripped-marker-overhead (prefix close-delim inner)
  "Return visible width of PREFIX + CLOSE-DELIM when markup is hidden.
In strip-markup mode, `markdown-hide-markup' hides most delimiters.

Exceptions where markers remain visible:
- Bold-italic (***): the italic * markers stay visible → overhead 2.
- Italic (*) wrapping bold (**): when INNER contains bold spans,
  continuation lines can start/end with **, combining with the
  italic * to form *** — which keeps the italic * visible → overhead 2.

CLOSE-DELIM is the closing delimiter (without trailing characters).
INNER is the text that will be wrapped inside the markers."
  (ignore close-delim)
  (pcase prefix
    ("***" 2)
    ;; Italic wrapping text that contains bold spans:
    ;; * + **text** + * = ***text*** — italic markers stay visible.
    ("*" (if (string-match-p "\\*\\*" inner) 2 0))
    (_ 0)))

(defun markdown-table-wrap--should-degrade-markup-p (prefix inner-width marker-overhead)
  "Return non-nil if markup should be degraded to plain text.
PREFIX identifies the markup type.  INNER-WIDTH is the available
width for content after subtracting marker and trailing overhead.
MARKER-OVERHEAD is the visible width of the markup delimiters.

For symmetric markup (bold, italic, code, strikethrough, bold-italic),
degrade when markers consume more than half of each line.
For links/images, degrade only when there is no room for content at all
\(their marker overhead includes the URL, which is much larger)."
  (if (member prefix '("[" "!["))
      (< inner-width 1)
    (< inner-width marker-overhead)))

(defun markdown-table-wrap--force-break-markup (parts width)
  "Force-break a markup span described by PARTS to fit WIDTH.
PARTS is (PREFIX INNER SUFFIX) as returned by
`markdown-table-wrap--markup-span-parts'.
Return a list of lines.  Non-last lines get only the closing
delimiter; the last line gets the full suffix (including any
trailing punctuation like commas or semicolons).

When the inner content width is less than the marker overhead,
more than half of every line would be markup ceremony.  In that
case, the markers are dropped and the inner text is wrapped as
plain text — preserving legibility over formatting.  Trailing
punctuation (commas, parens, semicolons) survives the degradation."
  (pcase-let ((`(,prefix ,inner ,suffix) parts))
    (let* ((close-delim (markdown-table-wrap--closing-delimiter
                         prefix suffix))
           (trailing (substring suffix (length close-delim)))
           ;; Marker overhead: visible width of prefix + closing delimiter.
           ;; In strip-markup mode, most delimiters are hidden by
           ;; markdown-hide-markup.  Bold-italic (***) is the exception:
           ;; ** is hidden but * remains visible (2 chars overhead).
           (marker-overhead
            (if markdown-table-wrap--strip-markup
                (markdown-table-wrap--stripped-marker-overhead
                 prefix close-delim inner)
              (+ (length prefix)
                 (markdown-table-wrap--display-width close-delim))))
           ;; Trailing characters (commas, semicolons, parens) are always
           ;; visible and appear only on the last line.  Account for them
           ;; in the inner width so the last line fits.
           (trailing-overhead
            (if (string-empty-p trailing) 0
              (markdown-table-wrap--display-width trailing)))
           (inner-width (- width marker-overhead trailing-overhead)))
      (if (markdown-table-wrap--should-degrade-markup-p
           prefix inner-width marker-overhead)
          ;; Markers dominate — drop them, wrap inner text as plain.
          ;; For links/images, the URL is discarded (prevents ](url)
          ;; fragments from triggering catastrophic regex backtracking
          ;; in markdown-mode's font-lock).
          ;; Use wrap-cell (not force-break-plain) so multi-word inner
          ;; text gets proper word-boundary wrapping.  Include trailing
          ;; punctuation as part of the text so it wraps naturally
          ;; within width — no risk of overflow from appending.
          (markdown-table-wrap-cell (concat inner trailing) width)
        (let* ((inner-lines (markdown-table-wrap-cell inner inner-width))
               (n (length inner-lines)))
          (cl-loop for il in inner-lines
                   for i from 1
                   collect (if (= i n)
                               (concat prefix il suffix)
                             (concat prefix il close-delim))))))))

(defun markdown-table-wrap--force-break-plain (word width)
  "Force-break plain WORD at WIDTH boundaries.
Return a list of chunks, each fitting within WIDTH visible characters.

This function receives only non-markup tokens from the tokenizer
\(i.e., tokens for which `markdown-table-wrap--markup-span-parts'
returned nil).  Strip-markup has no effect on non-markup text.

Two execution paths:
1. All characters are single-byte (ASCII): substring at exact
   WIDTH boundaries, zero measurement.
2. Contains multibyte characters (CJK, accented, emoji):
   jump-and-adjust with `display-width-from-to' for CJK, or
   `display-width' on substrings when VS16 emoji are present."
  (let ((pos 0)
        (len (length word))
        (chunks nil))
    (if (= (length word) (string-bytes word))
        ;; Fast path: all characters are single-byte ASCII.
        ;; Character position equals display position, so we can
        ;; substring at exact WIDTH boundaries with no measurement.
        (while (< pos len)
          (let ((chunk-end (min (+ pos width) len)))
            (push (substring word pos chunk-end) chunks)
            (setq pos chunk-end)))
      ;; Slow path: contains multibyte characters (CJK, etc.).
      ;; Jump to estimated position, then adjust ±1 character.
      ;; Use `display-width-from-to' to avoid both substring allocation
      ;; and GUI auto-composition overhead.  When VS16 emoji (U+FE0F)
      ;; are present, fall back to `display-width' on substrings for
      ;; correct VS16 width correction — this case is extremely rare.
      (let ((has-vs16 (string-search "\xFE0F" word)))
        (while (< pos len)
          (let* ((est (min (+ pos width) len))
                 (chunk-end est)
                 (chunk-vis (if has-vs16
                                (markdown-table-wrap--display-width
                                 (substring word pos chunk-end))
                              (markdown-table-wrap--display-width-from-to
                               word pos chunk-end))))
            ;; Shrink if overshot
            (while (and (> chunk-vis width) (> chunk-end (1+ pos)))
              (setq chunk-end (1- chunk-end))
              (setq chunk-vis (if has-vs16
                                  (markdown-table-wrap--display-width
                                   (substring word pos chunk-end))
                                (markdown-table-wrap--display-width-from-to
                                 word pos chunk-end))))
            ;; Grow if room remains
            (while (and (< chunk-end len)
                        (<= (if has-vs16
                                 (markdown-table-wrap--display-width
                                  (substring word pos (1+ chunk-end)))
                               (markdown-table-wrap--display-width-from-to
                                word pos (1+ chunk-end)))
                            width))
              (setq chunk-end (1+ chunk-end)))
            ;; Ensure progress
            (when (= chunk-end pos)
              (setq chunk-end (1+ chunk-end)))
            (push (substring word pos chunk-end) chunks)
            (setq pos chunk-end)))))
    (nreverse chunks)))

;;;; Cell Wrapping

(defun markdown-table-wrap-cell (text width)
  "Word-wrap TEXT to fit within WIDTH characters.
Return a list of lines.  Split on whitespace boundaries, keeping
markdown markup spans (links, bold, italic, code, strikethrough)
intact as single tokens.  Long unbroken tokens are force-broken
at WIDTH.

Width measurement uses `markdown-table-wrap-visible-width', which
respects the dynamic variable `markdown-table-wrap--strip-markup'.
When called from `markdown-table-wrap', this is bound automatically.
When calling this function directly, bind the variable yourself
if strip-markup behavior is needed.  Raw markup is always preserved
in the output regardless of the strip-markup setting."
  (if (or (<= width 0) (string-empty-p text))
      (list (or text ""))
    (let ((words (markdown-table-wrap--tokenize-cell-text text))
          (lines nil)
          (current-line "")
          (current-width 0))
      (dolist (word words)
        (let ((word-vis-width (markdown-table-wrap-visible-width word)))
          (cond
           ;; Word exceeds column — force-break it
           ((> word-vis-width width)
            (when (> current-width 0)
              (push current-line lines)
              (setq current-line "" current-width 0))
            (let* ((parts (markdown-table-wrap--markup-span-parts word))
                   (fragments (if parts
                                  (markdown-table-wrap--force-break-markup
                                   parts width)
                                (markdown-table-wrap--force-break-plain
                                 word width))))
              (dolist (frag fragments)
                (let ((vis (markdown-table-wrap-visible-width frag)))
                  (unless (string-empty-p current-line)
                    (push current-line lines))
                  (setq current-line frag current-width vis)))))
           ;; First word on current line
           ((= current-width 0)
            (setq current-line word current-width word-vis-width))
           ;; Fits on current line (word + 1 space)
           ((<= (+ current-width 1 word-vis-width) width)
            (setq current-line (concat current-line " " word))
            (setq current-width (+ current-width 1 word-vis-width)))
           ;; Doesn't fit — wrap
           (t
            (push current-line lines)
            (setq current-line word current-width word-vis-width)))))
      (when (> current-width 0)
        (push current-line lines))
      (let ((result (or (nreverse lines) (list ""))))
        ;; Backtick parity guard: if any wrapped line has an odd
        ;; backtick count, markdown-mode's multiline code regex will
        ;; leak across rows.  Fall back to a single (possibly
        ;; overflowing) line to preserve syntax integrity.
        (if (markdown-table-wrap--odd-backtick-line-p result)
            (list text)
          result)))))

;;;; Table Parsing

(defun markdown-table-wrap-parse (text)
  "Parse markdown pipe-table TEXT into structured data.
Return (HEADERS ALIGNS ROWS) where:
  HEADERS is a list of header cell strings (trimmed)
  ALIGNS is a list of alignment symbols: `left', `right', `center', or nil
  ROWS is a list of rows, each a list of cell strings (trimmed)
Escaped pipes (`\\|') in cells are preserved and not treated as
column separators."
  (let* ((lines (split-string text "\n" t))
         (headers nil)
         (aligns nil)
         (rows nil)
         (seen-separator nil))
    (dolist (line lines)
      (let ((trimmed (string-trim line)))
        (cond
         ;; Separator line: |---|:---:|---:|
         ((string-match-p "^|[-:|[:space:]]+|$" trimmed)
          (setq seen-separator t)
          (let ((cells (markdown-table-wrap--split-table-row trimmed)))
            (setq aligns
                  (mapcar (lambda (cell)
                            (let ((c (string-trim cell)))
                              (cond
                               ((and (string-prefix-p ":" c)
                                     (string-suffix-p ":" c)) 'center)
                               ((string-suffix-p ":" c) 'right)
                               ((string-prefix-p ":" c) 'left)
                               (t nil))))
                          cells))))
         ;; Header (before separator)
         ((not seen-separator)
          (setq headers (markdown-table-wrap--split-table-row trimmed)))
         ;; Data rows (after separator)
         (t
          (push (markdown-table-wrap--split-table-row trimmed) rows)))))
    (list headers
          (or aligns (make-list (length headers) nil))
          (nreverse rows))))

;;;; Column Width Computation

(defun markdown-table-wrap-compute-widths (headers rows available-width num-cols)
  "Compute column widths for table with HEADERS and ROWS.
AVAILABLE-WIDTH is the total character width available.
NUM-COLS is the number of columns.
Return a list of column widths (integers).

Column widths are stored internally as vectors for O(1) random
access (cf. `shr-table-widths' in shr.el).  The return value is
a plain list for callers that iterate with `dolist' or `cl-mapcar'.

Shrinking uses a waterfill algorithm with sqrt-dampened weights —
see `markdown-table-wrap-distribute-widths'.

This is a convenience wrapper around `markdown-table-wrap-compute-table-metrics'
and `markdown-table-wrap-distribute-widths'.  When rendering the same table at
multiple widths, call those two functions separately to avoid recomputing
the width-independent metrics."
  (let ((metrics (markdown-table-wrap-compute-table-metrics
                  headers rows num-cols)))
    (markdown-table-wrap-distribute-widths metrics available-width num-cols)))

(defun markdown-table-wrap-compute-table-metrics (headers rows num-cols)
  "Compute width-independent column metrics for HEADERS and ROWS.
NUM-COLS is the number of columns.
Return a plist (:natural-widths VEC) where the vector has NUM-COLS
elements.  Each element is the maximum visible width of any cell
in that column.

These metrics depend only on cell content, not on the target rendering
width, so they can be computed once and reused across multiple calls to
`markdown-table-wrap-distribute-widths'."
  (let ((natural-widths (make-vector num-cols 0)))
    ;; Measure headers
    (cl-loop for cell in headers
             for i from 0 below num-cols do
             (aset natural-widths i
                   (max (aref natural-widths i)
                        (markdown-table-wrap-visible-width cell))))
    ;; Measure data rows
    (dolist (row rows)
      (cl-loop for cell in row
               for i from 0 below num-cols do
               (aset natural-widths i
                     (max (aref natural-widths i)
                          (markdown-table-wrap-visible-width cell)))))
    (list :natural-widths natural-widths)))

(defun markdown-table-wrap-distribute-widths (metrics available-width num-cols)
  "Distribute AVAILABLE-WIDTH across columns using pre-computed METRICS.
METRICS is a plist as returned by `markdown-table-wrap-compute-table-metrics'.
NUM-COLS is the number of columns.
Return a list of column widths (integers).

When all columns fit at their natural width, return those widths
unchanged.  Otherwise use a waterfill algorithm with sqrt-dampened
weights: find the unique level L such that

  sum_i min(nat_i, L * sqrt(nat_i)) = budget

then round via Webster/Sainte-Laguë priorities.  This is
monotonic — widening the terminal never shrinks any column."
  (let* ((natural-widths (plist-get metrics :natural-widths))
         (border-overhead (+ (* 3 num-cols) 1))
         (available (max num-cols (- available-width border-overhead)))
         (total-natural (+ (markdown-table-wrap--vsum natural-widths)
                           border-overhead)))
    (if (<= total-natural available-width)
        ;; Guarantee each column at least 1, matching the shrinking
        ;; path's `max 1' floor below.  An all-empty column has a
        ;; natural width of 0; returning 0 misaligns renderers (the
        ;; separator draws a dash where the row draws nothing), since
        ;; the separator's `max 1' and the row's raw width disagree.
        (mapcar (lambda (w) (max 1 w))
                (append natural-widths nil))
      (let* ((col-w (make-vector num-cols 0))
             ;; Precompute sqrt weights.
             (weights (vconcat
                       (cl-loop for i below num-cols
                                collect (sqrt (max 1.0
                                                   (float (aref natural-widths i)))))))
             ;; Sort column indices by ascending weight (ascending
             ;; nat/weight ratio) so the analytical scan can peel
             ;; off capped columns from the small end first.
             (order (sort (number-sequence 0 (1- num-cols))
                          (lambda (a b)
                            (< (aref weights a) (aref weights b)))))
             ;; Find the continuous waterfill level.
             (rem-budget (float available))
             (rem-weight (cl-loop for i below num-cols
                                  sum (aref weights i)))
             (level 1.0e10))
        (dolist (i order)
          (let ((candidate (/ rem-budget (max 1.0e-9 rem-weight))))
            (cond
             ((<= candidate 0.0)
              (setq level 0.0))
             ((>= (* candidate (aref weights i))
                   (aref natural-widths i))
              ;; Column i is capped at natural — remove from pool.
              (setq rem-budget (- rem-budget (float (aref natural-widths i))))
              (setq rem-weight (- rem-weight (aref weights i))))
             (t
              ;; Found the level; all remaining columns are uncapped.
              (setq level candidate)))))
        ;; Continuous allocations → floor, respecting bounds [1, nat].
        (dotimes (i num-cols)
          (aset col-w i
                (min (aref natural-widths i)
                     (max 1 (floor (* level (aref weights i)))))))
        ;; Distribute rounding remainder via Webster priorities:
        ;; priority_i = weight_i / (2 * current_i + 1).
        (let ((diff (- available (markdown-table-wrap--vsum col-w))))
          (while (> diff 0)
            (let ((best-i -1) (best-pri -1.0))
              (dotimes (i num-cols)
                (when (< (aref col-w i) (aref natural-widths i))
                  (let ((pri (/ (aref weights i)
                                (1+ (* 2.0 (float (aref col-w i)))))))
                    (when (> pri best-pri)
                      (setq best-i i best-pri pri)))))
              (if (>= best-i 0)
                  (progn (aset col-w best-i (1+ (aref col-w best-i)))
                         (setq diff (1- diff)))
                (setq diff 0)))))
        (append col-w nil)))))

;;;; Code Fence Detection

(defun markdown-table-wrap-inside-code-fence-p (pos)
  "Return non-nil if POS is inside a fenced code block.
Count opening/closing fence lines (``` or ~~~, optionally with
a language specifier) from `point-min' to POS.  An odd count
means POS is inside a fenced block.  Does not depend on font-lock
properties, so it is safe to call before `font-lock-ensure'."
  (let ((fence-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[[:blank:]]*\\(```\\|~~~\\)" pos t)
        (setq fence-count (1+ fence-count))))
    (cl-oddp fence-count)))

;;;; Table Unwrapping

(defun markdown-table-wrap--merge-visual-rows (visual-rows)
  "Group VISUAL-ROWS into logical rows using the subset heuristic.
Each visual row is a list of cell strings.  Return a list of logical
rows, where each logical row is a list of merged cell strings (one
per column).

The heuristic: within a logical row, the set of non-empty columns
can only shrink (columns finish and go empty).  When a previously-empty
column reappears with content, a new logical row has started."
  (let ((logical-rows nil)
        (current nil)  ; vector of string-lists, one per column
        (num-cols (length (car visual-rows)))
        (prev-non-empty nil))
    (dolist (vr visual-rows)
      (let ((non-empty
             (cl-loop for c in vr for i from 0
                      when (not (string-empty-p (string-trim c)))
                      collect i)))
        ;; Subset check: is every non-empty column in this row also
        ;; in the previous row's non-empty set?
        (let ((is-continuation
               (and prev-non-empty
                    (cl-every (lambda (i) (memq i prev-non-empty))
                              non-empty))))
          (if is-continuation
              ;; Append non-empty cells to current logical row
              (cl-loop for c in vr for i from 0 do
                (let ((trimmed (string-trim c)))
                  (unless (string-empty-p trimmed)
                    (aset current i
                          (if (aref current i)
                              (concat (aref current i) " " trimmed)
                            trimmed)))))
            ;; Flush and start new logical row
            (when current
              (push (append current nil) logical-rows))
            (setq current (make-vector num-cols nil))
            (cl-loop for c in vr for i from 0 do
              (let ((trimmed (string-trim c)))
                (unless (string-empty-p trimmed)
                  (aset current i trimmed))))))
        (setq prev-non-empty non-empty)))
    ;; Flush last logical row
    (when current
      (push (append current nil) logical-rows))
    ;; Convert nil cells to empty strings
    (mapcar (lambda (row)
              (mapcar (lambda (cell) (or cell "")) row))
            (nreverse logical-rows))))

(defun markdown-table-wrap--render-separator-minimal (aligns)
  "Build a minimal separator line for ALIGNS.
Each alignment gets a minimal dash cell: `---', `:---', `---:', or `:---:'."
  (let ((cells
         (mapcar (lambda (align)
                   (pcase align
                     ('left ":---")
                     ('right "---:")
                     ('center ":---:")
                     (_ "---")))
                 aligns)))
    (concat "| " (mapconcat #'identity cells " | ") " |")))

(defun markdown-table-wrap-unwrap (text)
  "Merge continuation rows in wrapped pipe-table TEXT.
Return a pipe-table string with one visual row per logical row.
Alignment markers are preserved.  Cell text from continuation rows
is joined with spaces.

The boundary-detection heuristic: within a logical row, the set of
non-empty columns can only shrink.  When a previously-empty column
reappears with content, a new logical row has started.

Known limitation: when ALL columns wrap to the exact same height,
no column ever goes empty, and the heuristic cannot detect row
boundaries.  The entire table is treated as one logical row.  For
pixel-perfect fidelity, store the original table.

This function is idempotent: unwrapping an already-unwrapped table
returns it unchanged.  It is composable with `markdown-table-wrap':
  (markdown-table-wrap (markdown-table-wrap-unwrap wrapped) new-width)
is the correct resize pipeline."
  ;; Use the parser for alignment extraction (single source of truth
  ;; for separator detection and alignment parsing).  For row data,
  ;; we scan lines directly because the parser strips all-empty rows
  ;; (row separators) that we need as boundary signals.
  (pcase-let* ((`(,_headers ,aligns ,_rows) (markdown-table-wrap-parse text))
               (lines (split-string text "\n" t))
               (header-vrs nil)
               (data-vrs nil)
               (found-sep nil))
    ;; Phase 1: Classify lines into header visual rows, separator, data.
    (dolist (line lines)
      (let ((trimmed (string-trim line)))
        (cond
         ;; Separator line (same regex the parser uses)
         ((and (not found-sep)
               (string-match-p "^|[-:|[:space:]]+|$" trimmed))
          (setq found-sep t))
         ;; Header rows (before separator)
         ((not found-sep)
          (push (markdown-table-wrap--split-table-row trimmed) header-vrs))
         ;; Data rows (after separator) — including all-empty rows
         (t
          (push (markdown-table-wrap--split-table-row trimmed) data-vrs)))))
    (setq header-vrs (nreverse header-vrs))
    (setq data-vrs (nreverse data-vrs))
    ;; Phase 2: Merge visual rows into logical rows
    (let* ((merged-headers
            (when header-vrs
              (car (markdown-table-wrap--merge-visual-rows header-vrs))))
           (merged-data
            (when data-vrs
              (markdown-table-wrap--merge-visual-rows data-vrs)))
           (render-row
            (lambda (cells)
              (concat "| "
                      (mapconcat #'identity cells " | ")
                      " |"))))
      ;; Render as pipe table
      (mapconcat
       #'identity
       (append
        (when merged-headers
          (list (funcall render-row merged-headers)))
        (when aligns
          (list (markdown-table-wrap--render-separator-minimal aligns)))
        (mapcar render-row merged-data))
       "\n"))))

(defun markdown-table-wrap--render-row (cells col-widths aligns
                                              max-cell-height)
  "Wrap and pad CELLS into pipe-table output lines.
COL-WIDTHS and ALIGNS describe the table geometry.
MAX-CELL-HEIGHT caps cell height (nil means unlimited).
Return a list of formatted pipe-table lines.
CELLS shorter than COL-WIDTHS are padded with empty strings."
  (let* ((num-cols (length col-widths))
         (padded (append cells (make-list (max 0 (- num-cols (length cells)))
                                          "")))
         (wrapped-cells
          (cl-mapcar (lambda (cell w)
                       (markdown-table-wrap--truncate-cell-lines
                        (markdown-table-wrap-cell (or cell "") w)
                        max-cell-height))
                     padded col-widths))
         (max-h (apply #'max (mapcar #'length wrapped-cells))))
    (cl-loop for line-idx below max-h
             collect
             (let ((parts
                    (cl-mapcar
                     (lambda (cell-lines w align)
                       (markdown-table-wrap--pad-cell
                        (or (nth line-idx cell-lines) "") w align))
                     wrapped-cells col-widths aligns)))
               (concat "| " (mapconcat #'identity parts " | ") " |")))))

(defun markdown-table-wrap--render-separator (col-widths aligns)
  "Build the separator line for COL-WIDTHS and ALIGNS."
  (let ((sep-parts
         (cl-mapcar
          (lambda (w align)
            (let ((dashes (make-string (max 1 w) ?-)))
              (pcase align
                ('left (if (>= w 2)
                           (concat ":" (substring dashes 1))
                         ":"))
                ('right (if (>= w 2)
                            (concat (substring dashes 1) ":")
                          ":"))
                ('center (if (>= w 3)
                             (concat ":" (substring dashes 2) ":")
                           (if (>= w 2) "::" ":")))
                (_ dashes))))
          col-widths aligns)))
    (concat "| " (mapconcat #'identity sep-parts " | ") " |")))

;;;; Main Entry Point

(defun markdown-table-wrap (text width &optional max-cell-height strip-markup
                                compact)
  "Rewrite markdown pipe-table TEXT to fit WIDTH characters.
Return pipe-table-shaped text for readable source editing and
round-tripping with `markdown-table-wrap-unwrap'.  When the table
fits naturally, return it aligned (no wrapping needed).  When cells
need wrapping, continuation lines have empty cells for non-wrapping
columns.  Wrapped headers are no longer valid GFM tables, and
wrapped body continuation lines are parsed as additional rows by
Markdown renderers.

MAX-CELL-HEIGHT, when non-nil, caps cell height and adds \"…\" to
truncated cells.

When STRIP-MARKUP is non-nil, measure column widths using visible
text only (stripping **bold**, [links](url), `code`, etc.).  Use
this when the buffer has `markdown-hide-markup' enabled so that
hidden syntax does not count toward width.

When STRIP-MARKUP is nil (the default), measure raw string width.
This is the safe default: columns may be wider than necessary when
markup is hidden, but alignment is always correct.  Passing the
wrong value produces over-padded columns rather than broken
alignment.

When wrapping produces multi-line rows, an empty-cell row is
automatically inserted between each logical data row for visual
breathing room.  This makes row boundaries obvious in dense
wrapped output, but those spacer rows are also parsed as
additional rows by Markdown renderers.  No separator is added when
all rows fit on a single line, since the table is already easy to
read.

When COMPACT is non-nil, suppress these automatic separators.
This produces denser output even when rows wrap.

Return TEXT unchanged when the table is too narrow to render
or has no columns."
  (let ((markdown-table-wrap--strip-markup strip-markup))
    (pcase-let* ((`(,headers ,aligns ,rows) (markdown-table-wrap-parse text))
                 (num-cols (length headers)))
      (cond
       ((= num-cols 0) text)
       ((< (- width (+ (* 3 num-cols) 1)) num-cols) text)
       (t
        (let ((metrics (markdown-table-wrap-compute-table-metrics
                        headers rows num-cols)))
          (markdown-table-wrap--render-table
           headers aligns rows metrics width num-cols
           max-cell-height compact)))))))

(defun markdown-table-wrap--render-table (headers aligns rows metrics
                                                  width num-cols
                                                  max-cell-height
                                                  &optional compact)
  "Render a parsed table at WIDTH using pre-computed METRICS.
HEADERS, ALIGNS, ROWS are from `markdown-table-wrap-parse'.
METRICS is from `markdown-table-wrap-compute-table-metrics'.
NUM-COLS is the number of columns.
MAX-CELL-HEIGHT caps cell height (nil means unlimited).
When COMPACT is nil (the default), insert an empty-cell row between
logical data rows whenever wrapping produces multi-line rows.
When COMPACT is non-nil, suppress separators unconditionally.
Return a rendered pipe-table string."
  (let* ((col-widths (markdown-table-wrap-distribute-widths
                      metrics width num-cols))
         (rendered-rows
          (mapcar (lambda (row)
                    (markdown-table-wrap--render-row
                     row col-widths aligns max-cell-height))
                  rows))
         (needs-separators
          (and (not compact)
               (> (length rendered-rows) 1)
               (cl-some (lambda (lines) (> (length lines) 1))
                        rendered-rows)))
         (data-lines
          (if (not needs-separators)
              (apply #'append rendered-rows)
            ;; Insert empty-cell row between logical data rows.
            (let ((empty-row
                   (concat "| "
                           (mapconcat (lambda (w) (make-string w ?\s))
                                      col-widths " | ")
                           " |"))
                  (result nil))
              (dotimes (i (length rendered-rows))
                (when (> i 0)
                  (push empty-row result))
                (dolist (line (nth i rendered-rows))
                  (push line result)))
              (nreverse result)))))
    (mapconcat
     #'identity
     (append
      (markdown-table-wrap--render-row
       headers col-widths aligns max-cell-height)
      (list (markdown-table-wrap--render-separator col-widths aligns))
      data-lines)
     "\n")))

(defun markdown-table-wrap-batch (text widths &optional max-cell-height
                                       strip-markup compact)
  "Render markdown pipe-table TEXT at each width in WIDTHS.
Return a list of rendered strings, one per width.  Parses the table
and computes width-independent metrics once, then renders at each
width.  This is significantly faster than calling `markdown-table-wrap'
repeatedly when the table content doesn't change (e.g., during window
resize).

MAX-CELL-HEIGHT, STRIP-MARKUP, and COMPACT have the same meaning
as in `markdown-table-wrap'."
  (let ((markdown-table-wrap--strip-markup strip-markup))
    (pcase-let* ((`(,headers ,aligns ,rows) (markdown-table-wrap-parse text))
                 (num-cols (length headers)))
      (if (= num-cols 0)
          (make-list (length widths) text)
        (let ((metrics (markdown-table-wrap-compute-table-metrics
                        headers rows num-cols)))
          (mapcar
           (lambda (width)
             (if (< (- width (+ (* 3 num-cols) 1)) num-cols)
                 text
               (markdown-table-wrap--render-table
                headers aligns rows metrics width num-cols
                max-cell-height compact)))
           widths))))))

(provide 'markdown-table-wrap)
;;; markdown-table-wrap.el ends here
