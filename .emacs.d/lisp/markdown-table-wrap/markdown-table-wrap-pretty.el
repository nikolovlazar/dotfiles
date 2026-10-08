;;; markdown-table-wrap-pretty.el --- Display-only pretty rendering of pipe tables -*- lexical-binding: t; -*-

;; Copyright (C) 2026 SayreBlades

;; Author: SayreBlades
;; URL: https://github.com/dnouri/markdown-table-wrap
;; Keywords: convenience, wp, markdown, org, tables

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

;; This is the OPTIONAL display companion to `markdown-table-wrap'.
;; It renders markdown and org pipe tables as a wrapped, box-drawing
;; "pretty" view ON TOP OF the canonical raw table text, and toggles
;; between that pretty view and the raw source in place.  It is the
;; table analogue of inline images or `org-latex-preview':
;;
;;   - The buffer text is always the CANONICAL raw pipe table.  Nothing
;;     is ever rewritten, so export / copy / share / push-to-GitHub /
;;     send-to-LLM always see a valid, single-row-per-record table.
;;   - A display overlay shows a pretty rendering (wrapped to the
;;     window, Unicode box-drawing borders by default).
;;   - One command flips the overlay off to reveal/edit the raw source,
;;     and back on to re-render.
;;
;; Why display-only (not buffer mutation): `markdown-table-wrap' wraps a
;; long cell into multiple `| ... |' rows, but pipe-table syntax has no
;; row-spanning, so any real renderer (cmark-gfm, pandoc, GitHub, org
;; HTML export) sees N independent data rows, not one wrapped cell.
;; Mutating the buffer looks right in Emacs but is semantically broken
;; the moment the text leaves Emacs.  This package keeps the buffer
;; canonical and makes the pretty rendering a disposable display layer.
;;
;; Model (mirrors `org-latex-preview', `C-c C-x C-l'):
;;   - One display overlay per raw line (so `C-n' walks raw lines and big
;;     tables scroll naturally; the toggle is still whole-table).
;;   - Pretty = a view; editing reveals the raw source automatically via
;;     `modification-hooks' that remove the overlays on edit (the same
;;     pattern `org-latex-preview' uses, NOT `read-only', which would
;;     break undo).  Moving out of the table restores its preview;
;;     an explicit toggle can still keep the table raw.
;;   - State = overlay presence (no bookkeeping): "is this table
;;     pretty?" = "does it have `markdown-table-wrap-pretty-display'
;;     overlays?".
;;   - Resize re-renders pretty tables losslessly (regenerate from
;;     canonical; never unwrap).  Tables toggled to raw stay raw.
;;
;; Toggle command (`markdown-table-wrap-pretty-toggle') is point-aware,
;; like `org-latex-preview':
;;   - Point on a table      -> toggle that table.
;;   - Point off-table        -> toggle all tables in the buffer.
;;   - Active region          -> toggle tables overlapping the region.
;;   - `C-u'                  -> force pretty on all tables in buffer.
;;   - `C-u C-u'              -> force raw on all tables in buffer.
;;
;; Detection is universal line-based (`^[ \t]*|' runs, code-fence
;; guarded for markdown flavors, `org-in-src-block-p' for org) — no
;; tree-sitter dependency, so it works in `markdown-mode', `gfm-mode',
;; `md-ts-mode', and `org-mode'.
;;
;; Org: `|---+---|' hlines and width cookies `<[lrc]?[0-9]*>' are
;; stripped for RENDERING only; the raw buffer keeps them.  A trailing
;; `#+TBLFM:' line is outside the table region and stays visible and
;; unchanged while pretty.  This package does NOT refuse `#+TBLFM'
;; tables.
;;
;; Keybindings: this package ships NO default key (it owns no
;; major-mode map).  A consistent cross-mode key is the user's to
;; choose; one free, non-surprising slot in both `markdown-mode' and
;; `org-mode' is `C-c C-x C-k'.  Example:
;;
;;   (require 'markdown-table-wrap-pretty)
;;   (define-key markdown-mode-map (kbd "C-c C-x C-k")
;;               #'markdown-table-wrap-pretty-toggle)
;;   (define-key org-mode-map (kbd "C-c C-x C-k")
;;               #'markdown-table-wrap-pretty-toggle)
;;
;; Depends only on `markdown-table-wrap' (public API: `markdown-table-wrap',
;; `markdown-table-wrap-cell', `markdown-table-wrap-compute-widths',
;; `markdown-table-wrap-parse', `markdown-table-wrap-visible-width', and
;; `markdown-table-wrap-inside-code-fence-p').  There is no `markdown-mode'
;; or `org' dependency at load time; those features are called lazily, so
;; the file loads even when they are absent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'markdown-table-wrap)

(declare-function org-in-src-block-p "org" (&optional inside))

;;;; Customization

(defgroup markdown-table-wrap-pretty nil
  "Display-only pretty rendering of pipe tables."
  :group 'text
  :group 'convenience)

(defcustom markdown-table-wrap-pretty-prettify t
  "Non-nil means render pretty tables with Unicode box-drawing characters.
Nil means use plain markdown pipes (`|')."
  :type 'boolean
  :group 'markdown-table-wrap-pretty)

(defcustom markdown-table-wrap-pretty-auto-rewrap-on-resize t
  "Non-nil means re-render pretty tables when the window is resized.
Re-rendering is lossless (regenerate overlays from canonical source).
Tables toggled to raw stay raw.  Nil disables the resize hook."
  :type 'boolean
  :group 'markdown-table-wrap-pretty)

(defcustom markdown-table-wrap-pretty-default-on-major-modes nil
  "Major modes where tables default to pretty when the minor mode is on.
A list of major-mode symbols.  Default is nil, so tables start raw
everywhere and you toggle on demand.  Add symbols such as
`markdown-mode' or `org-mode' to start tables pretty in those modes."
  :type '(repeat symbol)
  :group 'markdown-table-wrap-pretty)

(defcustom markdown-table-wrap-pretty-rewrap-idle-delay 0.3
  "Seconds to wait after a resize before re-rendering pretty tables.
0 means immediate.  Debounces window drag."
  :type 'number
  :group 'markdown-table-wrap-pretty)

(defface markdown-table-wrap-pretty-code-face
  '((t :inherit fixed-pitch))
  "Face used for inline code spans in pretty tables.
Defaults to `fixed-pitch'.  Customize this face for a different look;
to match a surrounding `markdown-mode' buffer, inherit
`markdown-inline-code-face'.  Applied by
`markdown-table-wrap-pretty-render-inline-spans'."
  :group 'markdown-table-wrap-pretty)

;;;; Buffer-Local State

;; Forward declaration: `markdown-table-wrap-pretty-mode' is defined by
;; `define-minor-mode' below; it is referenced earlier by
;; `markdown-table-wrap-pretty--schedule-refresh' and
;; `markdown-table-wrap-pretty--maybe-default-pretty'.
(defvar markdown-table-wrap-pretty-mode nil)

(defvar-local markdown-table-wrap-pretty--last-width nil
  "Last window width used for table rendering in this buffer.")

(defvar-local markdown-table-wrap-pretty--refresh-timer nil
  "Idle timer for debounced resize re-render in this buffer.")

(defvar-local markdown-table-wrap-pretty--edited-tables nil
  "Marker pairs for edited previews awaiting a move out of the table.")

;;;; Width

(defun markdown-table-wrap-pretty--display-window ()
  "Return a visible window showing the current buffer, or nil."
  (or (get-buffer-window (current-buffer) nil)
      (get-buffer-window (current-buffer) 'visible)))

(defun markdown-table-wrap-pretty--window-width ()
  "Return usable character columns for the current buffer's window.
Fall back to 80 when the buffer has no visible window.  NOTE: this is
the raw window capacity and does NOT account for per-line visual
prefixes such as `line-prefix'/`wrap-prefix' (set by `org-indent-mode').
Use `markdown-table-wrap-pretty--effective-width-at' for the width
actually available at a given buffer position."
  (if-let* ((win (markdown-table-wrap-pretty--display-window)))
      (or (window-max-chars-per-line win) 80)
    80))

(defun markdown-table-wrap-pretty--prefix-display-width (spec)
  "Return the display width of a `line-prefix'/`wrap-prefix' value SPEC.
SPEC may be nil, a string, or a display spec; only the string case is
measured (what `org-indent-mode' sets).  Return 0 otherwise."
  (cond
   ((stringp spec)
    (if (= (length spec) (string-bytes spec))
        (length spec)                 ; ASCII fast path (spaces)
      (let ((w 0) (i 0) (n (length spec)))
        (while (< i n) (setq w (+ w (char-width (aref spec i))) i (1+ i)))
        w)))
   (t 0)))

(defun markdown-table-wrap-pretty--indent-width-at (pos)
  "Return the visual indent width consumed by text properties at POS.
Account for `line-prefix' and `wrap-prefix' (set by `org-indent-mode'
and other indentation minor modes), which eat columns that
`window-max-chars-per-line' does not see."
  (let ((line-pfx (get-text-property pos 'line-prefix))
        (wrap-pfx (get-text-property pos 'wrap-prefix)))
    (+ (markdown-table-wrap-pretty--prefix-display-width line-pfx)
       (markdown-table-wrap-pretty--prefix-display-width wrap-pfx))))

(defun markdown-table-wrap-pretty--effective-width-at (&optional pos)
  "Return the usable table width at POS (default: point).
Window capacity minus any visual indent prefix at POS (e.g. org-indent).
Never return less than 1."
  (max 1 (- (markdown-table-wrap-pretty--window-width)
            (markdown-table-wrap-pretty--indent-width-at (or pos (point))))))

;;;; Detection

(defconst markdown-table-wrap-pretty--table-line-re "\\`[[:blank:]]*|"
  "Regexp matching the start of a pipe-table line.
The leading backtick anchors to the start of a line STRING (used via
`string-match-p'), not to `point-min'.  See
`markdown-table-wrap-pretty--table-line-p'.")

(defun markdown-table-wrap-pretty--table-line-p (&optional line)
  "Return non-nil when LINE (or the current line) is a pipe-table line."
  (if line
      (string-match-p markdown-table-wrap-pretty--table-line-re line)
    ;; Operate on the line STRING so the regex backtick anchors to the
    ;; line start, not `point-min' (which `looking-at-p' would do).
    (save-excursion
      (beginning-of-line)
      (string-match-p
       markdown-table-wrap-pretty--table-line-re
       (buffer-substring-no-properties
        (point) (line-end-position))))))

(defun markdown-table-wrap-pretty--separator-line-p (line)
  "Return non-nil when LINE is a pipe-table separator row.
Accept both GFM (`|---|---|') and org (`|---+---|') separators."
  (let ((trimmed (string-trim line)))
    (and (string-prefix-p "|" trimmed)
         (or (string-suffix-p "|" trimmed)
             (string-suffix-p "|" (substring trimmed 0 -1)))
         (string-match-p "\\`|[-: +]*[-: +|]*|?\\'"
                         (replace-regexp-in-string
                          "[[:blank:]]" "" trimmed)))))

(defun markdown-table-wrap-pretty--inside-code-fence-p (pos)
  "Return non-nil when POS is inside a code block (fence or org src)."
  (cond
   ((derived-mode-p 'org-mode)
    (and (fboundp 'org-in-src-block-p)
         (save-excursion (goto-char pos) (org-in-src-block-p))))
   ((derived-mode-p 'markdown-mode 'md-ts-mode)
    (markdown-table-wrap-inside-code-fence-p pos))
   (t nil)))

(defun markdown-table-wrap-pretty--block-has-separator-p (beg end)
  "Return non-nil when the line block BEG..END has a separator row."
  (save-excursion
    (goto-char beg)
    (while (and (< (point) end)
                (not (markdown-table-wrap-pretty--separator-line-p
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (min end (line-end-position))))))
      (forward-line 1))
    (< (point) end)))

(defun markdown-table-wrap-pretty--table-bounds (&optional pos)
  "Return (BEG . END) for the pipe table at POS, or nil.
END is the start of the first line after the table.  Return nil when
POS is not on a table line, the block has no separator row, or the
block is inside a code fence."
  (save-excursion
    (goto-char (or pos (point)))
    (beginning-of-line)
    (when (markdown-table-wrap-pretty--table-line-p)
      (while (and (not (bobp))
                  (save-excursion
                    (forward-line -1)
                    (markdown-table-wrap-pretty--table-line-p)))
        (forward-line -1))
      (let ((beg (line-beginning-position)))
        (while (and (not (eobp))
                    (markdown-table-wrap-pretty--table-line-p))
          (forward-line 1))
        (let ((end (point)))
          (and (markdown-table-wrap-pretty--block-has-separator-p beg end)
               (not (markdown-table-wrap-pretty--inside-code-fence-p beg))
               (cons beg end)))))))

(defun markdown-table-wrap-pretty--table-regions (beg end)
  "Return pipe-table regions overlapping BEG..END, in buffer order.
Each element is (TABLE-BEG . TABLE-END) as from
`markdown-table-wrap-pretty--table-bounds'."
  (let ((start (min beg end))
        (limit (max beg end))
        (regions nil))
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (while (< (point) limit)
        (let ((bounds (markdown-table-wrap-pretty--table-bounds (point))))
          (if bounds
              (progn
                (when (> (cdr bounds) start)
                  (push bounds regions))
                (goto-char (cdr bounds)))
            (forward-line 1)))))
    (nreverse regions)))

;;;; Parsing

(defun markdown-table-wrap-pretty--normalize-org-line (line)
  "Normalize org table LINE for GFM-style parsing.
Strip org width cookies `<[lrc]?[0-9]*>'.  Convert org hline
separators `|---+---|' to GFM form `|---|---|'."
  (let ((stripped (replace-regexp-in-string "<[lrc]?[0-9]*>" "" line)))
    (if (and (derived-mode-p 'org-mode)
             (markdown-table-wrap-pretty--separator-line-p stripped))
        (replace-regexp-in-string "+" "|" stripped)
      stripped)))

(defun markdown-table-wrap-pretty--parse-table (raw-lines)
  "Parse RAW-LINES into (HEADERS ALIGNS ROWS), or nil.
RAW-LINES is a list of pipe-table line strings (no trailing newlines).
Org separators and width cookies are normalized for parsing only.
Return nil when there is no separator row or no header before it.
Cell splitting and alignment are delegated to the public
`markdown-table-wrap-parse'."
  (let* ((norm (mapcar #'markdown-table-wrap-pretty--normalize-org-line raw-lines))
         (sep-idx (cl-position-if
                   #'markdown-table-wrap-pretty--separator-line-p norm)))
    (when (and sep-idx (> sep-idx 0))
      ;; Body rules are display boundaries, not a second alignment header.
      (markdown-table-wrap-parse
       (mapconcat #'identity
                  (append (cl-subseq norm 0 (1+ sep-idx))
                          (cl-remove-if
                           #'markdown-table-wrap-pretty--separator-line-p
                           (nthcdr (1+ sep-idx) norm)))
                  "\n")))))

;;;; Rendering

(defun markdown-table-wrap-pretty-render-inline-spans (cell)
  "Render markdown inline spans in CELL for display: drop delimiters, add faces.
A display-only transform (the canonical CELL is not mutated).

Spans (longer delimiters folded first):
- Escaped pipe `\\|'                -> `|'   (GFM table-level unescape)
- Image `![alt](url)'               -> `alt' (`link' face + url help-echo)
- Link  `[text](url)'               -> `text' (`link' face + url help-echo)
- Bold-italic `***x***'             -> `x'   (`bold-italic' face)
- Bold `**x**'                      -> `x'   (`bold' face)
- Strikethrough `~~x~~'             -> `x'   (strike-through attribute)
- Inline code (`x` or `` x ``)      -> `x'   (code face, see below)
- Italic `*x*'                      -> `x'   (`italic' face)

Escaped pipes unescape FIRST (GFM table syntax; a `\\|' inside a code
span resolves to `|' like cmark-gfm).  Code spans are then extracted to
placeholders so their content is literal — `` `**not bold**` `` renders
as `**not bold**` in code face, not folded.  Text properties survive
`markdown-table-wrap-cell', so faces stay attached across wrapped
continuation lines; width measurement ignores text properties, so
columns size to the rendered text, not the raw markup."
  (let ((result (replace-regexp-in-string "\\\\|" "|" cell)) ; \| -> | (GFM, first)
        (code-spans nil)
        (placeholder-idx 0))
    ;; Phase 1: extract code spans (double-backtick then single) so their
    ;; content is literal.  Placeholders \x00CODE<n>\x00 cannot appear in
    ;; markdown.  Double-backtick: CommonMark trims one leading/trailing space.
    (setq result
          (replace-regexp-in-string
           "``\\([^`]\\|`[^`]\\)+``"
           (lambda (match)
             (let* ((inner (substring match 2 -2))
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
    ;; Phase 2: fold non-code spans with faces.  Image before link (both
    ;; use brackets); bold-italic before bold before italic (longer first).
    (setq result
          (replace-regexp-in-string            ; image ![alt](url)
           "!\\[\\([^]]*\\)\\](\\([^)]*\\))"
           (lambda (m)
             (propertize (or (match-string 1 m) "")
                         'face 'link 'mouse-face 'highlight
                         'help-echo (or (match-string 2 m) "")))
           result t t))
    (setq result
          (replace-regexp-in-string            ; link [text](url)
           "\\[\\([^]]*\\)\\](\\([^)]*\\))"
           (lambda (m)
             (propertize (or (match-string 1 m) "")
                         'face 'link 'mouse-face 'highlight
                         'help-echo (or (match-string 2 m) "")))
           result t t))
    (setq result
          (replace-regexp-in-string            ; bold-italic ***x***
           "\\*\\*\\*\\([^*]+\\)\\*\\*\\*"
           (lambda (m)
             (propertize (match-string 1 m) 'face 'bold-italic))
           result t t))
    (setq result
          (replace-regexp-in-string            ; bold **x**
           "\\*\\*\\([^*]+\\)\\*\\*"
           (lambda (m)
             (propertize (match-string 1 m) 'face 'bold))
           result t t))
    (setq result
          (replace-regexp-in-string            ; strikethrough ~~x~~
           "~~\\([^~]+\\)~~"
           (lambda (m)
             (propertize (match-string 1 m) 'face '(:strike-through t)))
           result t t))
    (setq result
          (replace-regexp-in-string            ; italic *x*
           "\\*\\([^*]+\\)\\*"
           (lambda (m)
             (propertize (match-string 1 m) 'face 'italic))
           result t t))
    ;; Phase 3: restore code spans with the code face.
    (dolist (pair code-spans)
      (setq result
            (replace-regexp-in-string
             (regexp-quote (car pair))
             (propertize (cdr pair) 'face 'markdown-table-wrap-pretty-code-face)
             result t t)))
    result))

(defun markdown-table-wrap-pretty--render-inline-spans-in-cell (cell)
  "Pretty-view wrapper around `markdown-table-wrap-pretty-render-inline-spans'.
Return CELL unchanged when `markdown-table-wrap-pretty-prettify' is nil,
so the raw pipe view shows the canonical markdown verbatim (valid source)."
  (if markdown-table-wrap-pretty-prettify
      (markdown-table-wrap-pretty-render-inline-spans cell)
    cell))

(defun markdown-table-wrap-pretty--render-row-lines (cells col-widths aligns)
  "Render table CELLS into display lines using COL-WIDTHS and ALIGNS.
When `markdown-table-wrap-pretty-prettify' is non-nil, emit Unicode
box-drawing verticals instead of markdown pipes.  Return a list of
display-line strings (one per wrapped visual row)."
  (let* ((num-cols (length col-widths))
         (padded (append cells
                         (make-list (max 0 (- num-cols (length cells))) "")))
         (wrapped-cells
          (cl-mapcar (lambda (cell column-width)
                       (markdown-table-wrap-cell (or cell "") column-width))
                     padded col-widths))
         (max-height (apply #'max (mapcar #'length wrapped-cells)))
         (pretty markdown-table-wrap-pretty-prettify)
         (delim-open  (if pretty "│ " "| "))
         (delim-mid   (if pretty " │ " " | "))
         (delim-close (if pretty " │" " |")))
    (cl-loop for line-index below max-height
             collect
             (let ((acc (list delim-open)))
               (cl-loop for cell-lines in wrapped-cells
                        for column-width in col-widths
                        for align in aligns
                        for first = t then nil
                        do
                        (unless first (push delim-mid acc))
                        (let* ((cell (or (nth line-index cell-lines) ""))
                               (empty (string-empty-p cell))
                               (pad (if empty
                                        column-width
                                      (max 0 (- column-width
                                                (markdown-table-wrap-visible-width cell))))))
                          (cond
                           (empty
                            (push (make-string column-width ?\s) acc))
                           ((eq align 'right)
                            (push (make-string pad ?\s) acc)
                            (push cell acc))
                           ((eq align 'center)
                            (let ((left-pad (/ pad 2)))
                              (push (make-string left-pad ?\s) acc)
                              (push cell acc)
                              (push (make-string (- pad left-pad) ?\s) acc)))
                           (t
                            (push cell acc)
                            (push (make-string pad ?\s) acc)))))
               (push delim-close acc)
               (apply #'concat (nreverse acc))))))

(defun markdown-table-wrap-pretty--render-separator-line (col-widths aligns)
  "Render the separator line for COL-WIDTHS and ALIGNS.
When `markdown-table-wrap-pretty-prettify' is non-nil, emit a
box-drawing rule; otherwise emit standard markdown separator syntax."
  (if markdown-table-wrap-pretty-prettify
      (concat "├─" (mapconcat (lambda (w) (make-string (max 1 w) ?─))
                              col-widths "─┼─")
              "─┤")
    (let ((parts
           (cl-mapcar
            (lambda (column-width align)
              (let ((dashes (make-string (max 1 column-width) ?-)))
                (pcase align
                  ('left
                   (if (>= column-width 2)
                       (concat ":" (substring dashes 1))
                     ":"))
                  ('right
                   (if (>= column-width 2)
                       (concat (substring dashes 1) ":")
                     ":"))
                  ('center
                   (if (>= column-width 3)
                       (concat ":" (substring dashes 2) ":")
                     (if (>= column-width 2) "::" ":")))
                  (_ dashes))))
            col-widths aligns)))
      (concat "| " (mapconcat #'identity parts " | ") " |"))))

;;;; Display Groups (per raw line)

(defun markdown-table-wrap-pretty--split-line-prefix (line)
  "Split table LINE into (PREFIX . BARE).
PREFIX is everything before the first `|'; BARE starts at the first `|'."
  (if-let* ((pipe-index (string-match-p "|" line)))
      (cons (substring line 0 pipe-index)
            (substring line pipe-index))
    (cons "" line)))

(defun markdown-table-wrap-pretty--table-display-groups (raw-lines width)
  "Return prefix-aware display groups for RAW-LINES at WIDTH.
Each result element corresponds to one raw source line and is a list
of the display lines for that logical table row.  Container prefixes
such as blockquotes and indentation are preserved on every visual
continuation line.  Plain tables take a fast path."
  (let* ((no-prefix (cl-every (lambda (l) (and (> (length l) 0)
                                                (= (aref l 0) ?|)))
                               raw-lines))
         (parts (unless no-prefix
                  (mapcar #'markdown-table-wrap-pretty--split-line-prefix raw-lines)))
         (prefixes (unless no-prefix (mapcar #'car parts)))
         (bare-lines (if no-prefix raw-lines (mapcar #'cdr parts)))
         (prefix-width (if no-prefix 0
                          (apply #'max 0 (mapcar #'string-width prefixes)))))
    (when (and (>= (length bare-lines) 2)
               (markdown-table-wrap-pretty--parse-table bare-lines))
      (pcase-let* ((`(,headers ,aligns ,rows)
                   (markdown-table-wrap-pretty--parse-table bare-lines))
                  ;; Fold inline spans (escaped pipes, links/images,
                  ;; bold/italic/strike/code) via
                  ;; `markdown-table-wrap-pretty--render-inline-spans-in-cell'
                  ;; so columns size and render to the visible text rather
                  ;; than the raw markup.  No-op when
                  ;; `markdown-table-wrap-pretty-prettify' is nil.
                  (disp-headers (mapcar #'markdown-table-wrap-pretty--render-inline-spans-in-cell
                                        headers))
                  (disp-rows (mapcar (lambda (r)
                                       (mapcar #'markdown-table-wrap-pretty--render-inline-spans-in-cell r))
                                     rows))
                  (num-cols (max (length disp-headers)
                                 (length aligns)
                                 (if disp-rows
                                     (apply #'max (mapcar #'length disp-rows))
                                   0)))
                  (content-width (max 1 (- width prefix-width)))
                  (col-widths
                   (markdown-table-wrap-compute-widths
                    disp-headers disp-rows content-width num-cols))
                  (header-lines
                   (markdown-table-wrap-pretty--render-row-lines disp-headers col-widths aligns))
                  (separator-line
                   (markdown-table-wrap-pretty--render-separator-line col-widths aligns))
                  (row-groups
                   (mapcar (lambda (row)
                             (markdown-table-wrap-pretty--render-row-lines
                              row col-widths aligns))
                           disp-rows)))
        ;; Keep one display group per source line, including body hlines.
        ;; Otherwise later rows shift onto the rule's overlay and disappear.
        (cl-loop for line in bare-lines
                 for index from 0
                 for prefix = (if no-prefix "" (nth index prefixes))
                 for group = (cond
                              ((= index 0) header-lines)
                              ((markdown-table-wrap-pretty--separator-line-p line)
                               (list separator-line))
                              (t (pop row-groups)))
                 collect (mapcar (lambda (l) (concat prefix l)) group))))))

;;;; Overlay Management

(defun markdown-table-wrap-pretty--overlays-in (beg end)
  "Return the `markdown-table-wrap-pretty-display' overlays in BEG..END."
  (cl-remove-if-not
   (lambda (ov) (overlay-get ov 'markdown-table-wrap-pretty-display))
   (overlays-in beg end)))

(defun markdown-table-wrap-pretty--undecorate-table (beg end)
  "Remove all markdown-table-wrap-pretty display overlays in BEG..END."
  (dolist (ov (markdown-table-wrap-pretty--overlays-in beg end))
    (delete-overlay ov)))

(defun markdown-table-wrap-pretty--table-pretty-p (beg end)
  "Return non-nil when the table at BEG..END is currently pretty."
  (cl-some (lambda (ov) (overlay-get ov 'markdown-table-wrap-pretty-display))
           (overlays-in beg end)))

(defun markdown-table-wrap-pretty--on-modify (ov _flag _beg _end &optional _length)
  "Modification hook: reveal the whole table when OV's text is edited.
This auto-removes the pretty display on any edit (org-latex-preview
style), so the user sees and edits the raw source directly.  There is no
`read-only' (which would break undo) and no blind editing.  The other
arguments are the standard modification-hook arguments and are ignored."
  (let ((inhibit-modification-hooks t))
    (when-let* ((markers (overlay-get ov 'markdown-table-wrap-pretty-markers))
                (mb (marker-position (car markers)))
                (me (marker-position (cdr markers))))
      ;; Drop the markers first so a re-entrant call is a no-op.
      (overlay-put ov 'markdown-table-wrap-pretty-markers nil)
      (cl-pushnew markers markdown-table-wrap-pretty--edited-tables :test #'eq)
      (markdown-table-wrap-pretty--undecorate-table mb me))))

(defun markdown-table-wrap-pretty--restore-edited-tables ()
  "Restore edited previews after point leaves their table.
Keep the source visible while editing.  Markers track changes without
rewriting text or changing the buffer's modified state."
  (setq markdown-table-wrap-pretty--edited-tables
        (cl-delete-if
         (lambda (markers)
           (let ((beg (marker-position (car markers)))
                 (end (marker-position (cdr markers))))
             (when (or (not beg) (not end)
                       (< (point) beg) (>= (point) end))
               (when (and beg end)
                 (save-excursion
                   (when-let* ((bounds (markdown-table-wrap-pretty--table-bounds beg)))
                     (markdown-table-wrap-pretty--decorate-table
                      (car bounds) (cdr bounds)))))
               (set-marker (car markers) nil)
               (set-marker (cdr markers) nil)
               t)))
         markdown-table-wrap-pretty--edited-tables)))

(defun markdown-table-wrap-pretty--forget-edited-tables (beg end)
  "Forget pending previews overlapping BEG..END after an explicit toggle."
  (setq markdown-table-wrap-pretty--edited-tables
        (cl-delete-if
         (lambda (markers)
           (when (and (marker-position (car markers))
                      (< (marker-position (car markers)) end)
                      (> (marker-position (cdr markers)) beg))
             (set-marker (car markers) nil)
             (set-marker (cdr markers) nil)
             t))
         markdown-table-wrap-pretty--edited-tables)))

(defun markdown-table-wrap-pretty--decorate-table (beg end &optional width)
  "Create per-line display overlays for the raw table BEG..END.
WIDTH defaults to the effective width at BEG (window capacity minus any
visual indent prefix from `org-indent-mode' and friends).  The raw
buffer text is preserved; each raw line gets its own overlay whose
`display' shows the wrapped/pretty output.  Original line terminators
stay outside the overlays so Org folds fully cover each replacement.
Return t when overlays were
created, nil when the table needs no change (fits, no prettify)."
  (let* ((table-beg (save-excursion (goto-char beg) (line-beginning-position)))
         (width (or width (markdown-table-wrap-pretty--effective-width-at table-beg)))
         (raw (buffer-substring-no-properties table-beg end))
         (trimmed (string-trim-right raw "\n+")))
    (when-let* ((raw-lines (split-string trimmed "\n"))
                (groups (markdown-table-wrap-pretty--table-display-groups raw-lines width)))
      ;; Skip when the display is identical to the raw (fits, no prettify).
      (unless (and (= (length groups) (length raw-lines))
                   (cl-every (lambda (raw-line group)
                               (and (= (length group) 1)
                                    (equal raw-line (car group))))
                             raw-lines groups))
        (let ((n (length raw-lines))
              (beg-marker (make-marker))
              (end-marker (make-marker)))
          (set-marker beg-marker table-beg)
          (set-marker end-marker end)
          (save-excursion
            (goto-char table-beg)
            (dotimes (i n)
              (let* ((line-beg (line-beginning-position))
                     (line-end (min (line-end-position) end))
                     (group (nth i groups))
                     (display-str (mapconcat #'identity group "\n"))
                     (ov (make-overlay line-beg line-end nil nil nil)))
                (overlay-put ov 'display display-str)
                (overlay-put ov 'face 'default)
                (overlay-put ov 'markdown-table-wrap-pretty-display t)
                (overlay-put ov 'markdown-table-wrap-pretty-markers
                             (cons beg-marker end-marker))
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'modification-hooks
                             (list #'markdown-table-wrap-pretty--on-modify))
                (overlay-put ov 'insert-in-front-hooks
                             (list #'markdown-table-wrap-pretty--on-modify))
                (overlay-put ov 'insert-behind-hooks
                             (list #'markdown-table-wrap-pretty--on-modify)))
              (forward-line 1)))
          t)))))

(defun markdown-table-wrap-pretty--decorate-tables-in-region (beg end &optional width)
  "Decorate all tables in BEG..END.
WIDTH, when non-nil, overrides the per-table effective width; otherwise
each table uses the effective width at its own position (window capacity
minus any visual indent prefix).  Idempotent: existing table overlays
are removed first."
  (let ((gc-cons-threshold (max gc-cons-threshold (* 8 1024 1024))))
    (setq markdown-table-wrap-pretty--last-width
          (markdown-table-wrap-pretty--window-width))
    (dolist (region (markdown-table-wrap-pretty--table-regions beg end))
      (markdown-table-wrap-pretty--undecorate-table (car region) (cdr region))
      (let* ((rb (car region))
             (re (cdr region))
             (raw (buffer-substring-no-properties rb re)))
        (when (markdown-table-wrap-pretty--parse-table
               (split-string (string-trim-right raw "\n+") "\n"))
          (pcase-let* ((`(,_headers ,_aligns ,rows)
                       (markdown-table-wrap-pretty--parse-table
                        (split-string (string-trim-right raw "\n+") "\n"))))
            (when rows                    ; skip header-only tables
              (markdown-table-wrap-pretty--decorate-table rb re width))))))))

;;;; Resize

(defun markdown-table-wrap-pretty--refresh-visible ()
  "Re-render all currently-pretty tables at the current window width.
Tables toggled to raw (no overlays) are left alone.  Lossless:
regenerate from canonical source."
  (let ((width (markdown-table-wrap-pretty--window-width)))
    (unless (equal width markdown-table-wrap-pretty--last-width)
      (setq markdown-table-wrap-pretty--last-width width)
      (let ((gc-cons-threshold (max gc-cons-threshold (* 8 1024 1024))))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((bounds (markdown-table-wrap-pretty--table-bounds (point))))
              (if bounds
                  (progn
                    (when (markdown-table-wrap-pretty--table-pretty-p
                           (car bounds) (cdr bounds))
                      (markdown-table-wrap-pretty--undecorate-table
                       (car bounds) (cdr bounds))
                      ;; Let decorate compute the effective width at this
                      ;; table's position (accounts for org-indent prefix).
                      (markdown-table-wrap-pretty--decorate-table
                       (car bounds) (cdr bounds)))
                    (goto-char (cdr bounds)))
                (forward-line 1)))))))))

(defun markdown-table-wrap-pretty--schedule-refresh ()
  "Schedule a debounced `markdown-table-wrap-pretty--refresh-visible'."
  (when (and markdown-table-wrap-pretty-mode
             markdown-table-wrap-pretty-auto-rewrap-on-resize)
    (when markdown-table-wrap-pretty--refresh-timer
      (cancel-timer markdown-table-wrap-pretty--refresh-timer))
    (setq markdown-table-wrap-pretty--refresh-timer
          (run-with-idle-timer markdown-table-wrap-pretty-rewrap-idle-delay nil
                               (lambda ()
                                 (when (buffer-live-p (current-buffer))
                                   (with-current-buffer (current-buffer)
                                     (markdown-table-wrap-pretty--refresh-visible))))))))

;;;; Minor Mode

(defun markdown-table-wrap-pretty--maybe-default-pretty ()
  "Decorate all tables if the current major mode defaults to pretty."
  (when (and markdown-table-wrap-pretty-mode
             (memq major-mode markdown-table-wrap-pretty-default-on-major-modes))
    (markdown-table-wrap-pretty--decorate-tables-in-region (point-min) (point-max))))

;;;###autoload
(define-minor-mode markdown-table-wrap-pretty-mode
  "Toggle display-only pretty rendering of pipe tables in this buffer.
When on, tables can be toggled between a pretty view and the canonical
raw pipe text with `markdown-table-wrap-pretty-toggle'.  Resize
re-renders pretty tables losslessly when
`markdown-table-wrap-pretty-auto-rewrap-on-resize' is on.  Tables
start raw unless the major mode is in
`markdown-table-wrap-pretty-default-on-major-modes'.  Editing a pretty
table auto-reveals the raw source via `modification-hooks' (the
`org-latex-preview' pattern; not `read-only', which would break undo).
Moving out of an edited table restores its preview."
  :lighter " TblPretty"
  :group 'markdown-table-wrap-pretty
  (if markdown-table-wrap-pretty-mode
      (progn
        ;; `window-configuration-change-hook' is a normal (no-arg) hook
        ;; and is buffer-local-correct; it fires on window size changes
        ;; as well as layout changes, which is what we need for re-wrap.
        ;; NOTE: do NOT use `window-size-change-functions' here: it is an
        ;; abnormal hook called with one arg (the frame), so a no-arg
        ;; function would error (silently, via `safe-run-hooks'); and it
        ;; is global, so a buffer-local registration is unreliable.
        (add-hook 'window-configuration-change-hook
                  #'markdown-table-wrap-pretty--schedule-refresh nil t)
        (add-hook 'post-command-hook
                  #'markdown-table-wrap-pretty--restore-edited-tables nil t)
        (markdown-table-wrap-pretty--maybe-default-pretty))
    (remove-hook 'window-configuration-change-hook
                 #'markdown-table-wrap-pretty--schedule-refresh t)
    (remove-hook 'post-command-hook
                 #'markdown-table-wrap-pretty--restore-edited-tables t)
    (markdown-table-wrap-pretty--forget-edited-tables (point-min) (point-max))
    (when markdown-table-wrap-pretty--refresh-timer
      (cancel-timer markdown-table-wrap-pretty--refresh-timer)
      (setq markdown-table-wrap-pretty--refresh-timer nil))
    (markdown-table-wrap-pretty--undecorate-table (point-min) (point-max))))

;;;; Public Commands

(defun markdown-table-wrap-pretty--force (state regions)
  "Apply STATE (`pretty' or `raw') to REGIONS.
Return the number of tables actually rendered (for `pretty') or
processed (for `raw').  A `pretty' table that already fits without
prettification (`markdown-table-wrap-pretty-prettify' is nil and the
table is narrower than the window) is not rendered and is not counted,
so the returned count reflects what the user actually sees change."
  (let ((count 0))
    (dolist (r regions)
      (let ((beg (car r)) (end (cdr r)))
        (markdown-table-wrap-pretty--forget-edited-tables beg end)
        (if (eq state 'pretty)
            (progn
              (markdown-table-wrap-pretty--undecorate-table beg end)
              ;; Let decorate compute the effective width at beg.
              (when (markdown-table-wrap-pretty--decorate-table beg end)
                (setq count (1+ count))))
          (markdown-table-wrap-pretty--undecorate-table beg end)
          (setq count (1+ count)))))
    count))

;;;###autoload
(defun markdown-table-wrap-pretty-toggle (&optional arg)
  "Toggle the pretty rendering of the pipe table at point, or all tables.
Mirrors `org-latex-preview'.  ARG selects the force behavior: a single
prefix arg forces pretty on every table, a double prefix arg forces
raw; otherwise the location rules below decide:
  - Point on a table  -> toggle that table.
  - Point off-table    -> toggle all tables in the buffer (if any are
    pretty, make all raw; otherwise make all pretty).
  - Active region      -> toggle tables overlapping the region."
  (interactive "P")
  (cond
   ;; C-u C-u -> force raw on all.
   ((equal arg '(16))
    (let ((all (markdown-table-wrap-pretty--table-regions
                (point-min) (point-max))))
      (markdown-table-wrap-pretty--force 'raw all)
      (message "Tables raw (%d)" (length all))))
   ;; C-u -> force pretty on all.
   ((equal arg '(4))
    (let* ((all (markdown-table-wrap-pretty--table-regions
                 (point-min) (point-max)))
           (n (markdown-table-wrap-pretty--force 'pretty all)))
      (message "Tables pretty (%d of %d)" n (length all))))
   ;; Active region -> toggle tables overlapping the region.
   ((use-region-p)
    (let* ((rb (region-beginning))
           (re (region-end))
           (regs (markdown-table-wrap-pretty--table-regions rb re))
           (any-pretty (cl-some
                        (lambda (r)
                          (markdown-table-wrap-pretty--table-pretty-p
                           (car r) (cdr r)))
                        regs))
           (state (if any-pretty 'raw 'pretty))
           (n (markdown-table-wrap-pretty--force state regs)))
      (message "Region tables %s (%d%s)"
               (if (eq state 'raw) "raw" "pretty")
               n
               (if (eq state 'raw) "" (format " of %d" (length regs))))))
   ;; No region: point on a table -> toggle it; else toggle all.
   (t
    (let ((bounds (markdown-table-wrap-pretty--table-bounds (point))))
      (when bounds
        (markdown-table-wrap-pretty--forget-edited-tables (car bounds) (cdr bounds)))
      (cond
       ((null bounds)
        (let* ((all (markdown-table-wrap-pretty--table-regions
                     (point-min) (point-max)))
               (any-pretty (cl-some
                            (lambda (r)
                              (markdown-table-wrap-pretty--table-pretty-p
                               (car r) (cdr r)))
                            all))
               (state (if any-pretty 'raw 'pretty))
               (n (markdown-table-wrap-pretty--force state all)))
          (message "Tables %s (%d%s)"
                   (if (eq state 'raw) "raw" "pretty")
                   n
                   (if (eq state 'raw) "" (format " of %d" (length all))))))
       ((markdown-table-wrap-pretty--table-pretty-p (car bounds) (cdr bounds))
        (markdown-table-wrap-pretty--undecorate-table (car bounds) (cdr bounds))
        (message "Table raw"))
       (t
        (markdown-table-wrap-pretty--decorate-table (car bounds) (cdr bounds))
        (message "Table pretty")))))))

;;;###autoload
(defun markdown-table-wrap-pretty-buffer ()
  "Force pretty rendering on all tables in the current buffer."
  (interactive)
  (let* ((all (markdown-table-wrap-pretty--table-regions
               (point-min) (point-max)))
         (n (markdown-table-wrap-pretty--force 'pretty all)))
    (message "Tables pretty (%d of %d)" n (length all))))

;;;###autoload
(defun markdown-table-wrap-pretty-region (beg end)
  "Force pretty rendering on all tables in BEG..END."
  (interactive "r")
  (let* ((regs (markdown-table-wrap-pretty--table-regions beg end))
         (n (markdown-table-wrap-pretty--force 'pretty regs)))
    (message "Region tables pretty (%d of %d)" n (length regs))))

(provide 'markdown-table-wrap-pretty)
;;; markdown-table-wrap-pretty.el ends here
