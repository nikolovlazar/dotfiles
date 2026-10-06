;;; my-org-preview.el --- Rich Org display in terminal Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-modern handles native Org syntax.  This companion renders the Markdown
;; quote/callout syntax shared with Neovim, using disposable display overlays.
;; C-c n p toggles all rendering; C-c n t toggles tables independently.

;;; Code:

(require 'org)
(require 'org-modern)
(require 'face-remap)
(require 'markdown-table-wrap-pretty)

(defface my-org-preview-quote
  '((t :inherit org-quote :foreground "#f5c2e7"))
  "Quote text and borders in the Mocha theme."
  :group 'org-faces)

(defconst my-org-preview--settings
  '((org-hide-emphasis-markers . t)
    (org-pretty-entities . t)
    (org-ellipsis . "…")
    (org-modern-star . replace)
    (org-modern-replace-stars . "◉○◈◇✳")
    (org-modern-hide-stars . ?\s)
    ;; Tables use the separate, source-preserving cell preview.
    (org-modern-table . nil)
    ;; Keep timestamp width stable during terminal wrapping and redisplay.
    (org-modern-timestamp . nil)
    ;; Terminal frames have no fringe or pixel-sized rules.
    (org-modern-block-fringe . nil)
    (org-modern-horizontal-rule . "────────────────────")
    (org-modern-list . ((?+ . "◦") (?- . "•") (?* . "•")))
    (org-modern-checkbox . ((?X . "☑") (?- . "▣") (?\s . "☐"))))
  "Buffer-local settings used while the preview is enabled.")

(defvar-local my-org-preview--saved nil)
(defvar-local my-org-preview--faces nil)
(defvar-local my-org-preview--modern-was-on nil)
(defvar-local my-org-preview--visual-line-was-on nil)
(defvar my-org-preview-mode nil)

(defun my-org-preview--overlay (beg end &rest properties)
  "Make a preview overlay at BEG..END with PROPERTIES."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'my-org-preview t)
    (overlay-put overlay 'evaporate t)
    (while properties
      (overlay-put overlay (pop properties) (pop properties)))
    overlay))

(defun my-org-preview--fontify (beg end)
  "Render quote lines near the fontified region BEG..END."
  (when (and my-org-preview-mode (derived-mode-p 'org-mode))
    (save-excursion
      (goto-char beg)
      (setq beg (line-beginning-position))
      (goto-char end)
      (setq end (line-end-position))
      (remove-overlays beg end 'my-org-preview t)
      (goto-char beg)
      (while (re-search-forward "^[ \t]*\\(>\\(?:[ \t]+\\)?\\)" end t)
        (let ((start (match-beginning 0))
              (prefix-start (match-beginning 1))
              (prefix-end (match-end 1))
              (line-end (line-end-position))
              (label "│ "))
          ;; Never interpret quoted examples inside literal Org blocks.
          (unless (memq (org-element-type (org-element-context))
                        '(src-block example-block export-block comment-block
                          fixed-width comment drawer property-drawer))
            (my-org-preview--overlay start line-end
                                     'face 'my-org-preview-quote
                                     'wrap-prefix
                                     (propertize "│ " 'face 'my-org-preview-quote))
            (when (looking-at "\\[!\\([[:alnum:]_-]+\\)\\][ \t]*")
              (setq prefix-end (match-end 0)
                    label (if (equal (upcase (match-string-no-properties 1)) "QUOTE")
                              "│ ❝ "
                            (concat "│ " (match-string-no-properties 1) " "))))
            (my-org-preview--overlay
             prefix-start prefix-end
             'display (propertize label
                                  'face 'my-org-preview-quote))))))
    (my-org-preview--reveal-line)))

(defun my-org-preview--reveal-line ()
  "Reveal quote markup on the current line for ordinary editing."
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (and (overlay-get overlay 'my-org-preview)
                 (or (overlay-get overlay 'display)
                     (overlay-get overlay 'my-org-preview-display)))
        (unless (overlay-get overlay 'my-org-preview-display)
          (overlay-put overlay 'my-org-preview-display
                       (overlay-get overlay 'display)))
        (overlay-put overlay 'display
                     (unless (<= beg (overlay-start overlay) end)
                       (overlay-get overlay 'my-org-preview-display)))))))

(defun my-org-preview--disable ()
  "Remove preview styling before changing the buffer's major mode."
  (when my-org-preview-mode (my-org-preview-mode -1)))

;;;###autoload
(define-minor-mode my-org-preview-mode
  "Toggle rich, source-preserving Org display, including terminal frames.
Native Org syntax is styled by `org-modern-mode'.  Markdown quote lines
get colored borders and callout titles; the current quote line reveals its
markup for editing.  Tables switch to raw source when the preview is off,
and return to readable cells when it is on."
  :lighter " Preview"
  (when (and my-org-preview-mode (not (derived-mode-p 'org-mode)))
    (setq my-org-preview-mode nil)
    (user-error "Org preview requires an Org buffer"))
  (if my-org-preview-mode
      (unless my-org-preview--saved
        (setq my-org-preview--modern-was-on org-modern-mode
              my-org-preview--visual-line-was-on visual-line-mode
              my-org-preview--saved
              (mapcar (lambda (setting)
                        (let ((symbol (car setting)))
                          (list symbol (local-variable-p symbol)
                                (symbol-value symbol))))
                      my-org-preview--settings))
        (dolist (setting my-org-preview--settings)
          (set (make-local-variable (car setting)) (cdr setting)))
        (setq my-org-preview--faces
              (list (face-remap-add-relative 'org-level-1
                                            :foreground "#89b4fa" :weight 'bold)
                    (face-remap-add-relative 'org-level-2 :foreground "#fab387")))
        (visual-line-mode 1)
        (org-modern-mode 1)
        (let ((markdown-table-wrap-pretty-default-on-major-modes '(org-mode)))
          (markdown-table-wrap-pretty-mode 1))
        (jit-lock-register #'my-org-preview--fontify)
        (add-hook 'post-command-hook #'my-org-preview--reveal-line nil t)
        (add-hook 'change-major-mode-hook #'my-org-preview--disable nil t))
    (jit-lock-unregister #'my-org-preview--fontify)
    (remove-hook 'post-command-hook #'my-org-preview--reveal-line t)
    (remove-hook 'change-major-mode-hook #'my-org-preview--disable t)
    (markdown-table-wrap-pretty-mode -1)
    (save-restriction
      (widen)
      (remove-overlays (point-min) (point-max) 'my-org-preview t))
    (unless my-org-preview--modern-was-on (org-modern-mode -1))
    (unless my-org-preview--visual-line-was-on (visual-line-mode -1))
    (mapc #'face-remap-remove-relative my-org-preview--faces)
    (setq my-org-preview--faces nil)
    (dolist (setting my-org-preview--saved)
      (if (nth 1 setting)
          (set (car setting) (nth 2 setting))
        (kill-local-variable (car setting))))
    (setq my-org-preview--saved nil))
  (font-lock-flush))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'my-org-preview-mode)
  (define-key org-mode-map (kbd "C-c n p") #'my-org-preview-mode)
  (define-key org-mode-map (kbd "C-c n t") #'markdown-table-wrap-pretty-toggle))

(provide 'my-org-preview)
;;; my-org-preview.el ends here
