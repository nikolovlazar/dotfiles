;;; my-org-prose.el --- Auto-fill Org prose without breaking table source -*- lexical-binding: t; -*-

(require 'org)

(defun my/org-prose-setup ()
  "Wrap Org prose at word boundaries and auto-fill at 80 columns."
  (setq-local fill-column 80)
  ;; Fill as soon as typing crosses column 80, rather than waiting for a space.
  ;; Org's native auto-fill function excludes tables and literal source blocks.
  (setq-local auto-fill-chars (copy-sequence auto-fill-chars))
  (set-char-table-range auto-fill-chars nil t)
  (visual-line-mode 1)
  (auto-fill-mode 1))

(add-hook 'org-mode-hook #'my/org-prose-setup)

(provide 'my-org-prose)
;;; my-org-prose.el ends here
