;;; my-org-prose.el --- Adaptive visual wrapping for Org prose -*- lexical-binding: t; -*-

(require 'org)
(require 'visual-fill-column)

(defun my/org-prose-setup ()
  "Wrap Org prose visually at 80 columns or the window width."
  (setq-local fill-column 80
              visual-fill-column-width 80
              visual-fill-column-center-text nil)
  (auto-fill-mode -1)
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(add-hook 'org-mode-hook #'my/org-prose-setup)
(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)

(provide 'my-org-prose)
;;; my-org-prose.el ends here
