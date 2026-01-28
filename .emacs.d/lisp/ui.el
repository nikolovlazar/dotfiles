;; UI and theme configuration

;; Catppuccin theme
(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha) ;; Choose 'latte, 'frappe, 'macchiato, or 'mocha
  (load-theme 'catppuccin t))

(defun my/set-transparent-background ()
  "Remove background colors for terminal compatibility."
  (unless (display-graphic-p)
    (set-face-background 'default "none")
    (set-face-background 'line-number "none")
    (set-face-background 'line-number-current-line "none")
    ;; The Mode Line fixes
    (set-face-background 'mode-line "none")
    (set-face-background 'mode-line-inactive "none")
    ))

(add-hook 'after-init-hook #'my/set-transparent-background)

(provide 'ui)
