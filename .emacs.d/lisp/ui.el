;;; ui.el --- UI and theme configuration -*- lexical-binding: t; -*-

;; Catppuccin theme
(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha) ;; Choose 'latte, 'frappe, 'macchiato, or 'mocha
  (load-theme 'catppuccin t))

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-modal t)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-modal-modern-icon nil))

;; Set Evil state tags for doom-modeline to use
(with-eval-after-load 'evil
  (setq evil-normal-state-tag   "NORMAL")
  (setq evil-insert-state-tag   "INSERT")
  (setq evil-visual-state-tag   "VISUAL")
  (setq evil-replace-state-tag  "REPLACE")
  (setq evil-operator-state-tag "OPERATOR")
  (setq evil-motion-state-tag   "MOTION")
  (setq evil-emacs-state-tag    "EMACS")
  (setq evil-mode-line-format nil))

;; Requires all-the-icons (run M-x all-the-icons-install-fonts after)
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; Frame padding and window dividers (for clean org-modern appearance)
(when (display-graphic-p)
  (modify-all-frames-parameters
   '((right-divider-width . 20)
     (internal-border-width . 20)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background)))

;; Transparent background for terminal
(defun my/set-transparent-background ()
  "Remove background colors for terminal compatibility."
  (unless (display-graphic-p)
    (set-face-background 'default "none")
    (set-face-background 'line-number "none")
    (set-face-background 'line-number-current-line "none")
    (set-face-background 'mode-line "none")
    (set-face-background 'mode-line-inactive "none")
    ))

(add-hook 'after-init-hook #'my/set-transparent-background)
(add-hook 'after-load-theme-hook #'my/set-transparent-background)

(when (not (display-graphic-p))
  (my/set-transparent-background))

(provide 'ui)
