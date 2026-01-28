;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil) ;; Required for evil-collection
  (setq evil-want-C-u-scroll t) ;; C-u scrolls up (Vim behaviour)
  (setq evil-want-C-i-jump t) ;; C-i for jump forward
  (setq evil-undo-system 'undo-redo) ;; Use emacs 28+ undo system
  (setq evil-echo-state nil)
  ;; Cursor configuration
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-visual-state-cursor 'box)
  (setq evil-replace-state-cursor 'hollow)
  (setq evil-emacs-state-cursor 'box)
  :config
  (evil-mode 1)
  ;; Use visual line motions
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-terminal-cursor-changer
  :ensure t
  :after evil
  :config
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate)))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  ;; Don't let evil-collection override Emacs keybindings we want to keep
  (setq evil-collection-want-unimpaired-p nil) ;; Keep [ and ] available
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'evil-config)
