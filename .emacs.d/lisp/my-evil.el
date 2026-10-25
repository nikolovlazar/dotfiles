;;; my-evil.el --- Vim editing for text buffers only -*- lexical-binding: t; -*-

;; Evil is an allowlist: vim states in text-editing buffers, plain Emacs
;; everywhere else (Magit, agenda, dired, help, calendar, treemacs, ...).
;; Variables must be set before Evil loads.
(setq evil-want-integration t
      evil-want-keybinding nil       ; no evil-collection-style rebinding
      evil-want-C-u-scroll nil       ; C-u stays universal-argument
      evil-want-C-i-jump nil         ; TAB stays Org's TAB
      evil-disable-insert-state-bindings t ; insert state = Emacs keys
      evil-undo-system 'undo-redo
      evil-default-state 'emacs
      evil-want-Y-yank-to-eol t
      evil-respect-visual-line-mode t ; j/k/0/$ follow wrapped screen lines
      evil-esc-delay 0.01)

(require 'evil)
(evil-mode 1)

;; Hook-enabled `visual-line-mode' (e.g. in Org) must refresh Evil's keymaps,
;; or j/k keep moving by logical lines until the mode is toggled by hand.
(add-hook 'visual-line-mode-hook #'evil-normalize-keymaps)

;; Vim only in buffers where you edit text (org, markdown, and code derive
;; from these). Everything else falls back to `evil-default-state' (emacs).
(dolist (mode '(text-mode prog-mode conf-mode))
  (evil-set-initial-state mode 'normal))

;; Never touch the minibuffer or special buffers.
(setq evil-want-minibuffer nil)

;; Keep Emacs line motion and prefix keys free in normal/motion state.
(dolist (map (list evil-normal-state-map evil-motion-state-map))
  (dolist (key '("C-n" "C-p" "C-t" "C-z"))
    (define-key map (kbd key) nil)))
;; Escape hatch: toggle between Emacs and vim state in the current buffer.
(global-set-key (kbd "C-c v") #'evil-mode)

;; ys / cs / ds surround editing.
(when (require 'evil-surround nil t)
  (global-evil-surround-mode 1))

(provide 'my-evil)
;;; my-evil.el ends here
