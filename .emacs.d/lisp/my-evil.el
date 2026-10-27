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

;; Evil 1.15 expects the globalized-mode macro to define this; Emacs 31 doesn't.
(defvar evil-mode-buffers nil)

;; Block cursor everywhere except insert state, where it's a bar.
(setq evil-default-cursor '(box)
      evil-normal-state-cursor 'box
      evil-motion-state-cursor 'box
      evil-visual-state-cursor 'box
      evil-replace-state-cursor 'hbar
      evil-operator-state-cursor 'box
      evil-emacs-state-cursor 'box
      evil-insert-state-cursor '(bar . 2))

(require 'evil)
(evil-mode 1)

;; Hook-enabled `visual-line-mode' (e.g. in Org) must refresh Evil's keymaps,
;; or j/k keep moving by logical lines until the mode is toggled by hand.
(add-hook 'visual-line-mode-hook #'evil-normalize-keymaps)

;; Vim only in buffers where you edit text (org, markdown, and code derive
;; from these). Everything else falls back to `evil-default-state' (emacs).
(dolist (mode '(text-mode prog-mode conf-mode))
  (evil-set-initial-state mode 'normal))

;; Emacs has no mode for these, so they open in `fundamental-mode', which
;; isn't derived from the modes above. Give them one that is.
(add-to-list 'auto-mode-alist
             '("/\\.\\(?:git\\|docker\\|npm\\|prettier\\)ignore\\'" . conf-unix-mode))

;; Never touch the minibuffer or special buffers.
(setq evil-want-minibuffer nil)

;; Keep Emacs line motion and prefix keys free in normal/motion state.
(dolist (map (list evil-normal-state-map evil-motion-state-map))
  (dolist (key '("C-n" "C-p" "C-t" "C-z"))
    (define-key map (kbd key) nil)))
;; Escape hatch: toggle between Emacs and vim state in the current buffer.
(global-set-key (kbd "C-c v") #'evil-mode)

;; Visual block works on logical-line columns, so it breaks on wrapped prose.
;; Drop wrapping while a block is selected, then restore it.
(defvar-local my/evil-block-unwrapped nil
  "Non-nil while wrapping is suspended for a visual block selection.")

(defun my/evil-block-unwrap (&rest _)
  "Suspend visual wrapping for the block selection."
  (unless my/evil-block-unwrapped
    (setq my/evil-block-unwrapped
          (list (bound-and-true-p visual-line-mode)
                (bound-and-true-p visual-fill-column-mode)))
    (when (nth 1 my/evil-block-unwrapped) (visual-fill-column-mode -1))
    (when (nth 0 my/evil-block-unwrapped) (visual-line-mode -1))
    (setq-local truncate-lines t)))

(defun my/evil-block-rewrap ()
  "Restore wrapping after the block selection ends."
  (when my/evil-block-unwrapped
    (let ((was my/evil-block-unwrapped))
      (setq my/evil-block-unwrapped nil)
      (kill-local-variable 'truncate-lines)
      (when (nth 0 was) (visual-line-mode 1))
      (when (nth 1 was) (visual-fill-column-mode 1)))))

(advice-add 'evil-visual-block :before #'my/evil-block-unwrap)
(add-hook 'evil-visual-state-exit-hook #'my/evil-block-rewrap)

;; ys / cs / ds surround editing.
(when (require 'evil-surround nil t)
  (global-evil-surround-mode 1))

;;; my-evil.el ends here

;; Terminal frames ignore `cursor-type'; init.el pins them to a steady block
;; via escape sequences, so switch to a steady bar while in insert state.
(defvar my/tty-cursor-shape nil)
(defun my/evil-tty-cursor ()
  "Send a bar cursor in insert state, a block otherwise (terminal only)."
  (unless (display-graphic-p)
    (let ((shape (if (and (bound-and-true-p evil-local-mode)
                          (eq evil-state 'insert))
                     "\e[6 q" "\e[2 q")))
      (unless (equal shape my/tty-cursor-shape)
        (setq my/tty-cursor-shape shape)
        (send-string-to-terminal shape)))))
(add-hook 'post-command-hook #'my/evil-tty-cursor)

(provide 'my-evil)
