;;; editor.el --- Basic editor setings and performance tweaks -*- lexical-binding: t; -*-

;; Terminal support
(unless (display-graphic-p)
  (menu-bar-mode -1)
  (xterm-mouse-mode 1))     ;; Enables mouse clicking/scrolling in Ghostty!

;; Repeat mode allows you to hit C-x or C-c once and the next key multiple times
;; ex, C-x and then multiple times { to make the window narrower
(repeat-mode 1)

;; Performance optimizations
(setq jit-lock-defer-time 0.05) ;; Wait 50ms of idle time before recoloring
(setq-default bidi-display-reordering nil)
(setq bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t) ;; Disable the Bidirectional Parentheses Algorithm
(setq gc-cons-threshold 100000000) ;; 100MB
(setq read-process-output-max (* 1024 1024)) ;; 1mb; helps with lsp/terminal data

;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Disable line numbers in specific modes
(dolist (mode '(org-mode-hook
                org-agenda-mode-hook
                magit-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Use macOS pbcopy to bridge the terminal clipboard gap
  (defun my/copy-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" nil "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
(setq interprogram-cut-function 'my/copy-to-osx)

;; CUA mode
(cua-mode 1)

;; Clippety
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;; Recent files tracking
(recentf-mode 1)
(setq recentf-max-saved-items 100)

(provide 'editor)
