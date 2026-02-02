;;; editor.el --- Basic editor settings -*- lexical-binding: t; -*-

;; Clean UI
(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; Which-key for discoverability
(which-key-mode 1)
(setq which-key-idle-delay 0.3)

;; Recent files
(recentf-mode 1)
(setq recentf-max-saved-items 50)

;; Save place in files
(save-place-mode 1)

;; Auto-revert files when changed on disk
(global-auto-revert-mode 1)

;; UTF-8 everywhere
(set-default-coding-systems 'utf-8)

;; Smoother scrolling
(setq scroll-margin 3)
(setq scroll-conservatively 101)

;; No backup files cluttering directories
(setq make-backup-files nil)
(setq auto-save-default nil)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Show matching parens
(show-paren-mode 1)

(provide 'editor)
