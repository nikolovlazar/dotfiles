;;; init.el --- Emacs config entrypoint -*- lexical-binding: t; -*-

;; Performance: increase GC threshold during startup
(setq gc-cons-threshold (* 50 1024 1024))  ; 50MB during init

;; Silence native-comp warnings (functions defined at runtime via macros)
(setq native-comp-async-report-warnings-errors 'silent)

;; Fix for macOS BSD ls (doesn't support GNU --dired flag)
;; Must be set before dired loads
(setq dired-use-ls-dired nil)

;; "C-x p p" opens dired directly
(setq project-switch-commands 'project-dired)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; exec-path-from-shell must load early
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x nil)) ;; 'nil added for terminal/daemon support
    (exec-path-from-shell-initialize)))

;; Load config modules
(load "editor")
(load "ui")
(load "org-config")
(load "dev")
(load "keybindings")
(load "evil-config")
(load "search")
(load "completion")
(load "files")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/org/inbox.org" "~/org/gsd.org" "~/org/someday.org"))
 '(package-selected-packages
   '(ace-window all-the-icons cape catppuccin-theme clipetty consult
		corfu dirvish doom-modeline eldoc-box embark
		evil-collection evil-org evil-terminal-cursor-changer
		exec-path-from-shell git-auto-commit-mode magit
		marginalia nerd-icons-corfu orderless org-journal
		org-modern org-roam osx-dictionary rg vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:height 1.6 :weight bold))))
 '(org-level-1 ((t (:height 1.4 :weight bold))))
 '(org-level-2 ((t (:height 1.3 :weight semi-bold))))
 '(org-level-3 ((t (:height 1.2 :weight semi-bold))))
 '(org-level-4 ((t (:height 1.1))))
 '(org-level-5 ((t (:height 1.0)))))

;; Performance: lower GC threshold after init (but keep it reasonable)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1024 1024))))  ; 8MB after init

;; ===== END ======
