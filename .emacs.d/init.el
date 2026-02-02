;;; init.el --- Minimal Emacs for org-mode -*- lexical-binding: t; -*-

;; Performance: increase GC threshold during startup
(setq gc-cons-threshold (* 50 1024 1024))

;; Silence native-comp warnings
(setq native-comp-async-report-warnings-errors 'silent)

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

;; Load config modules
(load "editor")
(load "ui")
(load "completion")
(load "org-config")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/org/inbox.org" "~/org/gsd.org" "~/org/someday.org"))
 '(package-selected-packages nil))

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

;; Performance: lower GC threshold after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1024 1024))))

;;; init.el ends here
