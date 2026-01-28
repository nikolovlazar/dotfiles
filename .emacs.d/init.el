;;; init.el --- Emacs config entrypoint -*- lexical-binding: t; -*-

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/org/inbox.org" "~/org/gsd.org" "~/org/someday.org") t)
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ===== END ======
