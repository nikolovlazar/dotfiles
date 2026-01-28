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

(custom-set-variables
 '(package-selected-packages nil))
(custom-set-faces)

;; ===== END ======
