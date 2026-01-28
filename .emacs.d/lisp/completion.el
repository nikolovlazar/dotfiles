;;; completion.el --- Completion and fuzzy finding -*- lexical-binding: t; -*-

;; Vertico - vertical completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Orderless - fuzzy matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Marginalia - show file info in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Consult - enhanced search commands
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
	 ("C-x C-r" . consult-recent-file)
	 ("C-c f" . project-find-file)))

;;; In-buffer completion (code completion popups)

;; Corfu - completion popup like VSCode
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)              ;; Auto-popup completions
  (corfu-auto-prefix 2)       ;; After 2 characters
  (corfu-auto-delay 0.2)      ;; With slight delay
  (corfu-cycle t)             ;; Cycle with TAB
  (corfu-popupinfo-delay 0)   ;; Show docs immediately
  :bind
  (:map corfu-map
   ("C-y" . corfu-insert))          ;; Select with C-y
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom-face
  (corfu-border ((t (:background "#555555")))))

;; Cape - extra completion sources
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

;; Icons in completion popup
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'completion)
