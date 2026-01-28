;; Completion and fuzzy finding

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
	 ("C-c f" . project-find-file)
	 ("C-c g" . consult-ripgrep)))

(provide 'completion)
