;;; completion.el --- Minimal completion for org-mode -*- lexical-binding: t; -*-

;; Vertico - vertical completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  ;; Allow spaces in org-roam node names
  (define-key vertico-map (kbd "SPC") #'self-insert-command))

;; Orderless - fuzzy matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Marginalia - show info in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Consult - enhanced search/switch commands
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-s" . consult-line)))

;; Allow spaces in minibuffer completion
(define-key minibuffer-local-completion-map (kbd "SPC") #'self-insert-command)

(provide 'completion)
