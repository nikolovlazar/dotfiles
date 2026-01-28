;;; lsp.el --- LSP configuration with eglot -*- lexical-binding: t; -*-

(use-package eglot
  :ensure t
  :hook ((js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :custom
  (eglot-report-progress nil)  ;; Reduce noise
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-sync-connect nil)
  (setq eglot-send-changes-idle-time 0.5))

;; Eldoc - show docs immediately (displayed in corfu-popupinfo)
(use-package eldoc
  :custom
  (eldoc-idle-delay 0)             ;; No delay
  (eldoc-echo-area-use-multiline-p nil))  ;; Keep echo area clean

(provide 'lsp)
