;; LSP configuration with eglot

(use-package eglot
  :ensure t
  :hook ((js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t) ;; Shutdown server when last buffer closes
  (setq eglot-sync-connect nil) ;; Don't block on startup

  ;; Performance tweaks
  (setq company-idle-delay 0.1) 
  (setq company-minimum-prefix-length 1))

(provide 'lsp)
