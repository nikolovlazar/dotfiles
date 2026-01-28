;; Terminal emulator configuration

(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-buffer-name-string "vterm %s"))

(provide 'terminal)
