;;; git.el --- Git integration -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(provide 'git)
