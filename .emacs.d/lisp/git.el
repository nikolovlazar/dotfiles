;;; git.el --- Git integration -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(setq magit-status-headers-hook
      '(magit-insert-error-header
        magit-insert-diff-filter-header
        magit-insert-head-branch-header
        magit-insert-upstream-branch-header
        magit-insert-push-branch-header
        magit-insert-tags-header
        magit-insert-remote-header))

(provide 'git)
