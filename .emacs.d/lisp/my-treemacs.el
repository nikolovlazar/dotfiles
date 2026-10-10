;;; my-treemacs.el --- Project sidebar -*- lexical-binding: t; -*-

;; Workspace state contains local paths and belongs outside tracked config.
(setq treemacs-persist-file
      (expand-file-name "treemacs/persist" user-emacs-directory)
      treemacs-last-error-persist-file
      (expand-file-name "treemacs/persist-at-last-error" user-emacs-directory)
      treemacs-position 'left
      treemacs-width 32
      treemacs-read-string-input 'from-minibuffer)
(require 'treemacs)

;; Treemacs 3.2 removes nodes in every scope, including unrelated workspaces.
(defun my/treemacs-remove-project-in-current-workspace (original &rest args)
  "Run ORIGINAL with ARGS only in scopes sharing the current workspace."
  (let* ((workspace (treemacs-current-workspace))
         (treemacs--scope-storage
          (cl-remove-if-not
           (lambda (entry)
             (eq workspace (treemacs-scope-shelf->workspace (cdr entry))))
           treemacs--scope-storage)))
    (apply original args)))
(advice-add 'treemacs-do-remove-project-from-workspace :around
            #'my/treemacs-remove-project-in-current-workspace)

;; Theme application can enable minor modes with a numeric argument.
;; Treemacs expects a named indicator setting instead of prompting.
(defun my/treemacs-fringe-mode-args (args)
  (if (and (numberp (car args)) (> (car args) 0))
      '(always)
    args))
(advice-add 'treemacs-fringe-indicator-mode :filter-args
            #'my/treemacs-fringe-mode-args)
(treemacs-follow-mode 1)
(treemacs-filewatch-mode 1)
(when (executable-find "git")
  (treemacs-git-mode 'simple))

(defun my/treemacs-toggle ()
  "Toggle the sidebar, adding the current project when opening it."
  (interactive)
  (if (eq (treemacs-current-visibility) 'visible)
      (treemacs)
    (treemacs-add-and-display-current-project)))

(global-set-key (kbd "C-c b") #'my/treemacs-toggle)
(global-set-key (kbd "C-c B") #'treemacs-find-file)

(provide 'my-treemacs)
;;; my-treemacs.el ends here
