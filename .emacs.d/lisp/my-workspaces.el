;;; my-workspaces.el --- Persistent perspectives -*- lexical-binding: t; -*-

;; Session files contain personal paths and remain ignored local state.
(setq persp-save-dir (expand-file-name "persp-confs/" user-emacs-directory)
      persp-auto-save-opt 2
      persp-auto-resume-time 3.0
      *persp-restrict-buffers-to* 2
      persp-set-frame-buffer-predicate t)
(require 'persp-mode)
(require 'treemacs-persp)

;; Use the package's default C-c p prefix and standard commands.
(unless noninteractive
  (tab-bar-mode -1)
  (persp-mode 1)
  ;; Each perspective owns a Treemacs buffer and its own project workspace.
  (treemacs-set-scope-type 'Perspectives))

(provide 'my-workspaces)
;;; my-workspaces.el ends here
