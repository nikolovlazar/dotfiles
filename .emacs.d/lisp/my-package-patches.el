;;; my-package-patches.el --- Local fixes for VC-installed packages -*- lexical-binding: t; -*-

;; Keeps small upstream fixes in patches/<package>.patch next to this config,
;; so a fresh install or an upgrade gets them again.  Applying is idempotent.

(require 'package)

(defconst my/package-patch-directory
  ;; This file is reached through a symlink; the patches live beside its target.
  (expand-file-name "../patches/"
                    (file-name-directory
                     (file-truename (or load-file-name buffer-file-name))))
  "Directory holding one PACKAGE.patch file per patched package.")

(defun my/package-patch--git (directory &rest args)
  "Run git apply with ARGS in DIRECTORY; non-nil if it succeeded."
  (eq 0 (apply #'call-process "git" nil nil nil "-C" directory "apply" args)))

(defun my/apply-package-patch (package)
  "Apply the local patch for PACKAGE, recompiling it if that changed anything."
  (let ((desc (cadr (assq package package-alist)))
        (patch (expand-file-name (format "%s.patch" package)
                                 my/package-patch-directory)))
    (when (and desc (file-exists-p patch))
      (let ((directory (package-desc-dir desc)))
        (cond
         ;; Reversible means it is already in place.
         ((my/package-patch--git directory "--reverse" "--check" patch))
         ((my/package-patch--git directory patch)
          (package-recompile desc)
          (message "Patched %s" package))
         (t
          (display-warning
           'package-patches
           (format "%s no longer applies to %s; upstream changed, so review it"
                   (file-name-nondirectory patch) package))))))))

(provide 'my-package-patches)
;;; my-package-patches.el ends here
