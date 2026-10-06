;;; my-org-paths.el --- Configurable Org paths -*- lexical-binding: t; -*-
(require 'org)
(require 'seq)

(defvar my/org-note-file-format "${slug}.org"
  "New note path relative to `org-directory'.")
(defvar my/org-journal-file-format "%Y/%Y-%m-%d.org"
  "Daily file path relative to `org-directory', formatted by `format-time-string'.")
(defvar my/org-journal-quote-file nil
  "Quote collection path, absolute or relative to `org-directory'.")
(defvar my/org-journal-template-file nil
  "Daily template path, absolute or relative to `org-directory'.")
(defvar my/org-reflection-prompts-file nil
  "Prompt collection path, absolute or relative to `org-directory'.")
(defvar my/org-reflection-template-file nil
  "Reflection template path, absolute or relative to `org-directory'.")
(defvar my/org-reflection-directory ""
  "Reflection destination relative to `org-directory'.")
(defvar my/org-excluded-directories nil
  "Support directories to exclude, relative to `org-directory'.")

(defun my/org-excluded-path-p (path)
  "Return non-nil when PATH is hidden, temporary, or in a support directory."
  (let ((relative (file-relative-name path org-directory)))
    (or (seq-some (lambda (part) (string-match-p "\\`[.#]" part))
                  (split-string relative "/" t))
        (seq-some
         (lambda (directory)
           (let ((directory (directory-file-name directory)))
             (or (equal relative directory)
                 (string-prefix-p (concat directory "/") relative))))
         my/org-excluded-directories))))

(provide 'my-org-paths)
;;; my-org-paths.el ends here
