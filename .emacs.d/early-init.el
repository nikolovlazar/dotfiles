;;; early-init.el --- Startup environment -*- lexical-binding: t; -*-

;; Initialize packages explicitly in init.el.
(setq package-enable-at-startup nil)

;; Use the installed macOS Command Line Tools for native compilation.
(when (and (eq system-type 'darwin)
           (file-directory-p "/Library/Developer/CommandLineTools"))
  (setenv "DEVELOPER_DIR" "/Library/Developer/CommandLineTools"))

;; Finder and login daemons start with a minimal PATH.
(when (and (eq system-type 'darwin)
           (file-directory-p "/opt/homebrew/bin"))
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH"))))

;; mise shims select the working Node runtime for each project.
(let ((shims (expand-file-name "~/.local/share/mise/shims")))
  (when (file-directory-p shims)
    (add-to-list 'exec-path shims)
    (unless (member shims (split-string (getenv "PATH") path-separator))
      (setenv "PATH" (concat shims path-separator (getenv "PATH"))))))

;;; early-init.el ends here
