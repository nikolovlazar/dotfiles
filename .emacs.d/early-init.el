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

;;; early-init.el ends here
