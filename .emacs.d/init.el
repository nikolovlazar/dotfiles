;;; init.el --- Emacs with Org and Org-roam -*- lexical-binding: t; -*-

;; Keep generated preferences and recovery files beside the configuration.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t)
(dolist (directory '("backups" "auto-save"))
  (make-directory (expand-file-name directory user-emacs-directory) t))
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      backup-by-copying t
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

;; Hide the menu bar in terminal frames, including Emacs Client frames.
(defun my/hide-terminal-menu-bar (frame)
  (unless (display-graphic-p frame)
    (set-frame-parameter frame 'menu-bar-lines 0)))
(add-hook 'after-make-frame-functions #'my/hide-terminal-menu-bar)
(mapc #'my/hide-terminal-menu-bar (frame-list))

;; Built-in conveniences.
(which-key-mode 1)
(fido-vertical-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(show-paren-mode 1)

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-selected-packages '(org-roam magit))
(package-initialize)

;; Built-in tinted Modus themes and the theme toggle.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-theme)

;; Existing notes stay in ~/org. Agenda sources exclude templates and backups.
(require 'org)
(require 'org-agenda)

(setq org-directory (expand-file-name "~/org")
      org-default-notes-file (expand-file-name "inbox.org" org-directory)
      org-todo-keywords
      '((sequence "TODO(t)" "PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
      org-log-done 'time
      org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-capture-templates
      `(("i" "Inbox task" entry
         (file ,org-default-notes-file)
         "* TODO %?\n")))

;; Share daily journal files and templates with Neovim.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-org-journal)

;; Use native Org highlighting and folding without custom inline previews.
;; Auto-fill prose at 80 columns without breaking table source.
(require 'my-org-prose)
(require 'my-org-tables)

(defun my/org-refresh-agenda-files ()
  "Discover current agenda files without scanning templates or backups."
  (setq org-agenda-files
        (append (when (file-readable-p org-default-notes-file)
                  (list org-default-notes-file))
                (mapcan
                 (lambda (name)
                   (let ((directory (expand-file-name name org-directory)))
                     (when (file-directory-p directory)
                       (seq-filter
                        (lambda (file)
                          (and (file-regular-p file)
                               (file-readable-p file)
                               (not (string-prefix-p ".#" (file-name-nondirectory file)))
                               (not (string-prefix-p "#" (file-name-nondirectory file)))))
                        (directory-files-recursively directory "\\.org\\'")))))
                 '("notes" "projects" "journal")))))

(defun my/org-agenda ()
  "Refresh the notes list and open the Org agenda dispatcher."
  (interactive)
  (my/org-refresh-agenda-files)
  (call-interactively #'org-agenda))

(my/org-refresh-agenda-files)
(global-set-key (kbd "C-c a") #'my/org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)

;; Index notes; keep the SQLite cache outside the notes repository.
(setq org-roam-directory (file-truename org-directory)
      org-roam-db-location (expand-file-name "org-roam.db" user-emacs-directory)
      org-roam-file-exclude-regexp
      '("\\(?:\\`\\|/\\)\\.[^/]+/" "\\`\\(?:templates\\|attachments\\)/")
      org-roam-capture-templates
      '(("d" "Note" plain "%?"
         :target (file+head "notes/${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))
(require 'org-roam)
(org-roam-db-autosync-mode 1)
(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n c") #'org-roam-capture)

;; Review and sync notes through the existing Git repository.
(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-x g") #'magit-status)
(defun my/org-git-status ()
  "Open Magit for all notes in `org-directory'."
  (interactive)
  (magit-status org-directory))
(global-set-key (kbd "C-c n g") #'my/org-git-status)

;; Emacs Client connects to this session.
(require 'server)
(unless (or noninteractive (daemonp) (server-running-p))
  (server-start))

;;; init.el ends here
