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
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("gnu" . 20) ("nongnu" . 20) ("melpa-stable" . 20) ("melpa" . 0))
      package-selected-packages '(org-roam org-roam-ui magit))
(package-initialize)

;; Built-in tinted Modus themes and the theme toggle.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-theme)

;; Personal paths belong in the ignored local configuration.
(require 'my-org-paths)
(load (expand-file-name "local.el" user-emacs-directory) t t)

;; Agenda sources exclude configured support directories and hidden files.
(require 'org)
(require 'org-agenda)

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
      org-log-done 'time
      org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-capture-templates
      '(("i" "Daily task" entry
         (file+headline my/org-journal-ensure-file "✅ Tasks")
         "* TODO %?\n")))

;; Share daily journal files and templates with Neovim.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-org-journal)
(setq org-default-notes-file (my/org-journal-file))

;; Use native Org highlighting and folding without custom inline previews.
;; Auto-fill prose at 80 columns without breaking table source.
(require 'my-org-prose)
(require 'my-org-tables)

(defun my/org-refresh-agenda-files ()
  "Discover current agenda files without scanning templates or backups."
  (setq org-agenda-files
        (when (file-directory-p org-directory)
          (seq-filter
           (lambda (file)
             (and (file-regular-p file)
                  (file-readable-p file)
                  (not (my/org-excluded-path-p file))))
           (directory-files-recursively
            org-directory "\\.org\\'" nil
            (lambda (directory) (not (my/org-excluded-path-p directory))))))))

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
      (list (concat "\\(?:\\`\\|/\\)\\.[^/]+/"
                    (when my/org-excluded-directories
                      (concat "\\|\\`" (regexp-opt my/org-excluded-directories t) "/"))))
      org-roam-capture-templates
      `(("d" "Note" plain "%?"
         :target (file+head ,my/org-note-file-format "#+title: ${title}\n")
         :unnarrowed t)))
(require 'org-roam)
(require 'my-org-reflections)
;; Keep the full graph compact enough for Emacs to render.
(setq org-roam-graph-executable "neato"
      org-roam-graph-extra-config
      '(("overlap" . "false")
        ("pack" . "true")
        ("size" . "\"18,18\"")))
(org-roam-db-autosync-mode 1)
(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n c") #'org-roam-capture)

;; Open the live browser graph on demand.
(autoload 'org-roam-ui-open "org-roam-ui" nil t)
(setq org-roam-ui-sync-theme t
      org-roam-ui-follow t
      org-roam-ui-update-on-save t
      org-roam-ui-open-on-start t)
(global-set-key (kbd "C-c n u") #'org-roam-ui-open)

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
