;;; org-config.el --- Org-mode and Org-roam configuration -*- lexical-binding: t; -*-

;; Basic org settings
(setq org-directory "~/org")
(setq org-agenda-files '("~/org/inbox.org"
                         "~/org/gsd.org"
                         "~/org/someday.org"))
(setq org-default-notes-file (concat org-directory "/inbox.org"))

;; Fix for org-element-cache warnings
(setq org-element-use-cache t)
(setq org-element-cache-persistent nil)

;; Org appearance
(setq org-descriptive-links t)
(setq org-log-done 'time)
(setq org-adapt-indentation t)

;; Formatting
(setq org-use-sub-superscripts nil)

;; Clean, modern styling
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)
(setq org-ellipsis " ...")
(setq org-auto-align-tags nil)
(setq org-tags-column 0)
(setq org-catch-invisible-edits 'show-and-error)
(setq org-special-ctrl-a/e t)
(setq org-insert-heading-respect-content t)

;; Virtual indentation
(add-hook 'org-mode-hook #'org-indent-mode)

;; Auto-fill for prose
(add-hook 'org-mode-hook #'auto-fill-mode)

;; Modern, clean org-mode appearance
(use-package org-modern
  :ensure t
  :init
  (global-org-modern-mode)
  :config
  (setq org-modern-star '("*" "*" "*" "*" "*" "*" "*"))
  (setq org-modern-checkbox '((?X . "X")
                              (?- . "-")
                              (?\s . " ")))
  (setq org-modern-table nil)
  (setq org-modern-keyword nil)
  (setq org-modern-timestamp t)
  (setq org-modern-priority t))

;; Scale heading sizes
(custom-set-faces
 '(org-level-1 ((t (:height 1.4 :weight bold))))
 '(org-level-2 ((t (:height 1.3 :weight semi-bold))))
 '(org-level-3 ((t (:height 1.2 :weight semi-bold))))
 '(org-level-4 ((t (:height 1.1))))
 '(org-level-5 ((t (:height 1.0))))
 '(org-document-title ((t (:height 1.6 :weight bold)))))

;; Org refile
(setq org-refile-targets
      '(("gsd.org" :maxlevel . 3)
        ("someday.org" :level . 1)))
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)

;; Custom keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . (:foreground "orange" :weight bold))
        ("CANCELLED" . (:foreground "red" :weight bold))))

;; Auto-update last_modified
(defun my/org-update-last-modified ()
  "Update the #+last_modified: property at the top of the file on save."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+last_modified: " nil t)
        (delete-region (point) (line-end-position))
        (insert (format-time-string "[%Y-%m-%d %a %H:%M]"))))))

(add-hook 'before-save-hook #'my/org-update-last-modified)

;; Sluggifier
(defun my/sluggify-org-title (title)
  "Convert a title to a lowercase-filename-friendly-slug."
  (let* ((slug (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase title))))
    (setq slug (replace-regexp-in-string "^-\\|-$" "" slug))
    slug))

;; Capture templates
(setq org-capture-templates
      '(("i" "Inbox entry" entry
         (file "inbox.org")
         "** TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %i\n  %a")))

;; Org Agenda
(setq org-agenda-with-colors t)
(setq org-agenda-fontify-priorities t)
(setq org-agenda-hide-tags-regexp ".")
(setq org-agenda-skip-deadline-if-scheduled t)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-dim-blocked-tasks nil)

;; Git auto-commit for org directory
(use-package git-auto-commit-mode
  :ensure t
  :config
  (setq gac-debounce-interval 60)
  (setq gac-default-message "sync")

  (defun my/enable-git-auto-commit-in-org ()
    "Enable git-auto-commit-mode with auto-push for files in ~/org."
    (when (and buffer-file-name
               (string-prefix-p (expand-file-name "~/org/")
                                (file-truename buffer-file-name)))
      (setq-local gac-automatically-push-p t)
      (git-auto-commit-mode 1)))

  (add-hook 'org-mode-hook #'my/enable-git-auto-commit-in-org))

;; Org Roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org"))
  (org-roam-db-location (file-truename "~/org/org-roam.db"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "projects/${slug}.org"
                            ":PROPERTIES:\n:ID: %(org-id-new)\n:END:\n#+title: ${title}\n#+last_modified: %U\n\nContext: %a\n\n")
         :unnarrowed t)))

;; Derive headline title for roam node
(defun my/org-roam-node-from-todo ()
  "Get the current headline title (minus TODO) and search/create it in Org-roam."
  (interactive)
  (let* ((title (nth 4 (org-heading-components))))
    (if title
        (org-roam-node-find nil title)
      (message "Not on a headline!"))))

;; org-journal
(use-package org-journal
  :ensure t
  :bind (("C-c j n" . org-journal-new-entry))
  :config
  (setq org-journal-dir "~/org/journal"
        org-journal-file-format "%Y%m%d.org"))

;; Standard org keybindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(provide 'org-config)
