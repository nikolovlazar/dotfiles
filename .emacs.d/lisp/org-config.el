;; Org-mode and Org-roam configuration

;; Basic org settings
(setq org-directory "~/org")
(setq org-agenda-files '("~/org/inbox.org"
			 "~/org/gsd.org"
			 "~/org/someday.org"))
(setq org-default-notes-file (concat org-directory "/inbox.org"))

;; Org appearance
(setq org-descriptive-links t)
(setq org-log-done 'time)
(setq org-adapt-indentation t)   ; Tells Org to indent properties/logs to the headline level
(setq org-indent-mode t)        ; Turns on virtual indentation (makes it look nested without adding spaces)

;; Org refile
(setq org-refile-targets 
      '(("gsd.org" :maxlevel . 3)      ;; Allow refiling to gsd.org up to 3 levels deep
        ("someday.org" :level . 1)))   ;; Allow refiling to top level of someday.org
(setq org-outline-path-complete-in-steps nil)         ; Refile in one go
(setq org-refile-use-outline-path 'file)              ; Show file name in the path

;; Custom keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . (:foreground "orange" :weight bold))
        ("CANCELLED" . (:foreground "red" :weight bold))))

;; Timestamps (last_modified)
(setq org-time-stamps-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))

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

;; Org Roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t) ;; Silence v2 upgrade warning
  :custom
  (org-roam-directory (file-truename "~/org"))
  (org-roam-db-location (file-truename "~/org/org-roam.db"))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle))
  :config
  (org-roam-db-autosync-mode))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "projects/${slug}.org"
                            ":PROPERTIES:\n:ID: %(org-id-new)\n:END:\n#+title: ${title}\n#+last_modified: %U\n\nContext: %a\n\n")
         :unnarrowed t)))

;; Fix for spaces in Org-Roam node creation
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "SPC") #'self-insert-command))

(define-key minibuffer-local-completion-map (kbd "SPC") #'self-insert-command)

;; Derive headline title for roam node
(defun my/org-roam-node-from-todo ()
  "Get the current headline title (minus TODO) and search/create it in Org-roam."
  (interactive)
  (let* ((title (nth 4 (org-heading-components))))
    (if title
        (org-roam-node-find nil title)
      (message "Not on a headline!"))))

(provide 'org-config)
