;;; org-config.el --- Org-mode and Org-roam configuration -*- lexical-binding: t; -*-

;; Basic org settings
(setq org-directory "~/org")
(setq org-agenda-files '("~/org/inbox.org"
			 "~/org/gsd.org"
			 "~/org/someday.org"))
(setq org-default-notes-file (concat org-directory "/inbox.org"))

;; Fix for org-element-cache warnings (common in Emacs 31 dev builds)
(setq org-element-use-cache t)
(setq org-element-cache-persistent nil)  ; Don't persist cache to disk

;; Org appearance
(setq org-descriptive-links t)
(setq org-log-done 'time)
(setq org-adapt-indentation t)   ; Indent properties/logs to headline level

;; Clean, modern styling
(setq org-hide-emphasis-markers t)      ; Hide markup characters (*bold*, /italic/, etc.)
(setq org-pretty-entities t)            ; Render \alpha, \beta, etc. as symbols
(setq org-ellipsis "…")                 ; Nicer ellipsis for folded content
(setq org-auto-align-tags nil)          ; Don't auto-align tags (faster)
(setq org-tags-column 0)                ; Tags right after heading
(setq org-catch-invisible-edits 'show-and-error)  ; Prevent editing hidden text
(setq org-special-ctrl-a/e t)           ; Smart home/end keys
(setq org-insert-heading-respect-content t)  ; New headings go after content

;; Virtual indentation (looks nested without adding spaces)
(add-hook 'org-mode-hook #'org-indent-mode)

;; Enable auto-fill mode for automatic line wrapping
(add-hook 'org-mode-hook #'auto-fill-mode)

;; Modern, clean org-mode appearance
(use-package org-modern
  :ensure t
  :init
  (global-org-modern-mode)  ; Enable in all org buffers
  :config
  ;; Heading bullets
  (setq org-modern-star '("◉" "○" "●" "○" "●" "○" "●"))
  ;; Better checkboxes
  (setq org-modern-checkbox '((?X . "☑")
                              (?- . "◐")
                              (?\s . "☐")))
  ;; Keep classic rendering for some elements
  (setq org-modern-table nil)     ; Keep classic table rendering
  (setq org-modern-keyword nil)   ; Keep classic #+keyword style
  (setq org-modern-timestamp t)   ; Style timestamps
  (setq org-modern-priority t))   ; Style priorities

;; Scale heading sizes for visual hierarchy (like rendered markdown)
(custom-set-faces
 '(org-level-1 ((t (:height 1.4 :weight bold))))
 '(org-level-2 ((t (:height 1.3 :weight semi-bold))))
 '(org-level-3 ((t (:height 1.2 :weight semi-bold))))
 '(org-level-4 ((t (:height 1.1))))
 '(org-level-5 ((t (:height 1.0))))
 '(org-document-title ((t (:height 1.6 :weight bold)))))

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

;; Agenda performance
(setq org-agenda-inhibit-startup t)      ; Don't apply startup visibility
(setq org-agenda-dim-blocked-tasks nil)  ; Faster rendering

;; Git auto-commit for org directory
(use-package git-auto-commit-mode
  :ensure t
  :config
  ;; Wait 60 seconds after last change before committing
  (setq gac-debounce-interval 60)
  ;; Commit message format: "sync [timestamp]"
  (setq gac-default-message "sync")

  ;; Enable git-auto-commit-mode only for files in ~/org directory
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
  (setq org-roam-v2-ack t) ;; Silence v2 upgrade warning
  :custom
  (org-roam-directory (file-truename "~/org"))
  (org-roam-db-location (file-truename "~/org/org-roam.db"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config

  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
 )

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

;;; Dictionary support for writing

(defun my/org-mode-completion-setup ()
  "Add dictionary completion with definitions for org-mode."
  (setq-local completion-at-point-functions
              (list #'my/cape-dict-with-definitions #'cape-dabbrev #'cape-file))
  ;; Slower completion delay for prose writing
  (setq-local corfu-auto-delay 0.4))

(add-hook 'org-mode-hook #'my/org-mode-completion-setup)

;; org-diary
(use-package org-journal
  :ensure t
  :bind (("C-c j n" . org-journal-new-entry))
  :config
  (setq org-journal-dir "~/org/journal")
  )

(provide 'org-config)
