(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if you don't have it
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(cua-mode 1)

;; Org mode
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-directory "~/org")
(setq org-agenda-files '("~/org/inbox.org"
			 "~/org/gtd.org"
			 "~/org/someday.org"))
(setq org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-descriptive-links t)
(setq org-log-done 'time)
(setq org-adapt-indentation t)   ; Tells Org to indent properties/logs to the headline level
(setq org-indent-mode t)        ; Turns on virtual indentation (makes it look nested without adding spaces)

(setq org-refile-targets 
      '(("gtd.org" :maxlevel . 3)      ;; Allow refiling to gtd.org up to 3 levels deep
        ("someday.org" :level . 1)))   ;; Allow refiling to top level of someday.org

;; Custom keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . (:foreground "orange" :weight bold))
        ("CANCELLED" . (:foreground "red" :weight bold))))

;; This helps you find targets with just a few keystrokes
(setq org-outline-path-complete-in-steps nil)         ; Refile in one go
(setq org-refile-use-outline-path 'file)              ; Show file name in the path

;; last_modified hook
(setq org-time-stamps-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))
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
      '(("t" "Todo [Inbox]" entry
         (file "inbox.org")
         "** TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %i\n  %a")))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "projects/${slug}.org"
                            ":PROPERTIES:\n:ID: %(org-id-new)\n:END:\n#+title: ${title}\n#+last_modified: %U\n\nContext: %a\n\n")
         :unnarrowed t)))

;; Org Agenda
(setq org-agenda-with-colors t)
(setq org-agenda-fontify-priorities t)
;; Hide the link brackets and IDs in the agenda
(setq org-agenda-hide-tags-regexp ".")

;; Org Roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t) ;; Silence v2 upgrade warning
  :custom
  (org-roam-directory (file-truename "~/org"))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle))
  :config
  (org-roam-db-autosync-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit org-roam vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Fix for spaces in Org-Roam node creation
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "SPC") #'self-insert-command))

;; Generic fallback if you aren't using Vertico
(define-key minibuffer-local-completion-map (kbd "SPC") #'self-insert-command)

;; Derive headline title
(defun my/org-roam-node-from-todo ()
  "Get the current headline title (minus TODO) and search/create it in Org-roam."
  (interactive)
  (let* ((title (nth 4 (org-heading-components))))
    (if title
        (org-roam-node-find nil title)
      (message "Not on a headline!"))))

;; Bind it to a key (or replace your standard find key)
(global-set-key (kbd "C-c n p") #'my/org-roam-node-from-todo)

;; Hide deadline warning
(setq org-agenda-skip-deadline-if-scheduled t)

;; vterm & lazygit
(use-package vterm
  :ensure t)

(defun my/pop-lazygit ()
  "Open lazygit in a vterm buffer and kill the buffer on exit."
  (interactive)
  (let ((buffer (vterm-other-window "*lazygit*")))
    (with-current-buffer buffer
      ;; This hook kills the buffer as soon as the process (lazygit) exits
      (set-process-sentinel (get-buffer-process buffer)
                            (lambda (proc event)
                              (when (string-match-p "finished" event)
                                (let ((buf (process-buffer proc)))
                                  (when (buffer-live-p buf)
                                    (kill-buffer buf)
                                    ;; Optionally delete the window too
                                    (delete-window))))))
      ;; Send the command
      (vterm-send-string "lazygit && exit")
      (vterm-send-return))))

(global-set-key (kbd "C-c g") #'my/pop-lazygit)
