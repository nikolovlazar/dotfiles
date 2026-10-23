;;; my-org-agenda.el --- Fast background agenda buffers -*- lexical-binding: t; -*-
(require 'org-agenda)

(defvar-local my/org-agenda-deferred-setup nil)

(defun my/org-agenda-setup-visible-buffer (window)
  "Finish editing setup when this agenda source is displayed in WINDOW."
  (when (eq (window-buffer window) (current-buffer))
    (remove-hook 'window-buffer-change-functions
                 #'my/org-agenda-setup-visible-buffer t)
    (let ((setup my/org-agenda-deferred-setup))
      (setq my/org-agenda-deferred-setup nil)
      (dolist (function setup) (funcall function)))))

(defun my/org-agenda-file-buffer (original file)
  "Defer Git checks and table display for new background agenda buffers."
  (if (org-find-base-buffer-visiting file)
      (funcall original file)
    (let* ((setup
            (append
             (when (memq 'vc-refresh-state find-file-hook)
               '(vc-refresh-state))
             (when (memq 'magit-auto-revert-mode-enable-in-buffer
                         after-change-major-mode-hook)
               '(magit-auto-revert-mode-enable-in-buffer))
             (when (memq 'markdown-table-wrap-pretty-mode org-mode-hook)
               '(markdown-table-wrap-pretty-mode))))
           (find-file-hook (remq 'vc-refresh-state find-file-hook))
           (after-change-major-mode-hook
            (remq 'magit-auto-revert-mode-enable-in-buffer
                  after-change-major-mode-hook))
           (org-mode-hook (remq 'markdown-table-wrap-pretty-mode org-mode-hook))
           (buffer (funcall original file)))
      (with-current-buffer buffer
        (setq my/org-agenda-deferred-setup setup)
        ;; Keep external note edits visible without Magit's per-file Git probes.
        (when (memq 'magit-auto-revert-mode-enable-in-buffer setup)
          (auto-revert-mode 1))
        (add-hook 'window-buffer-change-functions
                  #'my/org-agenda-setup-visible-buffer nil t))
      buffer)))

(defun my/org-agenda-prepare (original &rest args)
  "Avoid frequent garbage collection while loading agenda sources."
  (let ((gc-cons-threshold (max gc-cons-threshold (* 64 1024 1024))))
    (apply original args)))

(advice-add 'org-get-agenda-file-buffer :around #'my/org-agenda-file-buffer)
(advice-add 'org-agenda-prepare-buffers :around #'my/org-agenda-prepare)
(defface my/org-agenda-date
  '((((class color) (background light))
     (:foreground "#665080" :background "#eee6f4"))
    (((class color) (background dark))
     (:foreground "#c9b8df" :background "#393044"))
    (t (:inherit font-lock-constant-face)))
  "Face for agenda scheduling and deadline information."
  :group 'org-agenda)

(defface my/org-agenda-file
  '((t (:inherit shadow)))
  "Face for the less prominent agenda source filename."
  :group 'org-agenda)

(defun my/org-agenda-entry-dates-label ()
  "Return \"Deadline YYYY-MM-DD Day\" / \"Scheduled ...\" for the entry at point."
  (string-join
   (delq nil
         (mapcar
          (lambda (spec)
            (let ((ts (org-entry-get nil (car spec))))
              (and ts (format "%s %s" (cdr spec)
                              (format-time-string "%Y-%m-%d %a"
                                                  (org-time-string-to-time ts))))))
          '(("SCHEDULED" . "Scheduled") ("DEADLINE" . "Deadline"))))
   " "))

(defun my/org-agenda-format-row (row)
  "Put the task first, capped at 80 columns, then scheduling and its file."
  (let ((heading (get-text-property 0 'txt row)))
    (if (or (not (derived-mode-p 'org-mode))
            (not buffer-file-name)
            (not heading) (string-empty-p heading))
        row
      (let* ((task (string-trim
                    (replace-regexp-in-string
                     org-tag-group-re "" (org-link-display-format heading))))
             (task (truncate-string-to-width task 80 nil nil "…"))
             (date (string-trim
                    (concat (get-text-property 0 'time row) " "
                            (get-text-property 0 'extra row))))
             (date (replace-regexp-in-string "[[:space:]]+" " " date))
             (date (string-remove-suffix ":" date))
             ;; Plain TODO lists (e.g. "Future") carry no time info, so show
             ;; the entry's own DEADLINE / SCHEDULED dates.
             (date (if (string-empty-p date)
                       (my/org-agenda-entry-dates-label)
                     date))
             (file (file-name-nondirectory buffer-file-name))
             (result (concat "  " task "  " (unless (string-empty-p date)
                                                  (concat date "  ")) file)))
        (add-text-properties 0 (length result) (text-properties-at 0 row) result)
        (remove-text-properties 0 (length result) '(org-heading nil) result)
        (add-text-properties 2 (+ 2 (length task)) '(org-heading t) result)
        ;; Display strings retain their faces when Org colors the whole row.
        (unless (string-empty-p date)
          (add-text-properties
           (+ 4 (length task)) (+ 4 (length task) (length date))
           (list 'display (propertize date 'face 'my/org-agenda-date)) result))
        (add-text-properties
         (- (length result) (length file)) (length result)
         (list 'display (propertize file 'face 'my/org-agenda-file)) result)
        result))))

(advice-add 'org-agenda-format-item :filter-return #'my/org-agenda-format-row)

;;; Weekly agenda with "Unplanned" and "Future" sections

(defun my/org-agenda-entry-day (property)
  "Absolute day of PROPERTY (\"DEADLINE\" or \"SCHEDULED\") at point, or nil."
  (let ((ts (org-entry-get nil property)))
    (and ts (org-time-string-to-absolute ts))))

(defun my/org-agenda-skip-planned ()
  "Skip entries that have a DEADLINE or SCHEDULED date."
  (when (or (my/org-agenda-entry-day "DEADLINE")
            (my/org-agenda-entry-day "SCHEDULED"))
    (org-with-wide-buffer (or (outline-next-heading) (point-max)))))

(defun my/org-agenda-skip-not-future ()
  "Skip entries unless a DEADLINE or SCHEDULED date is more than a week ahead."
  (let* ((limit (+ (org-today) 7))
         (days (delq nil (list (my/org-agenda-entry-day "DEADLINE")
                               (my/org-agenda-entry-day "SCHEDULED")))))
    (unless (seq-some (lambda (day) (> day limit)) days)
      (org-with-wide-buffer (or (outline-next-heading) (point-max))))))

;; Replaces the built-in weekly agenda on "a": the next 7 days from today (so
;; "Future" starts exactly where the agenda ends), then undated and far-off tasks.
(setq org-agenda-custom-commands
      '(("a" "Week, Unplanned and Future"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-start-on-weekday nil)))
          (todo "TODO|PROGRESS|WAITING"
                ((org-agenda-overriding-header "Future")
                 (org-agenda-skip-function #'my/org-agenda-skip-not-future)))
          (todo "TODO|PROGRESS|WAITING"
                ((org-agenda-overriding-header "Unplanned")
                 (org-agenda-skip-function #'my/org-agenda-skip-planned)))))))

(provide 'my-org-agenda)
;;; my-org-agenda.el ends here
