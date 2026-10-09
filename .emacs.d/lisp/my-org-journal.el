;;; my-org-journal.el --- Shared daily journals -*- lexical-binding: t; -*-
(require 'org)
(require 'org-id)
(require 'org-capture)
(require 'calendar)
(require 'my-org-paths)

(defun my/org-journal-file (&optional time)
  "Return the daily journal path for TIME, or today."
  (expand-file-name (format-time-string my/org-journal-file-format time)
                    org-directory))

(defun my/org-journal-quote ()
  "Choose a quote from the configured collection."
  (let (quotes)
    (with-temp-buffer
      (insert-file-contents (expand-file-name my/org-journal-quote-file org-directory))
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'item
        (lambda (item)
          (let ((heading (org-element-lineage item '(headline)))
                (paragraph (car (org-element-contents item))))
            (when (and heading (eq (org-element-type paragraph) 'paragraph))
              (push (cons (string-trim
                           (replace-regexp-in-string
                            "[ \t\n]+" " "
                            (buffer-substring-no-properties
                             (org-element-property :contents-begin paragraph)
                             (org-element-property :contents-end paragraph))))
                          (org-element-property :raw-value heading))
                    quotes))))))
    (unless quotes (user-error "No quotes found in %s" my/org-journal-quote-file))
    (let ((quote (nth (random (length quotes)) quotes)))
      (with-temp-buffer
        (insert (car quote))
        (let ((fill-column 78)) (fill-region (point-min) (point-max)))
        (concat (buffer-string) "\n— " (cdr quote))))))

(defun my/org-journal-ensure-file (&optional time)
  "Create a missing daily journal from the shared template for TIME."
  (let ((file (my/org-journal-file time)))
    (unless (file-exists-p file)
      (when (get-file-buffer file)
        (user-error "A buffer already visits the missing journal: %s" file))
      (let ((template
             (with-temp-buffer
               (insert-file-contents
                (expand-file-name my/org-journal-template-file org-directory))
               (buffer-string)))
            quote)
        (setq template
              (replace-regexp-in-string
               "%<\\([^>]+\\)>"
               (lambda (match)
                 (format-time-string (substring match 2 -1) time))
               template t t))
        (setq template
              (replace-regexp-in-string
               "^{{quote}}$"
               (lambda (_)
                 (save-match-data
                   (or quote (setq quote (my/org-journal-quote)))))
               template t t))
        (make-directory (file-name-directory file) t)
        (with-temp-buffer
          (insert template)
          (org-mode)
          (goto-char (point-min))
          (org-entry-put nil "ID" (org-id-new))
          (write-region (point-min) (point-max) file nil nil nil 'excl))
        (when (featurep 'org-roam)
          (org-roam-db-update-file file))))
    file))

(defun my/org-journal-today ()
  "Open today's journal, creating it from the shared template if needed."
  (interactive)
  (find-file (my/org-journal-ensure-file))
  (goto-char (point-min))
  (when (re-search-forward "^\\*+ Notes[ \t]*$" nil t)
    (org-fold-show-context)
    (org-fold-show-entry)
    (forward-line 1)))

(defun my/org-journal-date (file)
  "Return FILE's calendar date when it matches the daily journal layout."
  (when (and file
             (string-match
              "/\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\.org\\'"
              file))
    (let* ((year (string-to-number (match-string 1 file)))
           (month (string-to-number (match-string 2 file)))
           (day (string-to-number (match-string 3 file)))
           (date (list month day year)))
      (when (and (calendar-date-is-valid-p date)
                 (equal (expand-file-name file)
                        (my/org-journal-file (encode-time 0 0 12 day month year))))
        date))))

(defun my/org-journal-existing ()
  "Return existing daily journals as (DATE . FILE), oldest first."
  (let ((directory
         (expand-file-name
          (car (split-string my/org-journal-file-format "%")) org-directory))
        entries)
    (when (file-directory-p directory)
      (dolist (file (directory-files-recursively directory "\\.org\\'"))
        (let ((date (my/org-journal-date file)))
          (when (and date (file-regular-p file))
            (push (cons date file) entries)))))
    (sort entries
          (lambda (a b)
            (< (calendar-absolute-from-gregorian (car a))
               (calendar-absolute-from-gregorian (car b)))))))

(defun my/org-journal-current-date ()
  "Return the current daily journal's date, or today's calendar date."
  (or (my/org-journal-date buffer-file-name) (calendar-current-date)))

(defun my/org-journal--navigate (next)
  "Open the nearest existing journal after the current date if NEXT.
Otherwise open the nearest existing journal before it."
  (let* ((date (calendar-absolute-from-gregorian (my/org-journal-current-date)))
         (entries (my/org-journal-existing))
         (entry
          (seq-find
           (lambda (entry)
             (funcall (if next #'> #'<)
                      (calendar-absolute-from-gregorian (car entry)) date))
           (if next entries (reverse entries)))))
    (unless entry
      (user-error "No %s daily journal" (if next "next" "previous")))
    (find-file (cdr entry))))

(defun my/org-journal-previous ()
  "Open the previous existing daily journal, skipping days without one.
Outside a daily journal, navigate relative to today."
  (interactive)
  (my/org-journal--navigate nil))

(defun my/org-journal-next ()
  "Open the next existing daily journal, skipping days without one.
Outside a daily journal, navigate relative to today."
  (interactive)
  (my/org-journal--navigate t))

(defface my/org-journal-calendar-face
  '((t (:inherit link :underline t)))
  "Face marking existing journal dates with blue, underlined numbers."
  :group 'calendar)

(defun my/org-journal-calendar-mark ()
  "Mark visible dates that have a daily journal, refreshing from disk."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'my/org-journal-calendar)
      (delete-overlay overlay)))
  (save-excursion
    (dolist (entry (my/org-journal-existing))
      (when (calendar-date-is-visible-p (car entry))
        (calendar-cursor-to-visible-date (car entry))
        (let ((overlay
               (make-overlay (save-excursion
                               (skip-chars-backward "0-9")
                               (point))
                             (1+ (point)))))
          (overlay-put overlay 'my/org-journal-calendar t)
          (overlay-put overlay 'help-echo (cdr entry))
          (overlay-put overlay 'face 'my/org-journal-calendar-face))))))

(defun my/org-journal-calendar-open ()
  "Open the existing daily journal on the selected calendar date."
  (interactive)
  (let* ((date (calendar-cursor-to-date t))
         (file (my/org-journal-file
                (encode-time 0 0 12 (nth 1 date) (car date) (nth 2 date)))))
    (unless (file-regular-p file)
      (user-error "No daily journal for %s" (calendar-date-string date)))
    (calendar-exit)
    (find-file file)))

(defun my/org-journal-calendar ()
  "Pick an existing daily journal in a calendar with marked dates.
Start on the current journal's date, or today.  RET opens the selected
journal; q quits.  Standard calendar keys browse days and months."
  (interactive)
  (let ((date (my/org-journal-current-date))
        (calendar-buffer "*Journal Calendar*"))
    (calendar)
    (use-local-map (copy-keymap calendar-mode-map))
    (local-set-key (kbd "RET") #'my/org-journal-calendar-open)
    (local-set-key (kbd "r") #'calendar-redraw)
    (setq-local header-line-format
                "Daily journals: blue underlined dates have entries | RET open | q quit | r refresh")
    (add-hook 'calendar-today-visible-hook #'my/org-journal-calendar-mark nil t)
    (add-hook 'calendar-today-invisible-hook #'my/org-journal-calendar-mark nil t)
    (calendar-goto-date date)
    (my/org-journal-calendar-mark)))

(defun my/org-journal-capture ()
  "Capture a timestamped entry under today's Notes heading."
  (interactive)
  (org-capture nil "j"))

(add-to-list 'org-capture-templates
             '("j" "Journal note" plain
               (file+headline my/org-journal-ensure-file "Notes")
               "%U\n%?\n" :empty-lines 1))
(global-set-key (kbd "C-c n j") #'my/org-journal-today)
(global-set-key (kbd "C-c n J") #'my/org-journal-capture)
(global-set-key (kbd "C-c n [") #'my/org-journal-previous)
(global-set-key (kbd "C-c n ]") #'my/org-journal-next)
(global-set-key (kbd "C-c n d") #'my/org-journal-calendar)


(defface my/org-journal-quote-face
  '((t (:inherit org-document-title :weight bold)))
  "Face for the daily journal quote.")

(defface my/org-journal-quote-muted-face
  '((t (:inherit shadow)))
  "Face for the quote author.")

(defvar-local my/org-journal-quote-overlays nil)

(defun my/org-journal-style-quote (&rest _)
  "Style the native Org quote block before the first journal heading."
  (mapc #'delete-overlay my/org-journal-quote-overlays)
  (setq my/org-journal-quote-overlays nil)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t)
            (limit (save-excursion
                     (if (re-search-forward org-heading-regexp nil t)
                         (line-beginning-position)
                       (point-max)))))
        (when (re-search-forward "^[ \t]*#\\+begin_quote[ \t]*$" limit t)
          (beginning-of-line)
          (let* ((block (org-element-at-point))
                 (start (org-element-property :contents-begin block))
                 (end (org-element-property :contents-end block)))
            (when (and (eq (org-element-type block) 'quote-block)
                       start end (<= end limit))
              (let ((overlay (make-overlay start end)))
                (overlay-put overlay 'face 'my/org-journal-quote-face)
                (push overlay my/org-journal-quote-overlays))
              (goto-char start)
              (when (re-search-forward "^— .+$" end t)
                (let ((overlay (make-overlay (line-beginning-position)
                                             (line-end-position))))
                  (overlay-put overlay 'face 'my/org-journal-quote-muted-face)
                  (overlay-put overlay 'priority 1)
                  (push overlay my/org-journal-quote-overlays))))))))))

(defun my/org-journal-enable-quote-style ()
  "Enable quote overlays in daily journal files."
  (when (and buffer-file-name
             (file-in-directory-p
              buffer-file-name
              (expand-file-name (car (split-string my/org-journal-file-format "%"))
                                org-directory))
             (string-match-p "/[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org\\'"
                             buffer-file-name))
    (add-hook 'after-change-functions #'my/org-journal-style-quote nil t)
    (add-hook 'after-revert-hook #'my/org-journal-style-quote nil t)
    (my/org-journal-style-quote)))

(add-hook 'org-mode-hook #'my/org-journal-enable-quote-style)

(provide 'my-org-journal)
;;; my-org-journal.el ends here
