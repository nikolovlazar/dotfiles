;;; my-org-journal.el --- Shared daily journals -*- lexical-binding: t; -*-
(require 'org)
(require 'org-id)
(require 'org-capture)
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
        (concat "> [!QUOTE] A thought to carry\n> "
                (replace-regexp-in-string "\n" "\n> " (buffer-string) t t)
                "\n>\n> — " (cdr quote))))))

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
  (when (re-search-forward "^\\*+ ✍️ Notes[ \t]*$" nil t)
    (org-fold-show-context)
    (org-fold-show-entry)
    (forward-line 1)))

(defun my/org-journal-capture ()
  "Capture a timestamped entry under today's Notes heading."
  (interactive)
  (org-capture nil "j"))

(add-to-list 'org-capture-templates
             '("j" "Journal note" plain
               (file+headline my/org-journal-ensure-file "✍️ Notes")
               "%U\n%?\n" :empty-lines 1))
(global-set-key (kbd "C-c n j") #'my/org-journal-today)
(global-set-key (kbd "C-c n J") #'my/org-journal-capture)

(provide 'my-org-journal)
;;; my-org-journal.el ends here
