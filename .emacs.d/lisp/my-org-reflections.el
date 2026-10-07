;;; my-org-reflections.el --- Shared reflection capture -*- lexical-binding: t; -*-
(require 'org-capture)
(require 'org-id)
(require 'my-org-paths)

(defun my/org-reflection-prompts ()
  "Read emotions, areas and lenses from the shared prompt library."
  (let (section current library)
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name my/org-reflection-prompts-file org-directory))
      (dolist (line (split-string (buffer-string) "\n"))
        (cond
         ((string-match "^\\* \\(.+\\)$" line)
          (setq section (downcase (match-string 1 line)) current nil))
         ((and (member section '("emotions" "areas" "lenses"))
               (string-match "^- \\(.+\\)$" line))
          (setq current (list (match-string 1 line)))
          (push (cons section current) library))
         ((and current (string-match "^[ \t]+\\(\\S-.*\\)$" line))
          (setcar current (concat (car current) " " (match-string 1 line))))
         (t (setq current nil)))))
    (mapcar
     (lambda (name)
       (let ((items (mapcar #'cadr
                            (seq-filter (lambda (entry) (equal (car entry) name))
                                        (reverse library)))))
         (unless items (user-error "No reflection %s found" name))
         (cons name items)))
     '("emotions" "areas" "lenses"))))

(defun my/org-reflection-template ()
  "Expand the shared reflection template and choose a new capture file."
  (let* ((library (my/org-reflection-prompts))
         (pick (lambda (name)
                 (let ((items (cdr (assoc name library))))
                   (nth (random (length items)) items))))
         (emotion (funcall pick "emotions"))
         (area-entry (funcall pick "areas"))
         (lens (funcall pick "lenses"))
         (time (current-time)))
    (unless (string-match "\\`\\(.+?\\) :: \\(.+\\)\\'" area-entry)
      (user-error "Reflection areas need a name :: description"))
    (let* ((area (match-string 1 area-entry))
           (description (match-string 2 area-entry))
           (prompt (format "What has made me feel %s lately in %s?" emotion area))
           (values `(("emotion" . ,emotion) ("area" . ,area)
                     ("lens" . ,lens) ("description" . ,description)
                     ("prompt" . ,prompt) ("cursor" . "%?")))
           (template (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name my/org-reflection-template-file org-directory))
                       (buffer-string)))
           (slug (string-trim
                  (replace-regexp-in-string "[^a-z0-9]+" "-"
                                            (downcase (concat emotion "-" area)))
                  "-" "-"))
           (stem (concat (file-name-as-directory my/org-reflection-directory)
                         (format "%s-%s" (format-time-string "%Y-%m-%d" time) slug)))
           (path (concat stem ".org"))
           (suffix 1))
      (setq template
            (replace-regexp-in-string
             "%<\\([^>]+\\)>"
             (lambda (match) (format-time-string (substring match 2 -1) time))
             template t t))
      (setq template
            (replace-regexp-in-string
             "{{\\([a-z]+\\)}}"
             (lambda (match)
               (or (cdr (assoc (substring match 2 -2) values))
                   (user-error "Unknown reflection placeholder: %s" match)))
             template t t))
      (while (or (file-exists-p (expand-file-name path org-directory))
                 (get-file-buffer (expand-file-name path org-directory)))
        (setq suffix (1+ suffix)
              path (format "%s-%d.org" stem suffix)))
      (make-directory (file-name-directory (expand-file-name path org-directory)) t)
      (org-capture-put :reflection-file (expand-file-name path org-directory))
      (with-temp-buffer
        (insert template)
        (org-mode)
        (goto-char (point-min))
        (org-entry-put nil "ID" (org-id-new))
        (buffer-string)))))

(defun my/org-reflection-file ()
  "Return the new file chosen while expanding the reflection template."
  (org-capture-get :reflection-file))

(defun my/org-reflection-capture ()
  "Capture a reflection using the standard Org capture template."
  (interactive)
  (org-capture nil "r"))

(add-to-list 'org-capture-templates
             '("r" "Reflection" plain
               (file my/org-reflection-file)
               (function my/org-reflection-template)
               :unnarrowed t))
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c n r") #'my/org-reflection-capture)
(provide 'my-org-reflections)
;;; my-org-reflections.el ends here
