;;; my-org-autosch.el --- Quick scheduler setup for a task -*- lexical-binding: t; -*-

;; org-auto-scheduler reads a task's :AUTOSCH: tag, its Effort, an optional
;; :SPLITTABLE: tag and MIN_CHUNK property.  These commands set them from
;; the Org heading at point, so a task can be made schedulable in one go.
;; The scheduler has no max-chunk setting, so a maximum block length is set
;; through its POMODORO property (work:break minutes).

(require 'org)

(defvar org-auto-scheduler-splittable-tag)

(defconst my/autosch-efforts '("0:15" "0:30" "1:00" "1:30" "2:00" "3:00" "4:00")
  "Effort values offered when setting a task's duration.")

(defun my/autosch--tag (tag on)
  "Add TAG to the heading at point if ON, else remove it."
  (let ((tags (org-get-tags nil t)))
    (org-set-tags (if on (cl-adjoin tag tags :test #'string=) (remove tag tags)))))

;; The scheduler reads these properties as Org durations, where a bare "45:00"
;; means 45 hours.  Entering plain minutes and storing H:MM avoids that trap.
(defun my/autosch--hmm (minutes)
  "Return the string MINUTES (a number of minutes, or H:MM) as H:MM."
  (let ((text (string-trim minutes)))
    (format-seconds "%h:%02m"
                    (* 60 (if (string-match-p "\\`[0-9]+\\'" text)
                              (string-to-number text)
                            (org-duration-to-minutes text))))))

(defun my/autosch--splittable-tag ()
  (if (boundp 'org-auto-scheduler-splittable-tag) org-auto-scheduler-splittable-tag "SPLITTABLE"))

;;;###autoload
(defun my/autosch-effort ()
  "Set the Effort of the task at point."
  (interactive)
  (org-back-to-heading t)
  (org-set-property "Effort"
                    ;; A prompt left open elsewhere would otherwise refuse this one.
                    (let ((enable-recursive-minibuffers t))
                      (completing-read "Effort (H:MM): " my/autosch-efforts nil nil nil nil
                                     (or (org-entry-get nil "Effort") "1:00")))))

;;;###autoload
(defun my/autosch-toggle-splittable ()
  "Toggle whether the scheduler may split the task at point across time slots."
  (interactive)
  (org-back-to-heading t)
  (let* ((tag (my/autosch--splittable-tag))
         (on (not (member tag (org-get-tags nil t)))))
    (my/autosch--tag tag on)
    (message "%s" (if on "Splittable" "Not splittable"))))

;;;###autoload
(defun my/autosch-task ()
  "Make the task at point schedulable: tag, effort, split rules, deadline.
A task that is already tagged :AUTOSCH: has the tag removed instead, after
confirmation."
  (interactive)
  (org-back-to-heading t)
  (if (member "AUTOSCH" (org-get-tags nil t))
      (when (y-or-n-p "Already auto-scheduled; stop scheduling it? ")
        (my/autosch--tag "AUTOSCH" nil)
        (my/autosch--tag (my/autosch--splittable-tag) nil)
        (message "No longer auto-scheduled"))
    (unless (org-get-todo-state) (org-todo "TODO"))
    (my/autosch--tag "AUTOSCH" t)
    (my/autosch-effort)
    (let ((split (y-or-n-p "Splittable? ")))
      (my/autosch--tag (my/autosch--splittable-tag) split)
      (when split
        (let ((min (read-string "Minimum chunk in minutes (RET for default): ")))
          (unless (string-empty-p min)
            (org-set-property "MIN_CHUNK" (my/autosch--hmm min))))
        ;; The scheduler has no max-chunk setting; POMODORO is the closest:
        ;; WORK-minute blocks with BREAK minutes between them.
        (let ((max (read-string "Maximum block in minutes (RET for none): ")))
          (unless (string-empty-p max)
            (org-set-property "POMODORO"
                              (format "%s:%s" max
                                      (read-string "Break between blocks in minutes: " "10")))))))
    (when (y-or-n-p "Set a deadline? ") (org-deadline nil))
    (message "Queued for scheduling; run C-c A r")))

(global-set-key (kbd "C-c A a") #'my/autosch-task)
(global-set-key (kbd "C-c A e") #'my/autosch-effort)
(global-set-key (kbd "C-c A x") #'my/autosch-toggle-splittable)

(provide 'my-org-autosch)
;;; my-org-autosch.el ends here
