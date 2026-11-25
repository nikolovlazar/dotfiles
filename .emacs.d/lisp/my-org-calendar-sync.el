;;; my-org-calendar-sync.el --- Write scheduled tasks to a calendar -*- lexical-binding: t; -*-

;; After org-auto-scheduler applies a schedule, this mirrors each scheduled
;; task block to a calendar as an event, so the time is held for you on every
;; device and for anyone who can see that calendar.  The counterpart of
;; my-org-calendar.el, which reads meetings in.
;;
;; Stateless: each run compares the blocks in Org with the events already in
;; the calendar that carry `my/calendar-block-prefix', then creates what is
;; missing and deletes what is stale.  Running it twice changes nothing.
;; Only future blocks are touched, and only events with the prefix, so your
;; real meetings are never edited.  Events are written through Calendar.app
;; (AppleScript), so any account in it works, Google included.
;;
;; Opt in from an untracked file such as ~/.emacs.d/local.el:
;;   (setq my/calendar-write-calendar "Work")
;; The first run asks macOS to let Emacs control Calendar; allow it.

(require 'cl-lib)
(require 'subr-x)
(require 'my-org-calendar)

(defvar org-auto-scheduler-scheduled-property)
(defvar org-done-keywords)
(declare-function org-map-entries "org")
(declare-function org-get-todo-state "org")
(declare-function org-entry-get "org")
(declare-function org-get-heading "org")
(declare-function my/flex-file "my-org-flex-habits")

(defcustom my/calendar-write-calendar nil
  "Name of the calendar scheduled task blocks are written to, or nil for none."
  :type '(choice (const nil) string))

(defcustom my/calendar-habit-files nil
  "Org files of fixed repeating blocks (lunch and the like) to mirror.
Each entry's SCHEDULED stamp, with its repeater, is expanded into events.
nil means habits.org inside `org-directory'."
  :type '(choice (const nil) (repeat file)))

(defvar my/calendar-sync--process nil
  "The running osascript process, if any.")

(defun my/calendar-sync--range (stamp)
  "Return (START END) for the SCHEDULED STAMP, or nil without an end time."
  (let ((one "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) [^ >]* \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)"))
    (when (and stamp (string-match (concat "\\`" one "\\(?:-\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\)?[^>]*>") stamp))
      (let* ((date (match-string 1 stamp))
             (from (match-string 2 stamp))
             (to (match-string 3 stamp))
             (rest (substring stamp (match-end 0))))
        (cond
         (to (list (my/calendar--time date from) (my/calendar--time date to)))
         ((string-match (concat "\\`--" one) rest)
          (list (my/calendar--time date from)
                (my/calendar--time (match-string 1 rest) (match-string 2 rest)))))))))

(defun my/calendar-sync--title (heading)
  "Return the event title for task HEADING."
  (string-trim (concat my/calendar-block-prefix
                       (replace-regexp-in-string "[\n\r\t]+" " " (substring-no-properties heading)))))

(defun my/calendar-sync--wanted (from to)
  "Return (TITLE START END) for each scheduled task block starting in FROM..TO."
  (require 'org)
  (let (blocks)
    (org-map-entries
     (lambda ()
       (let ((state (org-get-todo-state)))
         (when (and state
                    (not (member state org-done-keywords))
                    (org-entry-get nil org-auto-scheduler-scheduled-property))
           (when-let* ((range (my/calendar-sync--range (org-entry-get nil "SCHEDULED")))
                       ((time-less-p from (car range)))
                       ((time-less-p (car range) to)))
             (push (list (my/calendar-sync--title (org-get-heading t t t t))
                         (car range) (cadr range))
                   blocks)))))
     nil 'agenda)
    (append (nreverse blocks)
            (my/calendar-sync--habits from to)
            (my/calendar-sync--flexible from to))))

(defun my/calendar-sync--occurrences (stamp from to)
  "Return (START END) for each occurrence of SCHEDULED STAMP within FROM..TO.
Handles a time range with an optional +N, ++N or .+N repeater in days or weeks."
  (when (and stamp
             (string-match (concat "\\`<\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
                                   " [^ >]* \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
                                   "-\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
                                   "\\(?: \\(?:\\+\\+\\|\\.\\+\\|\\+\\)\\([0-9]+\\)\\([dw]\\)\\)?[^>]*>")
                           stamp))
    (let* ((n (mapcar (lambda (i) (and (match-string i stamp) (string-to-number (match-string i stamp))))
                      '(1 2 3 4 5 6 7 8)))
           (step (and (nth 7 n) (* (nth 7 n) (if (equal (match-string 9 stamp) "w") 7 1))))
           (day 0) found)
      (while (let* ((start (encode-time 0 (nth 4 n) (nth 3 n) (+ (nth 2 n) day) (nth 1 n) (nth 0 n)))
                    (end (encode-time 0 (nth 6 n) (nth 5 n) (+ (nth 2 n) day) (nth 1 n) (nth 0 n))))
               (when (and (time-less-p from start) (time-less-p start to))
                 (push (list start end) found))
               (and step (time-less-p start to) (setq day (+ day step)))))
      (nreverse found))))

(defun my/calendar-sync--habits (from to)
  "Return (TITLE START END) for repeating blocks in the habit files within FROM..TO."
  (let ((files (seq-filter #'file-exists-p
                           (or my/calendar-habit-files
                               (list (expand-file-name "habits.org" org-directory)))))
        blocks)
    (when files
      (org-map-entries
       (lambda ()
         (let ((title (my/calendar-sync--title
                       ;; "Lunch (Mon)" is one entry per weekday; the day is noise.
                       (replace-regexp-in-string " ([A-Z][a-z][a-z])\\'" ""
                                                 (org-get-heading t t t t)))))
           (dolist (range (my/calendar-sync--occurrences (org-entry-get nil "SCHEDULED") from to))
             (push (list title (car range) (cadr range)) blocks))))
       nil files))
    (nreverse blocks)))

(defun my/calendar-sync--flexible (from to)
  "Return (TITLE START END) for the planned flexible habits starting in FROM..TO."
  (let ((file (and (fboundp 'my/flex-file) (my/flex-file)))
        blocks)
    (when (and file (file-exists-p file))
      (org-map-entries
       (lambda ()
         (when-let* ((range (my/calendar-sync--range (org-entry-get nil "TIMESTAMP")))
                     ((time-less-p from (car range)))
                     ((time-less-p (car range) to)))
           (push (list (my/calendar-sync--title (org-get-heading t t t t))
                       (car range) (cadr range))
                 blocks)))
       nil (list file)))
    (nreverse blocks)))

(defun my/calendar-sync--existing (from to)
  "Return (TITLE START END) for our events in the write calendar in FROM..TO."
  (seq-filter (lambda (event)
                (and (string-prefix-p my/calendar-block-prefix (car event))
                     (time-less-p from (nth 1 event))
                     (time-less-p (nth 1 event) to)))
              (my/calendar--parse
               (my/calendar--run (list my/calendar-write-calendar)))))

(defun my/calendar-sync--key (event)
  "Return a string identifying EVENT by title and times."
  (format "%s\t%d\t%d" (nth 0 event)
          (floor (float-time (nth 1 event))) (floor (float-time (nth 2 event)))))

(defun my/calendar-sync--diff (wanted existing)
  "Return (CREATE . DELETE): what to add and remove to turn EXISTING into WANTED."
  (let ((stale (copy-sequence existing)) create)
    (dolist (event wanted)
      (let ((match (cl-find (my/calendar-sync--key event) stale
                            :key #'my/calendar-sync--key :test #'equal)))
        (if match
            (setq stale (cl-remove match stale :count 1 :test #'eq))
          (push event create))))
    (cons (nreverse create) stale)))

(defun my/calendar-sync--string (text)
  "Return TEXT as an AppleScript string literal."
  (format "\"%s\"" (replace-regexp-in-string
                    "[\\\"]" "\\\\\\&" text)))

(defun my/calendar-sync--date (time)
  "Return an AppleScript expression for the local TIME."
  (let ((parts (decode-time time)))
    (format "my mk(%d, %d, %d, %d, %d)"
            (nth 5 parts) (nth 4 parts) (nth 3 parts) (nth 2 parts) (nth 1 parts))))

(defun my/calendar-sync--script (create delete)
  "Return the AppleScript that deletes DELETE events and creates CREATE ones."
  (concat
   "on mk(y, m, d, hh, mm)\n"
   "  set dt to current date\n"
   "  set day of dt to 1\n  set year of dt to y\n  set month of dt to m\n"
   "  set day of dt to d\n  set hours of dt to hh\n  set minutes of dt to mm\n"
   "  set seconds of dt to 0\n  return dt\nend mk\n"
   "tell application \"Calendar\"\n"
   (format "  tell calendar %s\n" (my/calendar-sync--string my/calendar-write-calendar))
   (mapconcat
    (lambda (event)
      (format "    try\n      delete (every event whose summary is %s and start date is %s)\n    end try\n"
              (my/calendar-sync--string (nth 0 event))
              (my/calendar-sync--date (nth 1 event))))
    delete "")
   (mapconcat
    (lambda (event)
      (format "    make new event with properties {summary:%s, start date:%s, end date:%s}\n"
              (my/calendar-sync--string (nth 0 event))
              (my/calendar-sync--date (nth 1 event))
              (my/calendar-sync--date (nth 2 event))))
    create "")
   "  end tell\nend tell\n"))

(defun my/calendar-sync--run (script summary)
  "Run SCRIPT through osascript without blocking Emacs, then report SUMMARY."
  (let* ((buffer (generate-new-buffer " *calendar-sync*"))
         (process (make-process
                   :name "calendar-sync" :buffer buffer :connection-type 'pipe
                   :command '("osascript" "-")
                   :sentinel
                   (lambda (process _event)
                     (unless (process-live-p process)
                       (setq my/calendar-sync--process nil)
                       (let ((output (string-trim (with-current-buffer (process-buffer process)
                                                    (buffer-string)))))
                         (kill-buffer (process-buffer process))
                         (if (zerop (process-exit-status process))
                             (message "Calendar: %s" summary)
                           (message "Calendar write failed: %s"
                                    (if (string-match-p "-1743\\|not allowed" output)
                                        "allow Emacs to control Calendar under System Settings > Privacy & Security > Automation"
                                      output)))))))))
    (setq my/calendar-sync--process process)
    (process-send-string process script)
    (process-send-eof process)))

;;;###autoload
(defun my/calendar-sync-blocks (&optional dry-run)
  "Write scheduled task blocks to `my/calendar-write-calendar'.
With a prefix argument, only report what would change."
  (interactive "P")
  (cond
   ((null my/calendar-write-calendar)
    (when (called-interactively-p 'any)
      (user-error "Set `my/calendar-write-calendar' first")))
   ((not (eq system-type 'darwin))
    (user-error "Writing events uses Calendar.app, which is macOS only"))
   ((process-live-p my/calendar-sync--process)
    (message "Calendar: previous write still running"))
   (t
    (let* ((from (current-time))
           (to (time-add from (days-to-time my/calendar-days)))
           (diff (my/calendar-sync--diff (my/calendar-sync--wanted from to)
                                         (my/calendar-sync--existing from to)))
           (summary (format "%d event%s added, %d removed in %s"
                            (length (car diff)) (if (= (length (car diff)) 1) "" "s")
                            (length (cdr diff)) my/calendar-write-calendar)))
      (cond
       (dry-run (message "Calendar (dry run): %s" summary))
       ((and (null (car diff)) (null (cdr diff)))
        (message "Calendar: blocks already up to date"))
       (t (my/calendar-sync--run (my/calendar-sync--script (car diff) (cdr diff))
                                 summary)))))))

(defun my/calendar-sync--after-apply (&rest _)
  "Mirror the schedule to the calendar after it is applied, never failing it."
  (when my/calendar-write-calendar
    (condition-case err
        (my/calendar-sync-blocks)
      (error (message "Calendar write skipped: %s" (error-message-string err))))))

;; The review buffer applies through `-review-execute'; the one-shot command
;; applies directly, but also runs in preview mode from the review, which
;; must not write anything.
(with-eval-after-load 'org-auto-scheduler-review
  (advice-add 'org-auto-scheduler-review-execute :after #'my/calendar-sync--after-apply))
(defun my/calendar-sync--after-schedule (&rest args)
  "Mirror a directly applied schedule, but not a preview, to the calendar."
  (unless (bound-and-true-p org-auto-scheduler--preview-mode)
    (apply #'my/calendar-sync--after-apply args)))

(with-eval-after-load 'org-auto-scheduler
  (advice-add 'org-auto-scheduler-schedule-tasks :after #'my/calendar-sync--after-schedule))

(global-set-key (kbd "C-c A p") #'my/calendar-sync-blocks)

(provide 'my-org-calendar-sync)
;;; my-org-calendar-sync.el ends here
