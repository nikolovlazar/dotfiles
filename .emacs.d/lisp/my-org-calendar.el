;;; my-org-calendar.el --- Calendar meetings as Org busy time -*- lexical-binding: t; -*-

;; Reads events from the macOS calendar database, which holds every account
;; added to Calendar.app (Google, iCloud, Exchange, ...), with icalBuddy, and
;; writes them to an Org file inside the agenda files.  org-auto-scheduler
;; treats Org timestamps that are not :AUTOSCH: tasks as fixed events, so
;; tasks get placed around meetings.  Read-only: nothing is written back to
;; any calendar.
;;
;; Setup (macOS):
;;   brew install ical-buddy
;;   System Settings > Privacy & Security > Calendars: allow Emacs.
;; Keep private choices, such as which calendars to import, in an untracked
;; file like ~/.emacs.d/local.el:
;;   (setq my/calendar-include '("Work" "Family"))

(require 'subr-x)

(defgroup my/calendar nil
  "Calendar meetings as Org busy time."
  :group 'org)

(defcustom my/calendar-include nil
  "Names of the calendars to import, or nil for every calendar.
All-day events are never imported, since they don't block working time."
  :type '(repeat string))

(defcustom my/calendar-days 21
  "How many days ahead to import."
  :type 'integer)

(defcustom my/calendar-file nil
  "Org file the meetings are written to, or nil for calendar/busy.org
inside `org-directory'.  It must lie inside the agenda files."
  :type '(choice (const nil) file))

(defcustom my/calendar-min-interval 60
  "Seconds before scheduling refreshes the calendar again."
  :type 'integer)

(defcustom my/calendar-block-prefix "[Org] "
  "Title prefix on the events written by `my-org-calendar-sync'.
Imports skip events with this prefix, so scheduled tasks never block
themselves."
  :type 'string)

(defvar my/calendar-after-refresh-functions nil
  "Functions called with the imported meetings after each refresh.
Each meeting is a list (TITLE START END).")

(defvar my/calendar--last-refresh nil
  "Time of the last successful refresh.")

(defconst my/calendar--todo-keywords
  '("TODO" "PROGRESS" "WAITING" "DONE" "CANCELLED")
  "Headings starting with these would become tasks, so titles are guarded.")

(defun my/calendar-file ()
  "Return the Org file the meetings are written to."
  (or my/calendar-file
      (expand-file-name "calendar/busy.org" (or (bound-and-true-p org-directory) "~/org"))))

(defun my/calendar--executable ()
  "Return the icalBuddy executable, or signal a helpful error."
  (or (executable-find "icalBuddy")
      (user-error "icalBuddy not found; install it with: brew install ical-buddy")))

(defun my/calendar--time (date clock)
  "Return the local time for DATE (YYYY-MM-DD) and CLOCK (HH:MM)."
  (date-to-time (format "%s %s:00" date clock)))

(defun my/calendar--run (&optional include days)
  "Return icalBuddy's text for the INCLUDE calendars over DAYS days.
Both default to `my/calendar-include' and `my/calendar-days'."
  (let ((coding-system-for-read 'utf-8)
        (include (or include my/calendar-include))
        (args (append '("-nc" "-npn" "-nrd" "-ea" "-b" "§"
                        "-iep" "title,datetime"
                        "-df" "%Y-%m-%d" "-tf" "%H:%M")
                      (when include
                        (list "-ic" (string-join include ",")))
                      (list "eventsFrom:today"
                            (format "to:today+%d" (or days my/calendar-days))))))
    (with-temp-buffer
      (let ((status (apply #'call-process (my/calendar--executable) nil t nil args))
            (text (buffer-string)))
        (cond
         ((string-match-p "^error: No calendars" text)
          (user-error "No calendars visible: allow Emacs under System Settings > Privacy & Security > Calendars"))
         ((string-match "^error: \\(.*\\)$" text)
          (user-error "icalBuddy: %s" (match-string 1 text)))
         ((not (eq status 0))
          (user-error "icalBuddy exited with status %s" status)))
        text))))

(defun my/calendar--parse (text)
  "Return (TITLE START END) for each event in icalBuddy TEXT."
  (let ((range (concat "\\`[ \t]+\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) at "
                       "\\([0-9]\\{2\\}:[0-9]\\{2\\}\\) - "
                       "\\(?:\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) at \\)?"
                       "\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)"))
        events title)
    (dolist (line (split-string text "\n"))
      (cond
       ((string-prefix-p "§" line)
        (setq title (string-trim (substring line 1))))
       ((and title (string-match range line))
        (let ((date (match-string 1 line)))
          (push (list title
                      (my/calendar--time date (match-string 2 line))
                      (my/calendar--time (or (match-string 3 line) date)
                                         (match-string 4 line)))
                events))
        (setq title nil))))
    (sort (nreverse events)
          (lambda (a b) (time-less-p (nth 1 a) (nth 1 b))))))

(defun my/calendar--heading (title)
  "Return TITLE made safe to use as an Org heading."
  (let* ((s (replace-regexp-in-string "[\n\r\t]+" " " title))
         ;; A trailing :word: would be read as a tag.
         (s (replace-regexp-in-string ":\\([[:alnum:]_@#%]+\\):[ ]*\\'" "\\1" s)))
    (if (string-match-p (concat "\\`" (regexp-opt my/calendar--todo-keywords) "\\b") s)
        (concat "· " s)
      (if (string-empty-p s) "(no title)" s))))

(defun my/calendar--timestamp (start end)
  "Return an active Org timestamp covering START to END."
  (let ((system-time-locale "C"))
    (if (equal (format-time-string "%F" start) (format-time-string "%F" end))
        (format "<%s-%s>" (format-time-string "%Y-%m-%d %a %H:%M" start)
                (format-time-string "%H:%M" end))
      (format "<%s>--<%s>" (format-time-string "%Y-%m-%d %a %H:%M" start)
              (format-time-string "%Y-%m-%d %a %H:%M" end)))))

(defun my/calendar--render (events)
  "Return the Org file text for EVENTS."
  (concat "#+title: Calendar\n"
          "# Imported from the system calendar by my-org-calendar.el.\n"
          "# Overwritten on every refresh; edit the calendar, not this file.\n\n"
          (mapconcat (lambda (event)
                       (format "* %s\n%s\n" (my/calendar--heading (nth 0 event))
                               (my/calendar--timestamp (nth 1 event) (nth 2 event))))
                     events "")))

(defun my/calendar--write (content &optional target)
  "Write CONTENT to TARGET if it changed.  Return non-nil if so.
TARGET defaults to the calendar file."
  (let ((file (or target (my/calendar-file))))
    (unless (and (file-exists-p file)
                 (string= content (with-temp-buffer
                                    (insert-file-contents file)
                                    (buffer-string))))
      (make-directory (file-name-directory file) t)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region content nil file nil 'silent))
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (unless (buffer-modified-p) (revert-buffer t t t))))
      ;; A new file must join the agenda files before the scheduler looks.
      (when (and (fboundp 'my/org-refresh-agenda-files)
                 (not (member file (bound-and-true-p org-agenda-files))))
        (my/org-refresh-agenda-files))
      t)))

;;;###autoload
(defun my/calendar-refresh (&optional quiet)
  "Import upcoming meetings into the Org busy-time file.
With QUIET, say nothing unless something changed."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "This importer reads the macOS calendar database"))
  (let* ((events (seq-remove
                  (lambda (event)
                    (string-prefix-p my/calendar-block-prefix (car event)))
                  (my/calendar--parse (my/calendar--run))))
         (changed (my/calendar--write (my/calendar--render events))))
    (setq my/calendar--last-refresh (current-time))
    (unless (and quiet (not changed))
      (message "Calendar: %d meeting%s%s" (length events)
               (if (= (length events) 1) "" "s")
               (if changed ", file updated" ", unchanged")))
    (run-hook-with-args 'my/calendar-after-refresh-functions events)
    (length events)))

(defun my/calendar--refresh-before-scheduling (&rest _)
  "Refresh meetings before the scheduler runs, never blocking scheduling."
  (when (and (eq system-type 'darwin)
             (or (null my/calendar--last-refresh)
                 (> (float-time (time-since my/calendar--last-refresh))
                    my/calendar-min-interval)))
    (condition-case err
        (my/calendar-refresh t)
      (error (message "Calendar import skipped: %s" (error-message-string err))))))

(dolist (command '(org-auto-scheduler-schedule-tasks
                   org-auto-scheduler-review-and-apply))
  (advice-add command :before #'my/calendar--refresh-before-scheduling))

(global-set-key (kbd "C-c A c") #'my/calendar-refresh)

(provide 'my-org-calendar)
;;; my-org-calendar.el ends here
