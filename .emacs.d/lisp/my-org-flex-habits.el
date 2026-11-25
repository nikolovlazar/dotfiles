;;; my-org-flex-habits.el --- Habits that move around meetings -*- lexical-binding: t; -*-

;; A flexible habit has a window instead of a fixed time.  Define it in
;; habits.org, with no TODO keyword and no SCHEDULED line:
;;
;;   * Lunch
;;   :PROPERTIES:
;;   :FLEX_WINDOW: 12:00-14:00
;;   :FLEX_MAX: 1:00
;;   :FLEX_MIN: 0:30
;;   :FLEX_DAYS: Mon Tue Wed Thu Fri
;;   :FLEX_OVERLAP: All Hands
;;   :END:
;;
;; FLEX_OVERLAP is an optional regexp: meetings whose title matches it don't
;; count as busy for that habit (you can eat during them).
;;
;; After each calendar import, every day gets the earliest FLEX_MAX gap
;; between meetings inside the window; failing that, the longest gap of at
;; least FLEX_MIN; failing that, the start of the window, with a warning.
;; The result goes to a generated file in the agenda, where the scheduler
;; treats it as busy time, so tasks never push a habit around.  Habits earlier
;; in the file are placed first.

(require 'cl-lib)
(require 'subr-x)
(require 'my-org-calendar)

(declare-function org-map-entries "org")
(declare-function org-entry-get "org")
(declare-function org-get-heading "org")
(declare-function org-duration-to-minutes "org-duration")
(declare-function org-auto-scheduler-clear-repeater-cache "org-auto-scheduler")

(defcustom my/flex-source-file nil
  "Org file defining flexible habits, or nil for habits.org in `org-directory'."
  :type '(choice (const nil) file)
  :group 'my/calendar)

(defun my/flex-file ()
  "Return the generated file, next to the imported meetings."
  (expand-file-name "flex.org" (file-name-directory (my/calendar-file))))

(defun my/flex--source-file ()
  (or my/flex-source-file
      (expand-file-name "habits.org" (or (bound-and-true-p org-directory) "~/org"))))

(defun my/flex--habits ()
  "Return a plist for each flexible habit in the source file."
  (let ((file (my/flex--source-file))
        habits)
    (when (file-exists-p file)
      (require 'org)
      (org-map-entries
       (lambda ()
         (when-let* ((window (org-entry-get nil "FLEX_WINDOW")))
           (unless (string-match "\\`\\([0-9]+\\):\\([0-9]\\{2\\}\\)-\\([0-9]+\\):\\([0-9]\\{2\\}\\)\\'" window)
             (user-error "FLEX_WINDOW must look like 12:00-14:00, not %s" window))
           (let* ((n (mapcar (lambda (i) (string-to-number (match-string i window)))
                             '(1 2 3 4)))
                  (max (* 60 (org-duration-to-minutes
                              (or (org-entry-get nil "FLEX_MAX") "1:00"))))
                  (min (if-let* ((value (org-entry-get nil "FLEX_MIN")))
                           (* 60 (org-duration-to-minutes value))
                         max)))
             (push (list :title (substring-no-properties (org-get-heading t t t t))
                         :from (list (nth 0 n) (nth 1 n))
                         :to (list (nth 2 n) (nth 3 n))
                         :min min :max max
                         :overlap (org-entry-get nil "FLEX_OVERLAP")
                         :days (split-string (or (org-entry-get nil "FLEX_DAYS")
                                                 "Mon Tue Wed Thu Fri")))
                   habits))))
       nil (list file)))
    (nreverse habits)))

(defun my/flex--slot (busy lo hi min max)
  "Return (START . END) between LO and HI avoiding BUSY, or nil.
All values are in seconds; BUSY is a list of (START . END).  Prefer the
earliest gap of MAX, else the longest gap of at least MIN."
  (let ((cursor lo) gaps)
    (dolist (b (sort (copy-sequence busy) (lambda (a b) (< (car a) (car b)))))
      (when (and (< (car b) hi) (> (cdr b) cursor))
        (when (> (car b) cursor)
          (push (cons cursor (car b)) gaps))
        (setq cursor (cdr b))))
    (when (< cursor hi)
      (push (cons cursor hi) gaps))
    (setq gaps (nreverse gaps))
    (if-let* ((full (cl-find-if (lambda (g) (>= (- (cdr g) (car g)) max)) gaps)))
        (cons (car full) (+ (car full) max))
      (let (best)
        (dolist (g gaps best)
          (let ((length (- (cdr g) (car g))))
            (when (and (>= length min)
                       (or (null best) (> length (- (cdr best) (car best)))))
              (setq best g))))))))

(defun my/flex--planned ()
  "Return (TITLE START END), in seconds, for the blocks already planned."
  (let ((file (my/flex-file))
        blocks)
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (while (re-search-forward
                "^\\* \\(.*\\)\n\\(?::.*\n\\)*<\\([-0-9]+\\) [^ >]+ \\([0-9:]+\\)-\\([0-9:]+\\)>" nil t)
          (let ((title (match-string 1))
                (date (match-string 2))
                (from (match-string 3))
                (to (match-string 4)))
            (push (list title
                        (float-time (my/calendar--time date from))
                        (float-time (my/calendar--time date to)))
                  blocks)))))
    blocks))

(defun my/flex--busy (meetings overlap)
  "Return (START . END) for MEETINGS, leaving out titles matching OVERLAP."
  (delq nil (mapcar (lambda (m)
                      (unless (and overlap
                                   (let ((case-fold-search t))
                                     (string-match-p overlap (car m))))
                        (cons (float-time (nth 1 m)) (float-time (nth 2 m)))))
                    meetings)))

(defun my/flex--plan (habits meetings planned now days)
  "Return ((TITLE START END) ...) and warnings for HABITS over DAYS days.
MEETINGS is a list of (TITLE START END) times, PLANNED the previous result,
and NOW the current time in seconds.  A block already under way is kept."
  (let ((today (decode-time now))
        blocks warnings)
    (dotimes (offset days)
      (let (taken)
        (dolist (habit habits)
          (cl-flet ((at (clock)
                      (float-time (encode-time 0 (nth 1 clock) (nth 0 clock)
                                               (+ (nth 3 today) offset)
                                               (nth 4 today) (nth 5 today)))))
            (let* ((title (plist-get habit :title))
                   (lo (at (plist-get habit :from)))
                   (hi (at (plist-get habit :to)))
                   (day (let ((system-time-locale "C"))
                          (format-time-string "%a" lo)))
                   (started (cl-find-if (lambda (p)
                                          (and (equal (car p) title)
                                               (>= (nth 1 p) lo) (< (nth 1 p) hi)
                                               (<= (nth 1 p) now)))
                                        planned))
                   (earliest (max lo (* 300 (ceiling now 300)))))
              (when (member day (plist-get habit :days))
                (cond
                 (started
                  (push started blocks)
                  (push (cons (nth 1 started) (nth 2 started)) taken))
                 ;; Too late today to fit even the shortest version.
                 ((and (zerop offset) (> (+ earliest (plist-get habit :min)) hi)))
                 (t
                  (let ((slot (my/flex--slot
                               (append taken (my/flex--busy meetings
                                                            (plist-get habit :overlap)))
                               earliest hi (plist-get habit :min)
                               (plist-get habit :max))))
                    (unless slot
                      (setq slot (cons earliest (min hi (+ earliest (plist-get habit :max)))))
                      (push (format "%s on %s" title
                                    (format-time-string "%a %b %d" lo))
                            warnings))
                    (push (list title (car slot) (cdr slot)) blocks)
                    (push slot taken))))))))))
    (cons (nreverse blocks) (nreverse warnings))))

(defun my/flex--render (blocks)
  "Return the Org text for BLOCKS."
  (concat "#+title: Flexible habits\n"
          "# Generated by my-org-flex-habits.el from habits.org; do not edit.\n\n"
          (mapconcat
           (lambda (block)
             (let ((start (seconds-to-time (nth 1 block))))
               (format "* %s\n:PROPERTIES:\n:ID:       flex-%s-%s\n:NON_BLOCKING: nil\n:END:\n%s\n"
                       (nth 0 block)
                       (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase (nth 0 block)))
                       (format-time-string "%Y%m%d" start)
                       (my/calendar--timestamp start (seconds-to-time (nth 2 block))))))
           blocks "")))

(defun my/flex-update (meetings)
  "Replan flexible habits around MEETINGS and write the generated file."
  (condition-case err
      (let ((habits (my/flex--habits)))
        (when (or habits (file-exists-p (my/flex-file)))
          (let ((result (my/flex--plan habits meetings (my/flex--planned)
                                       (float-time) my/calendar-days)))
            (when (my/calendar--write (my/flex--render (car result)) (my/flex-file))
              (when (fboundp 'org-auto-scheduler-clear-repeater-cache)
                (org-auto-scheduler-clear-repeater-cache)))
            (when (cdr result)
              (display-warning
               'flex-habits
               (format "No free gap, kept the usual time: %s"
                       (string-join (cdr result) ", ")))))))
    (error (message "Flexible habits skipped: %s" (error-message-string err)))))

(add-hook 'my/calendar-after-refresh-functions #'my/flex-update)

(provide 'my-org-flex-habits)
;;; my-org-flex-habits.el ends here
