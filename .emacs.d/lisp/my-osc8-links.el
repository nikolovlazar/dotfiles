;;; my-osc8-links.el --- Cmd-clickable links in pretty tables -*- lexical-binding: t; -*-

;; TblPretty replaces `[[url][label]]' with a display-only `label', so the
;; terminal never sees the URL and Ghostty cannot hover/click it.  After each
;; redisplay, rewrite the visible link labels in place wrapped in OSC 8
;; hyperlink sequences.  Same characters, same colours, plus a link attribute.

(require 'cl-lib)

(defgroup my-osc8-links nil "OSC 8 links for display-only text." :group 'tools)

(defcustom my-osc8-links-url-regexp "\\`\\(?:https?://\\|mailto:\\)"
  "Only help-echo values matching this are turned into terminal links."
  :type 'regexp)

(defvar my-osc8-links--timer nil)

(defun my-osc8-links--attr (faces attr)
  "First specified ATTR among FACES (face symbol, plist, or list of those)."
  (let ((faces (if (and (consp faces) (keywordp (car faces))) (list faces)
                 (if (listp faces) faces (list faces))))
        (found nil))
    (while (and faces (not found))
      (let* ((f (pop faces))
             (v (cond ((keywordp f) nil)
                      ((consp f) (plist-get f (intern (format ":%s" attr))))
                      ((facep f) (face-attribute f attr nil t)))))
        (when (and v (not (eq v 'unspecified))) (setq found v))))
    found))

(defun my-osc8-links--rgb (color)
  (when-let* ((c (and (stringp color) (color-values color))))
    (format "%d;%d;%d" (/ (nth 0 c) 257) (/ (nth 1 c) 257) (/ (nth 2 c) 257))))

(defun my-osc8-links--sgr (faces)
  "SGR escape reproducing FACES (plus the default background)."
  (let* ((fg (or (my-osc8-links--attr faces :foreground) (face-foreground 'default)))
         (bg (or (my-osc8-links--attr faces :background) (face-background 'default)))
         (weight (my-osc8-links--attr faces :weight))
         (slant (my-osc8-links--attr faces :slant))
         (ul (my-osc8-links--attr faces :underline))
         (parts nil))
    (when-let* ((x (my-osc8-links--rgb fg))) (push (concat "38;2;" x) parts))
    (when-let* ((x (my-osc8-links--rgb bg))) (push (concat "48;2;" x) parts))
    (when (memq weight '(bold semi-bold extra-bold ultra-bold heavy)) (push "1" parts))
    (when (memq slant '(italic oblique)) (push "3" parts))
    (when ul (push "4" parts))
    (concat "\e[0;" (mapconcat #'identity (nreverse parts) ";") "m")))

(defun my-osc8-links--string-runs (str)
  "Return (COLUMN URL SGR TEXT) for each linked run in single-row STR."
  (let ((i 0) (n (length str)) runs)
    (while (< i n)
      (let ((url (get-text-property i 'help-echo str))
            (next (next-single-property-change i 'help-echo str n)))
        (when (and (stringp url) (string-match-p my-osc8-links-url-regexp url))
          (let ((seg (substring str i next)))
            (push (list (string-width (substring str 0 i)) url
                        (my-osc8-links--sgr (get-text-property i 'face str))
                        (substring-no-properties seg))
                  runs)))
        (setq i next)))
    (nreverse runs)))

(defun my-osc8-links--window-links (win)
  "Return (ROW COL URL SGR TEXT) for linked text drawn by overlays in WIN."
  (let* ((edges (window-edges win t))
         (left (nth 0 edges)) (top (nth 1 edges))
         (beg (window-start win)) (end (window-end win t))
         links)
    (with-current-buffer (window-buffer win)
      (dolist (ov (overlays-in beg end))
        (let ((disp (overlay-get ov 'display)))
          (when (and (stringp disp) (overlay-get ov 'markdown-table-wrap-pretty-display)
                     (text-property-any 0 (length disp) 'mouse-face 'highlight disp))
            (when-let* (((not (invisible-p (overlay-start ov))))
                        ((pos-visible-in-window-p (overlay-start ov) win))
                        (pos (posn-at-point (overlay-start ov) win))
                        (xy (posn-x-y pos)))
              (let ((row (+ top (cdr xy))) (col0 (+ left (car xy))) (k 0) (line-start 0))
                (dolist (line (split-string disp "\n"))
                  (let ((sub (substring disp line-start (+ line-start (length line)))))
                    (dolist (r (my-osc8-links--string-runs sub))
                      (push (cons (+ row k) (cons (+ col0 (car r)) (cdr r))) links)))
                  (setq line-start (+ line-start (length line) 1) k (1+ k)))))))))
    (nreverse links)))

(defun my-osc8-links--emit ()
  (setq my-osc8-links--timer nil)
  (when (and (not noninteractive)
             (not (display-graphic-p))
             (not (minibuffer-window-active-p (minibuffer-window)))
             (frame-terminal (selected-frame)))
    (let ((out "") (win (selected-window)))
      (dolist (w (window-list (selected-frame) 'no-minibuf))
        (when (buffer-local-value 'markdown-table-wrap-pretty-mode (window-buffer w))
          (dolist (l (my-osc8-links--window-links w))
            (cl-destructuring-bind (row col url sgr text) l
              (setq out (concat out
                                (format "\e[%d;%dH%s\e]8;id=%x;%s\e\\%s\e]8;;\e\\"
                                        (1+ row) (1+ col) sgr
                                        (abs (sxhash-equal url)) url text)))))))
      (unless (string-empty-p out)
        (send-string-to-terminal (concat "\e7" out "\e[0m\e8"))))))

(defun my-osc8-links--schedule (&rest _)
  (unless (or my-osc8-links--timer (display-graphic-p))
    (setq my-osc8-links--timer
          (run-with-idle-timer 0.05 nil #'my-osc8-links--emit))))

(define-minor-mode my-osc8-links-mode
  "Emit OSC 8 hyperlinks for pretty-table links in terminal frames."
  :global t
  (if my-osc8-links-mode
      (progn (add-hook 'post-command-hook #'my-osc8-links--schedule)
             (add-hook 'window-scroll-functions #'my-osc8-links--schedule)
             (add-hook 'window-size-change-functions #'my-osc8-links--schedule))
    (remove-hook 'post-command-hook #'my-osc8-links--schedule)
    (remove-hook 'window-scroll-functions #'my-osc8-links--schedule)
    (remove-hook 'window-size-change-functions #'my-osc8-links--schedule)))

(provide 'my-osc8-links)
;;; my-osc8-links.el ends here
