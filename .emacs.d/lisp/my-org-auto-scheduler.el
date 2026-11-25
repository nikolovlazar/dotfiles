;;; my-org-auto-scheduler.el --- Trial of org-auto-scheduler -*- lexical-binding: t; -*-

;; Auto-schedules Org TODOs into free time.  Only tasks tagged :AUTOSCH: with
;; an Effort property are touched.  It rewrites SCHEDULED lines and adds
;; placeholder subheadings, so keep the Org directory under version control
;; to review (`git diff') or undo (`git checkout .') what it changed.

;; org-auto-scheduler declares only log4e, but soft-requires the rest:
;; org-focus-hud (Focus HUD) and transient (dispatch menu).  Meetings come
;; from my-org-calendar.el, so the optional org-caldav sync stays off.
(let ((missing (seq-remove #'package-installed-p
                           '(log4e))))
  (when missing
    ;; A stale archive index can name tarballs MELPA no longer serves.
    (package-refresh-contents)
    (mapc #'package-install missing)))
;; Checked once while org-auto-scheduler loads, so it must be installed first.
(unless (package-installed-p 'org-focus-hud)
  (package-vc-install '(org-focus-hud
                        :url "https://github.com/SSD2019/org-focus-hud")))
(unless (package-installed-p 'org-auto-scheduler)
  ;; Its dispatch menu is a `transient-define-prefix' inside
  ;; `with-eval-after-load', so the macro only expands when Transient is
  ;; loaded at compile time; otherwise loading the package fails.
  (require 'transient)
  (package-vc-install '(org-auto-scheduler
                        :url "https://github.com/SSD2019/org-auto-scheduler")))

;; Upstream bugs fixed locally (see patches/): `evil-define-key' called with a
;; loop variable, which breaks loading under Evil, and a Transient menu that
;; fails when Transient is absent at compile time.  Must run before either
;; package is loaded.
(require 'my-package-patches)
(mapc #'my/apply-package-patch '(org-focus-hud org-auto-scheduler))

;; The package lacks autoload cookies for its main commands.
(dolist (command '(org-auto-scheduler-review-and-apply
                   org-auto-scheduler-schedule-tasks
                   org-auto-scheduler-dispatch))
  (autoload command "org-auto-scheduler" nil t))

;; Loaded on first use through the package autoloads, keeping startup fast.
(with-eval-after-load 'org-auto-scheduler
  (setq org-auto-scheduler-state-weights '(("TODO" . 0) ("PROGRESS" . 2))
        org-auto-scheduler-waiting-states '("WAITING")
        org-auto-scheduler-start-time "09:00"
        org-auto-scheduler-end-time "17:00"
        org-auto-scheduler-excluded-days '(0 6) ; Sunday and Saturday
        ;; The package's example blocks reserve tags this setup doesn't use.
        org-auto-scheduler-time-blocks nil
        ;; Manual runs only while evaluating.
        org-auto-scheduler-background-enabled nil
        ;; Unconfigured, org-caldav talks to a placeholder server and fails
        ;; after every run.
        org-auto-scheduler-sync-caldav nil))

;; The report is a wide Org table.  Prose mode would wrap it at 80 columns,
;; which shreds it, so show that buffer unwrapped (scroll sideways).
(defun my/org-auto-scheduler-report-layout ()
  "Undo prose wrapping in the scheduler report buffer."
  (when (equal (buffer-name) "*Org Auto Scheduler Report*")
    (visual-line-mode -1)
    (when (fboundp 'visual-fill-column-mode) (visual-fill-column-mode -1))
    (setq-local truncate-lines t)))
(add-hook 'org-mode-hook #'my/org-auto-scheduler-report-layout 90)

;; `C-c a' is the agenda, so the scheduler lives under `C-c A'.
(global-set-key (kbd "C-c A r") #'org-auto-scheduler-review-and-apply)
(global-set-key (kbd "C-c A s") #'org-auto-scheduler-schedule-tasks)
(global-set-key (kbd "C-c A d") #'org-auto-scheduler-dispatch)
(global-set-key (kbd "C-c A f") #'org-auto-scheduler-focus)

(provide 'my-org-auto-scheduler)
;;; my-org-auto-scheduler.el ends here
