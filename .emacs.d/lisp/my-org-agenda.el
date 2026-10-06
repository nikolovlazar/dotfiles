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
(provide 'my-org-agenda)
;;; my-org-agenda.el ends here
