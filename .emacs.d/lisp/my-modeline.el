;;; my-modeline.el --- doom-modeline with a quiet Evil state tag -*- lexical-binding: t; -*-

;; The bar keeps the theme's neutral colour: a tinted bar is too loud for a
;; writing buffer.  Only the state tag changes, and only away from normal
;; state, so a buffer at rest looks like plain stock doom-modeline.

(setq doom-modeline-icon nil             ; no icon fonts in a terminal bar
      doom-modeline-major-mode-icon nil  ; the mode name is enough
      doom-modeline-minor-modes nil)     ; which-key lists them anyway

(require 'doom-modeline)
(doom-modeline-mode 1)

(defconst my/evil-tag-ansi
  '((insert . "green")
    (visual . "yellow")
    (replace . "red")
    (operator . "magenta")
    (motion . "cyan"))
  "Terminal palette slot for each Evil state tag.
Normal and Emacs state are absent on purpose: the tag stays neutral so an
idle buffer shows no colour at all.  Names rather than hex values, so the
chip follows whatever palette the terminal is using.")

(defun my/evil-tag-apply (&rest _)
  "Colour the state tag for non-normal states, in terminal palette slots."
  ;; These faces are themed, so a plain `set-face-attribute' is overridden at
  ;; redisplay; the override spec is the only thing that outranks a theme.
  (face-spec-set 'doom-modeline-evil-normal-state
                 '((t (:background "unspecified-bg" :foreground "unspecified-fg"
                      :weight bold)))
                 'face-override-spec)
  (dolist (entry my/evil-tag-ansi)
    (face-spec-set (intern (format "doom-modeline-evil-%s-state" (car entry)))
                   `((t (:background ,(cdr entry) :foreground "black" :weight bold)))
                   'face-override-spec)))

(my/evil-tag-apply)

;; Enabling or disabling a theme resets face specs; reapply afterwards.
(add-hook 'enable-theme-functions #'my/evil-tag-apply)
(when (boundp 'disable-theme-functions)
  (add-hook 'disable-theme-functions #'my/evil-tag-apply))

(provide 'my-modeline)
;;; my-modeline.el ends here
