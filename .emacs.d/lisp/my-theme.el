;;; my-theme.el --- Tinted Modus theme switching -*- lexical-binding: t; -*-

;; In terminal frames (e.g. inside Rex) no theme is forced: the terminal
;; owns the colors and background.  GUI frames still get Modus.

(defvar my/gui-theme 'modus-operandi
  "Theme used in graphical frames.")

(defun my/apply-theme (&optional frame)
  "Load the GUI theme for graphical FRAME; leave terminal frames alone."
  (with-selected-frame (or frame (selected-frame))
    (if (display-graphic-p)
        (unless custom-enabled-themes
          (load-theme my/gui-theme t))
      (mapc #'disable-theme custom-enabled-themes)
      (set-face-background 'default "unspecified-bg" (selected-frame)))))

(defun toggle-themes ()
  "Toggle between the light and dark tinted Modus themes (GUI only)."
  (interactive)
  (if (not (display-graphic-p))
      (message "Terminal frame: colors are handled by the terminal (Rex)")
    (setq my/gui-theme (if (memq 'modus-operandi custom-enabled-themes)
                           'modus-vivendi-tinted
                         'modus-operandi))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme my/gui-theme t)))

(defun my/modus-theme-variant (_original &optional _no-confirm _no-enable)
  "Use the two-theme toggle for the built-in variant command."
  (toggle-themes))

(advice-add 'theme-choose-variant :around #'my/modus-theme-variant)
(global-set-key (kbd "C-c t") #'toggle-themes)

(add-hook 'after-make-frame-functions #'my/apply-theme)
(my/apply-theme)
(provide 'my-theme)
;;; my-theme.el ends here
