;;; my-theme.el --- Tinted Modus theme switching -*- lexical-binding: t; -*-

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-operandi t)

(defun toggle-themes ()
  "Toggle directly between the light and dark tinted Modus themes."
  (interactive)
  (let ((next-theme (if (memq 'modus-operandi custom-enabled-themes)
                        'modus-vivendi-tinted
                      'modus-operandi)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme next-theme t)))

(defun my/modus-theme-variant (_original &optional _no-confirm _no-enable)
  "Use the two-theme toggle for the built-in variant command."
  (toggle-themes))

(advice-add 'theme-choose-variant :around #'my/modus-theme-variant)
(global-set-key (kbd "C-c t") #'toggle-themes)
(provide 'my-theme)
;;; my-theme.el ends here
