;;; keybindings.el --- Fallback C-c keybindings -*- lexical-binding: t; -*-

;; These bindings are kept as fallback during transition to SPC leader keys.
;; Primary bindings are now in leader.el (SPC-based).
;; Remove these once muscle memory has transitioned.

;; Search (SPC s g)
(global-set-key (kbd "C-c g") #'consult-ripgrep)

;; Org mode (SPC o)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Org roam (SPC n)
(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n p") #'my/org-roam-node-from-todo)

(provide 'keybindings)
