;;; keybindings.el --- All custom keybindings in one place -*- lexical-binding: t; -*-

;; Search
(global-set-key (kbd "C-c g") #'consult-ripgrep) ;; Grep with live preview

;; Org mode
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Org roam
(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n p") #'my/org-roam-node-from-todo)

(provide 'keybindings)
