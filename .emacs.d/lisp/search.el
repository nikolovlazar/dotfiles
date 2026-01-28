;; Search and navigation tools

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings)

  ;; Clear default ignore list first
  (setq rg-ignore-case 'smart)

  ;; Define custom search that includes hidden files
  (rg-define-search rg-project-with-hidden
    "Search project including dotfiles, ignoring .git and node_modules"
    :query ask
    :format regexp
    :files "everything"
    :flags ("--hidden" "--glob=!.git/" "--glob=!node_modules/")
    :dir project
    :menu ("Search" "p" "Project with dotfiles")))

;; Keybinding for the custom search
(global-set-key (kbd "C-c s") 'rg-project-with-hidden)

(provide 'search)
