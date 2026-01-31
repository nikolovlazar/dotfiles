;;; leader.el --- SPC leader key system -*- lexical-binding: t; -*-

;; Spacemacs/Doom-style leader key using general.el
;; SPC in normal/visual mode, M-SPC as fallback in insert mode

(use-package general
  :ensure t
  :demand t
  :config
  ;; Main leader key definer
  (general-create-definer my/leader-keys
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  ;; Local leader for mode-specific bindings (comma in normal mode)
  (general-create-definer my/local-leader-keys
    :states '(normal visual motion)
    :keymaps 'override
    :prefix ","
    :global-prefix "M-,")

  ;; === TOP LEVEL ===
  (my/leader-keys
    "x" '(execute-extended-command :which-key "M-x")
    "SPC" '(find-file :which-key "Find file")
    "," '(consult-buffer :which-key "Switch buffer")
    ":" '(eval-expression :which-key "Eval")
    ";" '(comment-dwim :which-key "Comment")
    "u" '(universal-argument :which-key "Universal arg"))

  ;; === FILES (f) ===
  (my/leader-keys
    "f" '(:ignore t :which-key "Files")
    "ff" '(find-file :which-key "Find file")
    "fr" '(consult-recent-file :which-key "Recent")
    "fs" '(save-buffer :which-key "Save")
    "fS" '(write-file :which-key "Save as")
    "fd" '(my/dirvish-here :which-key "Dirvish here")
    "fD" '(dirvish :which-key "Dirvish")
    "fi" '(my/view-image :which-key "View image"))

  ;; === BUFFERS (b) ===
  (my/leader-keys
    "b" '(:ignore t :which-key "Buffers")
    "bb" '(consult-buffer :which-key "Switch")
    "bd" '(kill-current-buffer :which-key "Kill")
    "bD" '(kill-buffer-and-window :which-key "Kill+window")
    "bn" '(next-buffer :which-key "Next")
    "bp" '(previous-buffer :which-key "Previous")
    "br" '(revert-buffer :which-key "Revert")
    "bs" '(scratch-buffer :which-key "Scratch"))

  ;; === SEARCH (s) ===
  (my/leader-keys
    "s" '(:ignore t :which-key "Search")
    "ss" '(consult-line :which-key "Buffer")
    "sS" '(consult-line-multi :which-key "All buffers")
    "sg" '(consult-ripgrep :which-key "Ripgrep")
    "sp" '(rg-project-with-hidden :which-key "Project")
    "si" '(consult-imenu :which-key "Imenu"))

  ;; === PROJECT (p) ===
  (my/leader-keys
    "p" '(:ignore t :which-key "Project")
    "pp" '(project-switch-project :which-key "Switch")
    "pf" '(project-find-file :which-key "Find file")
    "pd" '(project-dired :which-key "Dired")
    "pb" '(project-switch-to-buffer :which-key "Buffer")
    "pk" '(project-kill-buffers :which-key "Kill buffers")
    "ps" '(consult-ripgrep :which-key "Search")
    "pc" '(project-compile :which-key "Compile")
    "pe" '(project-eshell :which-key "Eshell"))

  ;; === GIT (g) ===
  (my/leader-keys
    "g" '(:ignore t :which-key "Git")
    "gg" '(magit-status :which-key "Status")
    "gb" '(magit-blame :which-key "Blame")
    "gl" '(magit-log-current :which-key "Log")
    "gL" '(magit-log-buffer-file :which-key "Log file")
    "gd" '(magit-diff-buffer-file :which-key "Diff file")
    "gf" '(magit-file-dispatch :which-key "File actions")
    "gc" '(magit-commit :which-key "Commit")
    "gp" '(magit-push :which-key "Push")
    "gF" '(magit-pull :which-key "Pull"))

  ;; === NOTES - ORG-ROAM (n) ===
  (my/leader-keys
    "n" '(:ignore t :which-key "Notes")
    "nf" '(org-roam-node-find :which-key "Find")
    "ni" '(org-roam-node-insert :which-key "Insert")
    "nl" '(org-roam-buffer-toggle :which-key "Backlinks")
    "ng" '(org-roam-graph :which-key "Graph")
    "nc" '(org-roam-capture :which-key "Capture")
    "np" '(my/org-roam-node-from-todo :which-key "From TODO")
    "nj" '(org-roam-dailies-capture-today :which-key "Daily")
    "nJ" '(org-journal-new-entry :which-key "Journal"))

  ;; === ORG (o) ===
  (my/leader-keys
    "o" '(:ignore t :which-key "Org")
    "oa" '(org-agenda :which-key "Agenda")
    "oc" '(org-capture :which-key "Capture")
    "ol" '(org-store-link :which-key "Store link")
    "oL" '(org-insert-link :which-key "Insert link")
    "ot" '(org-todo :which-key "TODO")
    "os" '(org-schedule :which-key "Schedule")
    "od" '(org-deadline :which-key "Deadline")
    "or" '(org-refile :which-key "Refile")
    "oe" '(org-export-dispatch :which-key "Export"))

  ;; === WINDOWS (w) ===
  (my/leader-keys
    "w" '(:ignore t :which-key "Windows")
    "ww" '(ace-window :which-key "Ace window")
    "wd" '(delete-window :which-key "Delete")
    "wD" '(delete-other-windows :which-key "Delete others")
    "ws" '(split-window-below :which-key "Split horiz")
    "wv" '(split-window-right :which-key "Split vert")
    "wh" '(windmove-left :which-key "Left")
    "wj" '(windmove-down :which-key "Down")
    "wk" '(windmove-up :which-key "Up")
    "wl" '(windmove-right :which-key "Right")
    "w=" '(balance-windows :which-key "Balance")
    "wm" '(delete-other-windows :which-key "Maximize"))

  ;; === HELP (h) ===
  (my/leader-keys
    "h" '(:ignore t :which-key "Help")
    "hf" '(describe-function :which-key "Function")
    "hv" '(describe-variable :which-key "Variable")
    "hk" '(describe-key :which-key "Key")
    "hm" '(describe-mode :which-key "Mode")
    "hb" '(describe-bindings :which-key "Bindings")
    "hi" '(info :which-key "Info")
    "hp" '(describe-package :which-key "Package")
    "hw" '(which-key-show-top-level :which-key "Which-key"))

  ;; === TOGGLE (t) ===
  (my/leader-keys
    "t" '(:ignore t :which-key "Toggle")
    "tl" '(display-line-numbers-mode :which-key "Line numbers")
    "tr" '(toggle-truncate-lines :which-key "Truncate")
    "tw" '(whitespace-mode :which-key "Whitespace")
    "tf" '(toggle-frame-fullscreen :which-key "Fullscreen")
    "tt" '(consult-theme :which-key "Theme")
    "te" '(elfeed :which-key "Elfeed"))

  ;; === QUIT (q) ===
  (my/leader-keys
    "q" '(:ignore t :which-key "Quit")
    "qq" '(save-buffers-kill-emacs :which-key "Quit")
    "qQ" '(kill-emacs :which-key "Quit (no save)")
    "qr" '(restart-emacs :which-key "Restart")))

;; === MAGIT: LAZYGIT-STYLE KEYBINDINGS ===
;; Space = stage in magit (like lazygit), use M-SPC for leader in magit buffers
(with-eval-after-load 'magit
  (evil-define-key 'normal magit-status-mode-map
    (kbd "SPC") 'magit-stage-file         ; Space = stage (like lazygit)
    (kbd "u")   'magit-unstage-file       ; u = unstage
    (kbd "c")   'magit-commit             ; c = commit menu
    (kbd "a")   'magit-stage-modified     ; a = stage all modified
    (kbd "d")   'magit-discard            ; d = discard
    (kbd "p")   'magit-pull               ; p = pull
    (kbd "P")   'magit-push)              ; P = push

  ;; In diff/hunk view: Space stages the hunk
  (evil-define-key 'normal magit-diff-mode-map
    (kbd "SPC") 'magit-stage))

(provide 'leader)
