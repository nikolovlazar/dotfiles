;;; files.el --- File management with dirvish -*- lexical-binding: t; -*-

;; Load dirvish
(require 'dirvish)

;; Enable dirvish UI globally (makes dired show dirvish interface)
(dirvish-override-dired-mode 1)

;; Configuration
(setq dirvish-attributes '(file-size collapse subtree-state vc-state git-msg))
(setq dirvish-mode-line-format
      '(:left (sort file-time " " file-size symlink)
        :right (omit yank index)))

;; C-x d opens dirvish immediately at project root (or current dir)
(defun my/dirvish-here ()
  "Open dirvish at project root, or current directory if not in a project."
  (interactive)
  (dirvish (or (and (project-current) (project-root (project-current)))
               default-directory)))

(global-set-key (kbd "C-x d") #'my/dirvish-here)

;;; Image viewing configuration

;; Open images in browser (best for SVG support)
(defun my/view-image ()
  "View image file in a browser (supports SVG)."
  (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (let* ((url (concat "file://" (expand-file-name file)))
             ;; Try to find an installed browser
             (browser (or (executable-find "brave")
                         (executable-find "google-chrome")
                         (executable-find "firefox")
                         "/Applications/Brave Browser.app"
                         "/Applications/Google Chrome.app"
                         "/Applications/Firefox.app"
                         "/Applications/Safari.app")))
        (if (and browser (file-exists-p browser))
            (start-process "open-image" nil "open" "-a" browser url)
          (start-process "open-image" nil "open" url))
        (message "Opened %s in browser" (file-name-nondirectory file))))))

;; Auto-open images externally when opened in terminal Emacs
(defun my/auto-view-image-in-terminal ()
  "Automatically open image files externally when in terminal."
  (when (and (display-graphic-p) (not (display-graphic-p)))
    (my/view-image)))

;; Ensure image files trigger external viewer in terminal
(add-to-list 'auto-mode-alist '("\\.\\(png\\|jpe?g\\|gif\\|svg\\|webp\\)\\'" . my/view-image))

;; Keybinding to manually open image
(global-set-key (kbd "C-c i") #'my/view-image)

(provide 'files)
