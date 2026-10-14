;;; my-mermaid.el --- Mermaid previews in terminal Emacs -*- lexical-binding: t; -*-

(require 'mermaid-mode)
(require 'kitty-graphics)
(require 'org)

;; Install the CLI once with:
;; PUPPETEER_SKIP_DOWNLOAD=true npm --prefix ~/.emacs.d/mermaid-tools install @mermaid-js/mermaid-cli
(let ((bin (expand-file-name "mermaid-tools/node_modules/.bin" user-emacs-directory)))
  (add-to-list 'exec-path bin))
(setq mermaid-mmdc-location
      (expand-file-name "mermaid-tools/node_modules/.bin/mmdc" user-emacs-directory))
;; Use the existing Chrome installation instead of downloading another browser.
(when (file-executable-p "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
  (setenv "PUPPETEER_EXECUTABLE_PATH"
          "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))
(add-to-list 'auto-mode-alist '("\\.\\(?:mmd\\|mermaid\\)\\'" . mermaid-mode))
(add-to-list 'org-src-lang-modes '("mermaid" . mermaid))
(with-eval-after-load 'markdown-mode
  (add-to-list 'markdown-code-lang-modes '("mermaid" . mermaid-mode)))
(kitty-graphics-setup)

(defun my/mermaid-source-at-point ()
  "Return Mermaid source from the region, current block, or Mermaid buffer."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((derived-mode-p 'mermaid-mode)
    (buffer-substring-no-properties (point-min) (point-max)))
   ((derived-mode-p 'org-mode)
    (let ((element (org-element-context)))
      (unless (and (eq (org-element-type element) 'src-block)
                   (equal (org-element-property :language element) "mermaid"))
        (user-error "Place point inside a Mermaid source block"))
      (org-element-property :value element)))
   ((derived-mode-p 'markdown-mode)
    (font-lock-ensure)
    (let ((bounds (markdown-get-enclosing-fenced-block-construct)))
      (unless bounds (user-error "Place point inside a fenced Mermaid block"))
      (save-excursion
        (goto-char (car bounds))
        (unless (looking-at "[ \t]*\\(?:`\\{3,\\}\\|~\\{3,\\}\\)[ \t]*mermaid[ \t]*$")
          (user-error "This code block is not Mermaid"))
        (let ((beg (line-beginning-position 2))
              (end (progn (goto-char (cadr bounds)) (line-beginning-position))))
          (buffer-substring-no-properties beg end)))))
   (t (user-error "Select Mermaid source, or use a Mermaid, Markdown or Org buffer"))))

(defvar-local my/mermaid-preview-file nil)
(defvar-local my/mermaid-preview-columns nil)

(define-derived-mode my/mermaid-preview-mode special-mode "Mermaid preview"
  "View Mermaid images at the available window width, without prose wrapping."
  (when (bound-and-true-p visual-fill-column-mode)
    (visual-fill-column-mode -1))
  (visual-line-mode -1)
  (auto-fill-mode -1)
  (setq-local truncate-lines t
              word-wrap nil
              left-margin-width 0
              right-margin-width 0))

(defun my/mermaid-fit-preview (&optional window)
  "Fit the preview image to WINDOW's full text width."
  (let ((window (or window (selected-window))))
    (with-current-buffer (window-buffer window)
      (when (and (derived-mode-p 'my/mermaid-preview-mode)
                 my/mermaid-preview-file
                 (file-exists-p my/mermaid-preview-file))
        (set-window-margins window 0 0)
        (let ((columns (max 1 (1- (window-body-width window)))))
          (unless (equal columns my/mermaid-preview-columns)
            (setq my/mermaid-preview-columns columns)
            (let ((inhibit-read-only t))
              (if (display-graphic-p (window-frame window))
                  (progn
                    (erase-buffer)
                    (insert-image (create-image my/mermaid-preview-file nil nil
                                                :max-width (window-body-width window t))))
                (kitty-graphics-remove-images (point-min) (point-max))
                (goto-char (point-max))
                ;; Fit by width; tall diagrams remain vertically scrollable.
                (kitty-graphics-display-image my/mermaid-preview-file
                                             (point) (point) columns 100000)))))))))

(defun my/mermaid-preview-resize (frame)
  "Refit Mermaid previews when windows in FRAME change size."
  (dolist (window (window-list frame 'no-minibuf))
    (with-selected-window window
      (my/mermaid-fit-preview window))))
(add-hook 'window-size-change-functions #'my/mermaid-preview-resize)

(defun my/mermaid-preview ()
  "Render Mermaid at point locally and show it in a separate Emacs buffer.
The source is left untouched.  Press q in the preview to return."
  (interactive)
  (unless (file-executable-p mermaid-mmdc-location)
    (user-error "Install Mermaid CLI under ~/.emacs.d/mermaid-tools first"))
  (let* ((source (my/mermaid-source-at-point))
         (directory (make-temp-file "emacs-mermaid-" t))
         (input (expand-file-name "diagram.mmd" directory))
         (output (expand-file-name "diagram.png" directory))
         (config (expand-file-name "config.json" directory))
         (log (generate-new-buffer "*Mermaid render*"))
         (frame (selected-frame)))
    (with-temp-file input (insert source))
    (with-temp-file config
      (insert "{\"themeVariables\":{\"fontSize\":\"22px\"},\"flowchart\":{\"useMaxWidth\":false},\"sequence\":{\"useMaxWidth\":false}}"))
    (make-process
     :name "mermaid-render" :buffer log :noquery t
     :command (list mermaid-mmdc-location "-i" input "-o" output
                    "-t" "neutral" "-b" "white" "-s" "4"
                    "-w" "2400" "-c" config)
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (if (and (zerop (process-exit-status process)) (file-exists-p output))
             (let ((preview (generate-new-buffer "*Mermaid preview*")))
               (with-selected-frame frame
                 (pop-to-buffer preview)
                 (my/mermaid-preview-mode)
                 (setq my/mermaid-preview-file output)
                 (let ((inhibit-read-only t)) (insert "Mermaid preview — q to return\n\n"))
                 (my/mermaid-fit-preview)
                 (add-hook 'kill-buffer-hook
                           (lambda () (delete-directory directory t)) nil t))
               (kill-buffer log))
           (delete-directory directory t)
           (display-buffer log)
           (message "Mermaid rendering failed; see %s" (buffer-name log))))))
    (message "Rendering Mermaid…")))

(global-set-key (kbd "C-c m p") #'my/mermaid-preview)
(provide 'my-mermaid)
;;; my-mermaid.el ends here
