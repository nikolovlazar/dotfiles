;;; completion.el --- Completion and fuzzy finding -*- lexical-binding: t; -*-

;; Vertico - vertical completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Orderless - fuzzy matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Marginalia - show file info in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Consult - enhanced search commands
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
	 ("C-x C-r" . consult-recent-file)
	 ("C-c f" . project-find-file)))

;;; In-buffer completion (code completion popups)

;; Corfu - completion popup like VSCode
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)              ;; Auto-popup completions
  (corfu-auto-prefix 2)       ;; After 2 characters
  (corfu-auto-delay 0.2)      ;; With slight delay
  (corfu-cycle t)             ;; Cycle with TAB
  (corfu-popupinfo-delay 0)   ;; Show docs immediately
  :bind
  (("C-SPC" . completion-at-point)   ;; Trigger completion manually
   :map corfu-map
   ("C-y" . corfu-insert))           ;; Select with C-y
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  ;; Slower completion in minibuffer (avoid hijacking :wq RET)
  (add-hook 'minibuffer-setup-hook
            (lambda () (setq-local corfu-auto-delay 0.5)))
  ;; Disable Corfu in Dirvish to preserve standard keybindings (d, x, etc.)
  (add-hook 'dirvish-mode-hook
            (lambda () (corfu-mode -1)))
  :custom-face
  (corfu-border ((t (:background "#555555")))))

;; Cape - extra completion sources
(use-package cape
  :ensure t
  :init
  ;; Only add file completion globally; dabbrev conflicts with LSP
  (add-hook 'completion-at-point-functions #'cape-file))

;;; Dictionary completion with definitions (for org-mode)

(defvar my/dict-definition-cache (make-hash-table :test 'equal)
  "Cache for dictionary definitions.")

(defun my/dict-fetch-definition (word)
  "Fetch definition for WORD from Free Dictionary API."
  (let ((cached (gethash word my/dict-definition-cache)))
    (if cached
        cached
      (condition-case nil
          (let* ((url (format "https://api.dictionaryapi.dev/api/v2/entries/en/%s"
                              (url-hexify-string word)))
                 (buffer (url-retrieve-synchronously url t t 2)))
            (when buffer
              (with-current-buffer buffer
                (goto-char (point-min))
                (when (re-search-forward "\n\n" nil t)
                  (let* ((json (json-parse-buffer :object-type 'alist))
                         (entry (and (vectorp json) (aref json 0)))
                         (meanings (alist-get 'meanings entry))
                         (result ""))
                    (when (vectorp meanings)
                      (dotimes (i (min 3 (length meanings)))
                        (let* ((meaning (aref meanings i))
                               (pos (alist-get 'partOfSpeech meaning))
                               (defs (alist-get 'definitions meaning)))
                          (setq result (concat result (format "(%s)\n" pos)))
                          (when (vectorp defs)
                            (dotimes (j (min 2 (length defs)))
                              (let ((def (alist-get 'definition (aref defs j))))
                                (setq result (concat result (format "  • %s\n" def)))))))))
                    (puthash word result my/dict-definition-cache)
                    result)))))
        (error nil)))))

(defun my/dict-doc-buffer (candidate)
  "Return documentation buffer for dictionary CANDIDATE."
  (let ((def (my/dict-fetch-definition candidate)))
    (when (and def (not (string-empty-p def)))
      (with-current-buffer (get-buffer-create " *dict-doc*")
        (erase-buffer)
        (insert (propertize candidate 'face 'bold) "\n\n" def)
        (current-buffer)))))

(defun my/cape-dict-with-definitions ()
  "Dictionary completion with inline definitions."
  (when-let ((result (cape-dict)))
    (let ((start (car result))
          (end (cadr result))
          (table (caddr result))
          (props (cdddr result)))
      `(,start ,end ,table
        :company-doc-buffer ,#'my/dict-doc-buffer
        :annotation-function ,(lambda (_) " Dict")
        ,@props))))

;; Icons in completion popup
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'completion)
