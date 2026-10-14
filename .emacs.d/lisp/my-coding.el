;;; my-coding.el --- Coding tools -*- lexical-binding: t; -*-

(require 'treesit)
(require 'eglot)
(require 'company)
(require 'editorconfig)

;; Install once with: npm --prefix ~/.emacs.d/language-servers install
;; typescript-language-server typescript@6 pyright
(let ((bin (expand-file-name "language-servers/node_modules/.bin" user-emacs-directory)))
  (add-to-list 'exec-path bin)
  (unless (member bin (split-string (getenv "PATH") path-separator))
    (setenv "PATH" (concat bin path-separator (getenv "PATH")))))

;; These pinned grammars work with Emacs's built-in tree-sitter modes.
(dolist (recipe '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src")
                  (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "v0.23.2" "src")
                  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
                  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
                  (json "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8" "src")
                  (go "https://github.com/tree-sitter/tree-sitter-go" "v0.25.0" "src")
                  (python "https://github.com/tree-sitter/tree-sitter-python" "v0.25.0" "src")))
  (setf (alist-get (car recipe) treesit-language-source-alist) (cdr recipe)))

(defun my/coding-install-grammars ()
  "Download and compile missing grammars for the configured coding modes."
  (interactive)
  (dolist (language '(javascript jsdoc typescript tsx json go python))
    (unless (treesit-language-available-p language)
      (treesit-install-language-grammar language))))

(setq treesit-font-lock-level 4
      js-indent-level 2
      js-ts-mode-indent-offset 2
      typescript-ts-indent-offset 2
      json-ts-mode-indent-offset 2
      company-idle-delay 0.2
      company-minimum-prefix-length 1
      company-selection-wrap-around t
      company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                          company-preview-if-just-one-frontend
                          company-echo-metadata-frontend)
      eglot-autoshutdown t)

(dolist (entry '(("\\.[cm]?ts\\'" . typescript-ts-mode)
                 ("\\.[jt]sx\\'" . tsx-ts-mode)
                 ("\\.[cm]?js\\'" . js-ts-mode)
                 ("\\.json\\'" . json-ts-mode)))
  (add-to-list 'auto-mode-alist entry))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))

(defun my/coding-buffer-setup ()
  "Enable core editor features in programming buffers."
  (setq-local display-line-numbers-type 'relative
              indent-tabs-mode nil
              tab-width 2
              tab-always-indent 'complete)
  (display-line-numbers-mode 1)
  (font-lock-mode 1)
  (electric-pair-local-mode 1)
  (company-mode 1))
(add-hook 'prog-mode-hook #'my/coding-buffer-setup)
(editorconfig-mode 1)

(defun my/coding-start-eglot ()
  "Start language intelligence for a supported local source file."
  (when (and buffer-file-name
             (not (file-remote-p default-directory))
             (executable-find
              (cond ((derived-mode-p 'go-ts-mode) "gopls")
                    ((derived-mode-p 'python-mode 'python-ts-mode) "pyright-langserver")
                    (t "typescript-language-server"))))
    (eglot-ensure)))
;; Pyright can watch files itself; avoid exhausting Emacs's file descriptors.
(defclass my/eglot-pyright (eglot-lsp-server) ())
(cl-defmethod eglot-client-capabilities ((server my/eglot-pyright))
  (let ((capabilities (cl-call-next-method)))
    (plist-put (plist-get (plist-get capabilities :workspace)
                          :didChangeWatchedFiles)
               :dynamicRegistration :json-false)
    capabilities))
(add-to-list 'eglot-server-programs
             '((python-mode python-ts-mode)
               . (my/eglot-pyright "pyright-langserver" "--stdio")))

(defun my/go-buffer-setup ()
  "Use Go's standard tab indentation."
  (setq-local indent-tabs-mode t tab-width 8))
(add-hook 'go-ts-mode-hook #'my/go-buffer-setup)

(dolist (hook '(js-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook
                go-ts-mode-hook python-mode-hook python-ts-mode-hook))
  (add-hook hook #'my/coding-start-eglot))

;; C-c e groups coding actions; Org and existing global shortcuts are untouched.
(define-key prog-mode-map (kbd "C-c e r") #'eglot-rename)
(define-key prog-mode-map (kbd "C-c e a") #'eglot-code-actions)
(define-key prog-mode-map (kbd "C-c e f") #'eglot-format-buffer)
(define-key prog-mode-map (kbd "C-c e d") #'flymake-show-buffer-diagnostics)
(define-key prog-mode-map (kbd "C-c e n") #'flymake-goto-next-error)
(define-key prog-mode-map (kbd "C-c e p") #'flymake-goto-prev-error)
(define-key prog-mode-map (kbd "C-c e h") #'eldoc-doc-buffer)
(define-key prog-mode-map (kbd "C-c e c") #'company-complete)

(provide 'my-coding)
;;; my-coding.el ends here
