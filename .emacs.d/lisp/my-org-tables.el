;;; my-org-tables.el --- Table-only Org preview -*- lexical-binding: t; -*-
(add-to-list 'load-path
             (expand-file-name "lisp/markdown-table-wrap" user-emacs-directory))
(require 'markdown-table-wrap-pretty)
(require 'org-element)
(setq markdown-table-wrap-pretty-default-on-major-modes '(org-mode))
(add-hook 'org-mode-hook #'markdown-table-wrap-pretty-mode)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n t") #'markdown-table-wrap-pretty-toggle))
(defun my/org-table-render-inline (cell)
  "Render Org emphasis, links and entities in CELL without changing its source."
  (cl-labels
      ((render (object)
         (if (stringp object)
             (substring-no-properties object)
           (let* ((type (org-element-type object))
                  (blank (or (org-element-property :post-blank object) 0))
                  (marker (cdr (assq type '((bold . "*") (italic . "/")
                                           (underline . "_") (strike-through . "+")
                                           (code . "~") (verbatim . "=")))))
                  (face (cadr (assoc marker org-emphasis-alist)))
                  (text
                   (cond
                    (marker
                     (if (memq type '(code verbatim))
                         (org-element-property :value object)
                       (mapconcat #'render (org-element-contents object) "")))
                    ((eq type 'link)
                     (if (org-element-contents object)
                         (mapconcat #'render (org-element-contents object) "")
                       (org-element-property :raw-link object)))
                    ((eq type 'entity)
                     (org-element-property :utf-8 object))
                    (t
                     ;; Preserve unsupported objects (including timestamps)
                     ;; exactly as written rather than treating them as Markdown.
                     (substring cell
                                (1- (org-element-property :begin object))
                                (- (org-element-property :end object) blank 1))))))
             (when face
               (add-face-text-property 0 (length text) face t text))
             (when (eq type 'link)
               (add-face-text-property 0 (length text) 'org-link t text)
               (add-text-properties
                0 (length text)
                (list 'mouse-face 'highlight
                      'help-echo (org-element-property :raw-link object)) text))
             (concat text (make-string blank ?\s))))))
    (mapconcat #'render
               (org-element-parse-secondary-string
                cell (cdr (assq 'table-cell org-element-object-restrictions)))
               "")))

(defun my/org-table-inline-dispatch (original cell)
  "Use native Org syntax in Org tables; delegate other file types."
  (if (and markdown-table-wrap-pretty-prettify (derived-mode-p 'org-mode))
      (my/org-table-render-inline cell)
    (funcall original cell)))

(advice-add 'markdown-table-wrap-pretty--render-inline-spans-in-cell
            :around #'my/org-table-inline-dispatch)

(provide 'my-org-tables)
