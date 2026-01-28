;;; treesitter.el --- Tree-sitter configuration -*- lexical-binding: t; -*-

(setq treesit-language-source-alist
      '((javascript "https://github.com/tree-sitter/tree-sitter-javascript")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (python "https://github.com/tree-sitter/tree-sitter-python")))

;; Auto-mode mappings for tree-sitter modes
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

(defun my/install-treesit-grammars ()
  "Install all tree-sitter language grammars."
  (interactive)
  (dolist (lang '(javascript typescript tsx python))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

(provide 'treesitter)
