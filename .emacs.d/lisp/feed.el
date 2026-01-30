;;; feed.el --- RSS Reader -*- lexical-binding: t; -*-

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds '("https://news.ycombinator.com/rss")))

(provide 'feed)
