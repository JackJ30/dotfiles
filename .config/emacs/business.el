;; elfeed
(use-package elfeed
  :ensure t
  :config
  (setq-default elfeed-search-filter "@1week +unread")
  (add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread)))

(use-package elfeed-org
  :ensure t
  :init
  (setq rmh-elfeed-org-files (list (locate-user-emacs-file "elfeed.org")))
  (elfeed-org))
