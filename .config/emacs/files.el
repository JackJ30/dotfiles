(use-package dired
  :commands (dired)
  :hook  ((dired-mode . dired-hide-details-mode)
	  (dired-mode . hl-line-mode)
	  (dired-mode . (lambda () (setq-local cursor-type nil))))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-listing-switches "-alh --group-directories-first"))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :hook
  (dired-subtree-after-insert . nerd-icons-dired--refresh)
  :config
  (setq dired-subtree-use-backgrounds nil))
