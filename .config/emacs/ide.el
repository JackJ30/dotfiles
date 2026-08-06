(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  (xref-prompt-for-identifier nil)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
