;; mwim
(use-package mwim
  :bind (("C-a" . mwim-beginning)
		 ("C-e" . mwim-end-of-line)))

;; expand keybind
(use-package expand-region
  :bind("C-=" . er/expand-region))

;; multiple cursors
(use-package multiple-cursors
  :bind (:map global-map
			  ("C->" . 'mc/mark-next-like-this)
			  ("C-<" . 'mc/mark-previous-like-this)
			  ("C-c C->" . 'mc/mark-all-like-this)
			  :map mc/keymap
			  ("<return>" . nil)))

;; delete marked text when typing start
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))
