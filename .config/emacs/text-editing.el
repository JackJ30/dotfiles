;; tabs instead of spaces
(setq-default tab-width 4)
(setq backward-delete-char-untabify-method "hungry")

;; scrolling
(setq scroll-up-aggressively nil
      scroll-down-aggressively nil
      scroll-conservatively 101)
(setq scroll-step 1)
(setq scroll-margin 8)

;; mwim
(use-package mwim
  :ensure nil
  :bind (("C-a" . mwim-beginning)
		 ("C-e" . mwim-end-of-line)))

;; expand keybind
(use-package expand-region
  :ensure nil
  :bind ("C-=" . er/expand-region))

;; delete marked text when typing start
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; better comment keybind
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))
