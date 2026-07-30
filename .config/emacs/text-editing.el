;; scrolling
(setq scroll-up-aggressively nil
      scroll-down-aggressively nil
      scroll-conservatively 101)
(setq scroll-step 1)
(setq scroll-margin 8)

;; columns
(setq-default fill-column 80)
(setq-default truncate-lines t)

;; mwim
(use-package mwim
  :bind (("C-a" . mwim-beginning)
		 ("C-e" . mwim-end-of-line)))

;; expand keybind
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; better comment keybind
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; delete selected region when you type new text
(use-package delsel
  :hook (after-init . delete-selection-mode))
