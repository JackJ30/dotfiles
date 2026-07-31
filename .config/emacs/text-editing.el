;; scrolling
(setq scroll-up-aggressively nil
      scroll-down-aggressively nil
      scroll-conservatively 101
      scroll-step 1
      scroll-margin 6)

;; columns
(setq-default fill-column 80)
(setq-default truncate-lines t)

;; saves last place in file
(use-package saveplace
  :config
  (save-place-mode))

;; mwim
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning)
		 ("C-e" . mwim-end-of-line)))

;; expand keybind
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; better comment keybind
(use-package evil-nerd-commenter
  :ensure t
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; delete selected region when you type new text
(use-package delsel
  :hook (after-init . delete-selection-mode))
