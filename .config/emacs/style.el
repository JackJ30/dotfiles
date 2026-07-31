;; turn off blink cursor
(blink-cursor-mode -1)

;; theme
(load-theme 'wombat)
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#222323" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 115 :width normal))))
 '(highlight ((t (:underline nil))))
 '(fringe ((t (:background "#222323"))))
 '(vertical-border ((t (:foreground "#303030"))))
 '(success ((t (:inherit (quote font-lock-keyword-face) :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "#86b7dd"))))
 '(font-lock-string-face ((t (:foreground "#98dc5f"))))
 )

;; ansi color
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

;; icons

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))
