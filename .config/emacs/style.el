;; get rid of ui
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; fonts
(let ((mono-spaced-font "DejaVuSansMono")
      (proportionately-spaced-font "DejaVuSans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 100)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

;; theme

(load-theme 'wombat)


(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#222323" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 115 :width normal :family "Monaco"))))
 '(highlight ((t (:underline nil))))
 '(fringe ((t (:background "#222323"))))
 '(vertical-border ((t (:foreground "#303030"))))
 '(success ((t (:inherit (quote font-lock-keyword-face) :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "#86b7dd"))))
 '(font-lock-string-face ((t (:foreground "#98dc5f"))))
 )

 ;; (custom-set-faces
 ;;  '(highlight ((t (:underline nil)))))
 ;; '(dired-directory ((t (:inherit (quote font-lock-keyword-face)))))
 ;; '(dired-header ((t (:inherit (quote font-lock-comment-face)))))
 ;; '(font-lock-keyword-face ((t (:foreground "#86b7dd"))))
 ;; '(font-lock-string-face ((t (:foreground "#98dc5f"))))
 ;; '(fringe ((t (:background "#222323"))))
 ;; '(ido-first-match ((t (:inherit (quote font-lock-comment-face)))))
 ;; '(ido-only-match ((t (:inherit (quote font-lock-comment-face)))))
 ;; '(ido-subdir ((t (:inherit (quote font-lock-keyword-face)))))
 ;; '(linum ((t (:inherit (shadow default) :background "#191919" :foreground "#505050"))))
 ;; '(success ((t (:inherit (quote font-lock-keyword-face) :weight normal))))
 ;; '(vertical-border ((t (:foreground "#303030"))))
 ;; '(company-scrollbar-bg ((t (:background "#303030"))))
 ;; '(company-scrollbar-fg ((t (:background "#99968b"))))
 ;; '(company-tooltip ((t (:background "#303030"))))
 ;; '(company-tooltip-annotation ((t (:inherit (quote font-lock-comment-face)))))
 ;; '(company-tooltip-common ((t (:inherit (quote font-lock-comment-face)))))
 ;; '(company-tooltip-common-selection ((t (:inherit (quote font-lock-keyword-face) :weight normal))))
 ;; '(company-tooltip-mouse ((t (:inherit (quote font-lock-keyword-face) :weight normal))))
 ;; '(company-tooltip-selection ((t (:inherit (quote font-lock-keyword-face) :weight normal))))

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
