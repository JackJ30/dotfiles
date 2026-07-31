(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1))

(use-package completion-preview
  :diminish
  :demand t
  :bind
  ( :map completion-preview-active-mode-map
    ("M-n" . completion-preview-next-candidate)
    ("M-p" . completion-preview-prev-candidate)
    ("<tab>" . completion-preview-insert)
    ("M-<return>" . completion-preview-insert))
  :config
  (global-completion-preview-mode)
  :custom
  (completion-preview-minimum-symbol-length 2)
  (completion-preview-ignore-case t))

(use-package cape
  :ensure t
  :demand t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  :config
  (advice-add 'eglot-completion-at-point :around  #'cape-wrap-buster))

(setq completion-auto-help nil)
