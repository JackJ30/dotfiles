;; (use-package corfu
;;   :ensure t
;;   :hook (after-init . global-corfu-mode)
;;   :bind (:map corfu-map ("<tab>" . corfu-complete))
;;   :config
;;   (setq tab-always-indent 'complete)
;;   (setq corfu-preview-current nil)
;;   (setq corfu-min-width 20)
;;   (setq corfu-popupinfo-delay '(1.25 . 0.5))
;;   (corfu-popupinfo-mode 1)
;;   ;; Sort by input history (no need to modify `corfu-sort-function').
;;   (with-eval-after-load 'savehist
;;     (corfu-history-mode 1)
;;     (add-to-list 'savehist-additional-variables 'corfu-history)))

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

(setq completion-auto-help nil)
