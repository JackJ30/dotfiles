;; behaviour improvemnts

(defun keyboard-quit-dwim ()
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))
(define-key global-map (kbd "C-g") #'keyboard-quit-dwim)

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package stillness-mode
  :demand t
  :vc (:url "https://github.com/neeasade/stillness-mode.el" :rev :newest)
  :ensure t
  :hook (after-init . stillness-mode))

;; minad stack

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
		("RET" . vertico-directory-enter)
		("DEL" . vertico-directory-delete-char)
		("M-DEL" . vertico-directory-delete-word))
  :config
  (setq read-extended-command-predicate #'command-completion-default-include-p
		minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-align-offset 5))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))
