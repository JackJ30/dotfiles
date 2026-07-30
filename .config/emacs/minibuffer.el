(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package vertico-directory
  :ensure t
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
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

(use-package savehist
  :hook (after-init . savehist-mode))
