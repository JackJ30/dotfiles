;; disable bs
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; columns and truncation
(column-number-mode +1)
(setq-default fill-column 80)
(setq-default truncate-lines t)

;; line numbers in prog mode
(defun my/prog-mode-hook ()
  (display-line-numbers-mode t)
  (setq display-line-numbers-width-start t
                display-line-numbers-type t))
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

;; vertico minibuffer completion

(use-package vertico
  :demand t
  :ensure nil
  :custom
  (vertico-count 15)
  :bind (:map vertico-map
		("C-n" . vertico-next)
		("C-p" . vertico-previous))
  :config
  (vertico-mode t))

(use-package vertico-directory
  :ensure nil
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
  :ensure nil
  :demand t
  :config
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package orderless
  :ensure nil
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))
