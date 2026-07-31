;; == eglot
(use-package eglot
  :ensure nil
  :bind
  (("C-c l a" . eglot-code-actions))
  (("C-c l r" . eglot-rename))
  :hook
  (eglot-managed-mode . (lambda () (setq-local eldoc-documentation-function 'eldoc-documentation-enthusiast)))
  (eglot-mode-hook . flymake-mode)
  :config
  (add-to-list 'eglot-ignored-server-capabilities ':inlayHintProvider)
  (add-to-list 'eglot-ignored-server-capabilities ':documentOnTypeFormattingProvider)
  (add-to-list 'eglot-ignored-server-capabilities ':documentHighlightProvider)
  (add-to-list 'eglot-server-programs
			   '((c-ts-mode c++-ts-mode c-mode c++-mode)
				 . ("clangd"
					"-j=8"
					"--log=error"
					"--malloc-trim"
					"--background-index"
					"--completion-style=detailed"
					"--pch-storage=memory"
					"--header-insertion=never"
					"--header-insertion-decorators=0")))
  :custom
  (eglot-send-changes-idle-time 0.1))

(use-package eldoc
  :ensure nil
  :config
  (setq-default eldoc-documentation-strategy 'eldoc-documentation-enthusiast)
  (setq-default eldoc-minor-mode-string ""))

(use-package sideline-flymake
  :ensure nil)
(use-package sideline
  :ensure nil
  :diminish
  :hook
  ((flymake-mode . sideline-mode))
  :init
  (setq sideline-backends-right '(sideline-flymake)
		sideline-backends-right-skip-current-line nil
		sideline-order-right 'up
		sideline-priority 100))

;; == dumb jump
(use-package dumb-jump
  :ensure nil
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;; == text completion
(use-package corfu
  :ensure nil
  :demand t
  :bind
  (:map corfu-map
		("C-g" . corfu-quit))
  :config
  (add-hook 'corfu-mode-hook
			(lambda ()
			  ;; disable orderless
			  (setq-local completion-styles '(basic)
						  completion-category-overrides nil
						  completion-category-defaults nil)))
  (global-corfu-mode))

(use-package corfu-history
  :ensure nil
  :demand t
  :config
  (corfu-history-mode))

(use-package cape
  :ensure nil
  :demand t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  :config
  (advice-add 'eglot-completion-at-point :around  #'cape-wrap-buster))
  ;; (defun my/lsp-capf-busted ()
  ;;   "Return an uncached LSP completion function."
  ;;   (cape-capf-buster #'lsp-completion-at-point))
  ;; (add-hook 'lsp-completion-mode-hook
  ;;           (lambda ()
  ;;             (setq-local completion-at-point-functions
  ;;                         (list (my/lsp-capf-busted))))))

(use-package completion-preview
  :ensure nil
  :demand t
  :diminish
  :config
  (global-completion-preview-mode)
  (global-set-key (kbd "M-n") 'completion-preview-next-candidate)
  (global-set-key (kbd "M-p") 'completion-preview-prev-candidate))

;; == snippets
(use-package yasnippet
  :ensure nil
  :diminish yas-minor-mode yas-global-mode
  :config
  (yas-reload-all)
  :hook
  (prog-mode . yas-minor-mode)
  :bind
  (:map yas-keymap
		("M-n" . yas-next-field)
		("M-p" . yas-prev-field)
		([(tab)] . nil)
		("TAB" . nil)
		("M-I" . nil))
  (:map yas-minor-mode-map
		("C-'". yas-expand)
		([(tab)] . nil)
		("TAB" . nil)
		("M-I" . nil)))

;; (use-package yasnippet
;;   :bind
;;   (:map yas-minor-mode-map
;;         ("C-'". yas-expand)
;;         ([(tab)] . nil)
;;         ("TAB" . nil))
;;   :config
;;   (yas-reload-all)
;;   (yas-global-mode t))

(use-package yasnippet-snippets
  :ensure nil
  :after yasnippet)
