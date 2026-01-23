;; Custom "in-line" text object
(evil-define-text-object evil-inner-line (count &optional beg end type)
  "Select the current line excluding leading/trailing whitespace."
  (let* ((line-start (line-beginning-position))
		 (line-end (line-end-position))
		 (text (buffer-substring-no-properties line-start line-end))
		 (nonspace-start (string-match-p "\\S-" text))
		 (nonspace-end (or (save-match-data
							 (string-match-p "\\S-\\s-*\\'" text))
						   0)))
	(evil-range (+ line-start nonspace-start)
				(+ line-start nonspace-end)
				'exclusive)))
(define-key evil-inner-text-objects-map "l" 'evil-inner-line)

;; icons
(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; == typst modes
(use-package typst-ts-mode
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode"
			:rev :newest))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
			   '(typst-ts-mode . "typst"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "tinymist")
                    :activation-fn (lsp-activate-on "typst")
                    :server-id 'theme-check)))
