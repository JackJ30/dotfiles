;; initialize package.el
(require 'package)
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
    	("gnu" . "https://elpa.gnu.org/packages/")
    	("melpa" . "https://melpa.org/packages/")
		("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(setq package-archive-priorities
	  '(("gnu" . 10)))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t)

;; == greener emacs

;; garbage collection improvement
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
(defun gc-idle-timer ()
  "Trigger garbage collection when Emacs is idle for 0.5 seconds."
  (run-with-idle-timer 1.2 t 'garbage-collect))
(gc-idle-timer)

;; customize in different file
(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(load custom-file :no-error-if-file-is-missing)

;; no lockfiles
(setq create-lockfiles nil) 

;; backups in one folder
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; == better meta behaviour
(setq inhibit-startup-message t
	  vc-follow-symlinks t)
(use-package saveplace
  :ensure nil
  :config
  (save-place-mode))

;; improved C-g dwim
(defun prot/keyboard-quit-dwim ()
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
(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;; == ui

;; disable bs
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; columns and line numbers
(column-number-mode +1)
(setq-default fill-column 80)
(global-display-line-numbers-mode t)
(setq display-line-numbers-width-start t
	  display-line-numbers-type t)
(setq-default truncate-lines t)

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ansi color in compilation
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

;; == text editing and navigation

;; tabs

(setq-default tab-width 4)
(setq backward-delete-char-untabify-method "hungry")

;; scrolling
(setq scroll-up-aggressively nil
      scroll-down-aggressively nil
      scroll-conservatively 101)
(setq scroll-step 1)
(setq scroll-margin 8)

;; electric pairs
;; (electric-pair-mode +1)

;; mwim
(use-package mwim
  :bind (("C-a" . mwim-beginning)
	 ("C-e" . mwim-end-of-line)))

;; better commenting
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; expand keybind
(use-package expand-region
  :bind("C-=" . er/expand-region))

;; multiple cursors
(use-package multiple-cursors
  :bind (:map global-map
	      ("C->" . 'mc/mark-next-like-this)
	      ("C-<" . 'mc/mark-previous-like-this)
	      ("C-c C->" . 'mc/mark-all-like-this)
	      :map mc/keymap
	      ("<return>" . nil)))

;; delete marked text when typing start
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; == dired
(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-listing-switches "-alh --group-directories-first"))

(use-package dired-subtree
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

;; == evil
(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  :custom
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-redo)
  :bind
  ( :map evil-insert-state-map
	("C-d" . evil-delete-char))
  ( :map evil-motion-state-map
	("C-e" . nil)))

;; C-g to exit mode
(defun evil-keyboard-quit ()
  "Keyboard quit and force normal state."
  (interactive)
  (and evil-mode (evil-force-normal-state))
  (keyboard-quit))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (define-key evil-normal-state-map   (kbd "C-g") #'evil-keyboard-quit) 
  (define-key evil-motion-state-map   (kbd "C-g") #'evil-keyboard-quit) 
  (define-key evil-insert-state-map   (kbd "C-g") #'evil-keyboard-quit) 
  (define-key evil-window-map         (kbd "C-g") #'evil-keyboard-quit) 
  (define-key evil-operator-state-map (kbd "C-g") #'evil-keyboard-quit))

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

(use-package evil-paste-indent
  :vc (:url "https://github.com/Schievel1/evil-paste-indent"
			:rev :newest)
  :config (global-evil-paste-indent-mode t))

;; == style
;; theme
;; (use-package dracula-theme
;;   :demand t
;;   :config
;;   (load-theme 'dracula)
;;   (set-face-attribute 'show-paren-match nil :background "dark violet" :foreground "black"))
(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-tokyo-night))

;; icons
(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-completion
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; rainbow mode
(use-package rainbow-mode
  :hook (after-change-major-mode . rainbow-mode))

;; == minibuffer completion
(use-package vertico
  :custom
  (vertico-count 15)
  :bind (:map vertico-map
		("C-n" . vertico-next)
		("C-p" . vertico-previous))
  :init
  (vertico-mode t))

(use-package vertico-directory
  :ensure nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
		("RET" . vertico-directory-enter)
		("DEL" . vertico-directory-delete-char)
		("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
	  completion-category-defaults nil
	  completion-category-overrides '((file (styles . (partial-completion))))))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

;; == effecient navigation

;; consult
(use-package consult
  :custom
  (consult-preview-key nil)
  :bind
  (("C-x b" . 'consult-buffer)    ;; Switch buffer, including recentf and bookmarks
   ("M-l"   . 'consult-git-grep)  ;; Search inside a project
   ("M-y"   . 'consult-yank-pop)  ;; Paste by selecting the kill-ring
   ("M-s"   . 'consult-line)      ;; Search current buffer, like swiper
   ("C-c i" . 'consult-imenu)     ;; Search the imenu
   ))

;; projectile
(use-package rg)
(use-package projectile
  :after rg
  :bind
  (("C-c g" . projectile-ripgrep)
   ("C-c v" . projectile-find-file)))

;; find other file
(global-set-key (kbd "C-c f") 'ff-find-other-file)

;; == magit
(use-package transient)
(use-package magit
  :after transient
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; == lsp
(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-enable-additional-text-edit nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-completion-provider :none)
  (lsp-idle-delay 0.1)
  (lsp-enable-indentation nil)
  :hook ((c++-mode . lsp-deferred)
		 (c-mode . lsp-deferred)
		 (typst-ts-mode . lsp-deferred)
		 (java-mode . lsp-deferred))
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind
  (("C-c r" . lsp-ui-peek-find-references)
   ("C-c d" . lsp-ui-peek-find-definitions)))

;; lsp booster
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; == text completion
(use-package corfu
  :bind
  (:map corfu-map
		("C-g" . corfu-quit))
  :init
  (add-hook 'corfu-mode-hook
			(lambda ()
			  ;; disable orderless
			  (setq-local completion-styles '(basic)
						  completion-category-overrides nil
						  completion-category-defaults nil)))
  (global-corfu-mode)
  (corfu-history-mode))

(use-package cape
  :after lsp-mode
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (defun my/lsp-capf-busted ()
    "Return an uncached LSP completion function."
    (cape-capf-buster #'lsp-completion-at-point))
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list (my/lsp-capf-busted))))))

(global-completion-preview-mode)
(global-set-key (kbd "M-n") 'completion-preview-next-candidate)
(global-set-key (kbd "M-p") 'completion-preview-prev-candidate)

;; == snippets
(use-package yasnippet
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

(use-package yasnippet
  :bind
  (:map yas-minor-mode-map
        ("C-'". yas-expand)
        ([(tab)] . nil)
        ("TAB" . nil))
  :config
  (yas-reload-all)
  (yas-global-mode t))

(use-package yasnippet-snippets
  :after yasnippet)

;; == treesitter
(setq treesit-language-source-alist
	  '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
		(c "https://github.com/tree-sitter/tree-sitter-c")
		(typst "https://github.com/uben0/tree-sitter-typst")))
(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
	(treesit-install-language-grammar (car lang))))
(setq treesit-load-name-override-list
   '((c++ "libtree-sitter-cpp")))

;; == languages

;; === c mode
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (setq c-default-style "k&r"
	    c-basic-offset 4
	    indent-tabs-mode t)
  (c-set-offset 'arglist-intro '+)
  (add-to-list 'c-offsets-alist '(arglist-close . c-lineup-close-paren)))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; == org mode
(use-package org
  :ensure nil
  :config
  (setq org-startup-folded t
		org-src-preserve-indentation t
		org-src-tab-acts-natively t
		org-edit-src-content-indentation t))

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

;; == random functionality
;; erc
(setq erc-join-buffer 'window)
