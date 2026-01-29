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

;; == custom lisp
(add-to-list 'load-path (concat (getenv "HOME") "/.config/emacs/lisp"))

;; == greener emacs
(use-package no-littering
  :demand t)

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
	  vc-follow-symlinks t
	  use-short-answers t)
(use-package saveplace
  :ensure nil
  :config
  (save-place-mode))

;; improved C-g dwim
(defun my-keyboard-quit ()
  "Smart C-g: exits evil mode, closes minibuffer, or behaves normally."
  (interactive)
  (if (and (bound-and-true-p evil-local-mode) (not (evil-normal-state-p)))
	  (evil-force-normal-state)
	(if (active-minibuffer-window)
		(if (minibufferp)
			(minibuffer-keyboard-quit)
		  (abort-recursive-edit))
	  (keyboard-quit))))
(global-set-key (kbd "C-g") #'my-keyboard-quit)

;; == ui

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

;; some binds
(global-set-key (kbd "C-c f") 'ff-find-other-file)
(global-set-key (kbd "C-c c") 'recompile)
(global-set-key (kbd "C-<return>") 'browse-url-xdg-open)

;; better commenting
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

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
  (evil-move-beyond-eol nil)
  :bind
  ( :map evil-insert-state-map
	("C-d" . evil-delete-char))
  ( :map evil-motion-state-map
	("C-e" . nil)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-paste-indent
  :vc (:url "https://github.com/Schievel1/evil-paste-indent"
			:rev :newest)
  :config (global-evil-paste-indent-mode t))

;; == style
;; theme
(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-one))

;; (use-package dracula-theme
;;   :demand t
;;   :config
;;   (load-theme 'dracula)
;;   (set-face-attribute 'show-paren-match nil :background "dark violet" :foreground "black"))
;; (use-package ef-themes
;;   :demand t
;;   :config
;;   (load-theme 'ef-bio))
;; (use-package catppuccin-theme
;;   :demand t
;;   :config
;;   (setq catppuccin-flavor 'mocha)
;;   (load-theme 'catppuccin t))

;; (load-theme `modus-vivendi)

;; (use-package spacious-padding
;;   :config
;;   (setq spacious-padding-widths (plist-put spacious-padding-widths :internal-border-width 10))
;;   (setq spacious-padding-widths (plist-put spacious-padding-widths :right-divider-width 10))
;;   (spacious-padding-mode))

;; (set-face-attribute 'mode-line nil
;;                     :box nil
;;                     :foreground "#9fefff"
;;                     :background "black")
;; (set-face-attribute 'mode-line-inactive nil
;;                     :box nil
;;                     :foreground "#5e8891"
;;                     :background "black")

;; (use-package doom-modeline
;;   :config
;;   (setq doom-modeline-modal nil
;; 		doom-modeline-height 0)
;;   (doom-modeline-mode))

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
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
	  completion-category-defaults nil
	  completion-category-overrides '((file (styles . (partial-completion))))))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

(use-package stillness-mode
  :ensure nil
  :init
  (stillness-mode))

;; == effecient navigation

;; consult
(use-package consult
  :custom
  (consult-preview-key nil)
  :bind (("M-l"   . 'consult-git-grep)  ;; Search inside a project
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

;; == magit
(use-package transient)
(use-package magit
  :after transient
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; == flycheck
(use-package flycheck)

;; == editorconfig
(editorconfig-mode 1)
(add-hook 'prog-mode 'editorconfig-apply)

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
  :commands lsp
  :bind
  (("C-c s" . lsp-signature-activate)))

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

;; == eglot
;; (use-package eglot
;;   :ensure nil
;;   :config
;;   (add-to-list 'eglot-ignored-server-capabilities ':inlayHintProvider)
;;   (add-to-list 'eglot-ignored-server-capabilities ':documentOnTypeFormattingProvider)
;;   (add-hook 'eglot-mode-hook 'flymake-mode))

;; == dumb jump
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

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
  :after yasnippet)

;; == development environment
(defun my/replace-master-vterm ()
  (interactive)
  (let* ((master-name "Master Terminal")
		 (banished-base "Banished Terminal ")
		 (master-buf (get-buffer master-name))
		 (visible-win (and master-buf (get-buffer-window master-buf t))))
	(when master-buf
	  (with-current-buffer master-buf
		(let ((n 1)
			  new-name)
		  (while (get-buffer (setq new-name (format "%s%d" banished-base n)))
			(setq n (1+ n)))
		  (rename-buffer new-name))))
	(let ((buf (if visible-win
                   (with-selected-window visible-win
                     (vterm))
                 (vterm-other-window))))
	  (with-current-buffer buf
		(rename-buffer master-name t)))))

(defun my/switch-to-master-vterm-other-window ()
  (interactive)
  (let* ((master-name "Master Terminal")
         (master-buf (get-buffer master-name))
         (visible-win (and master-buf (get-buffer-window master-buf t))))
    (cond
     (visible-win
      (select-window visible-win))
     (master-buf
      (switch-to-buffer-other-window master-buf))
     (t
      (let ((buf (vterm-other-window)))
        (with-current-buffer buf
          (rename-buffer master-name t)))))))

(defun my/switch-to-master-vterm ()
  (interactive)
  (let* ((master-name "Master Terminal")
         (master-buf (get-buffer master-name))
         (visible-win (and master-buf (get-buffer-window master-buf t))))
    (cond
     (visible-win
      (select-window visible-win))
     (master-buf
      (switch-to-buffer master-buf))
     (t
      (let ((buf (vterm)))
        (with-current-buffer buf
          (rename-buffer master-name t)))))))

(use-package vterm
  :bind ( ("C-c a" . my/switch-to-master-vterm-other-window)
		  ("C-c C-a" . my/replace-master-vterm)
		  :map vterm-mode-map
		  ("C-c c" . vterm-copy-mode)
		  ("C-l" . my-vterm-clear)
		  :map vterm-copy-mode-map
		  ("C-c c" . vterm-copy-mode))
  :config
  (defun my-vterm-clear ()
	(interactive)
	(vterm-send-key "l" nil nil t)
	(vterm-clear-scrollback))
  (defun my-vterm-copy-mode-evil-setup ()
	"Enable evil only in vterm-copy-mode."
	(if vterm-copy-mode
		;; on enter
		(progn
		  (evil-local-mode 1)
		  (evil-force-normal-state))
	  ;; on exit
	  (evil-local-mode -1)))
  (defun my-vterm-setup ()
	(when (bound-and-true-p evil-local-mode)
	  (evil-local-mode -1))
	(setq-local truncate-lines nil))
  ;; disable evil
  (with-eval-after-load 'evil
	(evil-set-initial-state 'vterm-mode 'emacs))
  (add-hook 'vterm-mode-hook #'my-vterm-setup)
  (add-hook 'vterm-copy-mode-hook #'my-vterm-copy-mode-evil-setup))

;; == languages

;; === c mode
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (setq c-default-style "k&r"
	    c-basic-offset 4
	    indent-tabs-mode t)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'case-label '+)
  (add-to-list 'c-offsets-alist '(arglist-close . c-lineup-close-paren)))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; == proj
(require 'proj)
(setq
 proj-locations '("~/development/" "~/opt/" "~/classes/psoft/" "~/classes/parallel-programming/" "~/classes/operating-systems/")
 proj-find-params '("-mindepth 1" "-maxdepth 1" "-path '*/.git'" "-prune -o" "-type d" "-print"))

(global-set-key (kbd "C-x b") `proj-switch-to-buffer)
(global-set-key (kbd "C-c b") `switch-to-buffer)

(global-set-key (kbd "C-x k") `proj-kill-buffer)
(global-set-key (kbd "C-c k") `kill-buffer)

;; == toggle-window
(require 'toggle-window)
(global-set-key (kbd "C-c w") 'toggle-window)
