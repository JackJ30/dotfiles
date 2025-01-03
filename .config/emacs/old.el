;; potentially should update: https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold most-positive-fixnum)
(setq garbage-collection-messages t)

;; package setup
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(elpaca-wait)

;; basic interface

(setq make-backup-files nil
      create-lockfiles nil
      erc-join-buffer 'window
      confirm-kill-processes nil)

(setq inhibit-startup-message t
      backup-inhibited t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(setq scroll-up-aggressively nil
      scroll-down-aggressively nil
      scroll-conservatively 101
      display-line-numbers-type t)

(setq scroll-step 1)
(setq scroll-margin 8)

(column-number-mode +1)
(global-display-line-numbers-mode t)
(setq-default fill-column 80)

(electric-pair-mode +1)

(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                mu4e-main-mode-hook
                mu4e-headers-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(set-face-attribute 'default nil
                    :font "DejaVu Sans Mono"
                    :family "Monospace"
                    :height 97)
(set-face-attribute 'variable-pitch nil
                    :font "DejaVu Sans"
                    :height 97)
(set-face-attribute 'fixed-pitch nil
                    :font "DejaVu Sans Mono"
                    :family "Monospace"
                    :height 97)

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 25)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-org-config))

(use-package miasma-theme
  :vc (:fetcher github :repo daut/miasma-theme.el)
  :config
  ;(load-theme 'miasma t)
  )

;; misc changes
(use-package diminish)

(auto-revert-mode 1)

(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; interface packages
;; - vertico and completion
(use-package vertico
  :ensure (vertico :files (:defaults "extensions/*"))
  :diminish vertico-mode
  :bind (:map vertico-map
	      ("C-n" . vertico-next)
	      ("C-p" . vertico-previous))
  :init
  (vertico-mode 1)
  ;; (vertico-flat-mode 1)
  (setq vertico-count 15))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package marginalia
  :diminish marginalia-mode
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode))

(use-package consult
  :config
  (setq completion-in-region-function
	(lambda (&rest args)
	  (apply (if vertico-mode
		     #'consult-completion-in-region
		   #'completion--in-region)
		 args)))
  (consult-customize consult-buffer :preview-key "M-."))

(use-package orderless
  :config
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

;; - icons
(use-package nerd-icons)
(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config
  (add-hook 'prog-mode-hook #'(lambda () (emojify-mode -1))))

;; text editing packages
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-quoted
  :diminish highlight-quoted-mode
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; - flyspell ( still working on perfect config )
;; (use-package flyspell
;;   :ensure nil
;;   ;; :diminish flyspell-mode
;;   )

;; (setq flyspell-prog-text-faces '(font-lock-doc-face))

;; (use-package flyspell-correct
;;   :after flyspell)

;; (use-package consult-flyspell
;;   :ensure (consult-flyspell :host gitlab :repo "OlMon/consult-flyspell" :branch "master")
;;   :config
;;   ;; default settings
;;   (setq consult-flyspell-select-function (lambda () (flyspell-correct-at-point) (consult-flyspell))
;; 	consult-flyspell-set-point-after-word t
;; 	consult-flyspell-always-check-buffer nil))

;; (add-hook 'text-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; directory changes and packages

(recentf-mode 1)

(use-package no-littering
  :config
  (add-to-list 'recentf-exclude
	       (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
	       (recentf-expand-file-name no-littering-etc-directory))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package savehist
  :ensure nil
  :diminish savehist-mode
  :init
  (savehist-mode 1))

;; - dired

(setf dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)

;; -- undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-limit 1000)
  (add-hook 'authinfo-mode-hook #'(lambda () (setq-local undo-tree-auto-save-history nil)))
  (defvar --undo-history-directory (concat user-emacs-directory "undotreefiles/")
    "Directory to save undo history files.")
  (unless (file-exists-p --undo-history-directory)
    (make-directory --undo-history-directory t))
  ;; stop littering with *.~undo-tree~ files everywhere
  (setq undo-tree-history-directory-alist `(("." . ,--undo-history-directory))))
(global-set-key (kbd "C-/") #'undo-tree-undo)
(global-set-key (kbd "M-/") #'undo-tree-redo)

;; - embark
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark--minimal-indicator-overlay nil)
  (setq embark-indicators (delq 'embark-mixed-indicator embark-indicators))
  (add-to-list 'embark-indicators #'embark-minimal-indicator))

(use-package embark-consult
  :config
  (define-key embark-file-map (kbd "S") 'sudo-find-file))

;; development
;; - magit
(use-package transient)
(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; - projectile

;; - tree sitter
(use-package treesit
  :elpaca nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

(use-package tree-sitter-langs)

;; - glsl
(use-package glsl-mode
    :ensure t)

;; - snippet
(use-package yasnippet
  :config
  (yas-global-mode 1)
  :bind (("C-M-n" . yas-next-field )
	 ("C-M-p" . yas-prev-field )))

;; - LSP mode

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-headerline-breadcrumb-enable nil
	lsp-lens-enable nil)
  :config
  (lsp-enable-which-key-integration t)
  :hook (
	 (c-mode . lsp)
	 (c++-mode . lsp-deferred)
	 (python-mode . lsp-deferred)
	 (csharp-mode . lsp)
	 ((tsx-ts-mode
               typescript-ts-mode
               js-ts-mode) . lsp-deferred)
	 )
  :custom
  (lsp-completion-provider :none) ; corfu
  (lsp-idle-delay 0.5)
  )

(defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))
(add-hook 'lsp-completion-mode-hook #'corfu-lsp-setup)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))
(setq lsp-ui-doc-position 'bottom)

;; - - lsp booster
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

;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :config
  (setq flycheck-error-message-buffer " *Flycheck error messages*")
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode 1))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

;; - - hooks
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-cmake-mode-setup ()
  "Switch to cmake-mode when opening a CMakeLists.txt file."
  (when (string-match "CMakeLists\\.txt\\'" (buffer-name))
    (cmake-ts-mode)))

(add-hook 'find-file-hook 'my-cmake-mode-setup)

;; - - additional packages

(use-package consult-lsp
  :after lsp)

(use-package lsp-treemacs
  :after lsp)

;- completion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.3)
  (corfu-popupinfo-delay '(0.2 . 0.1))
  (corfu-preview-current 'insert)
  (corfu-preselect 'first)
  (corfu-on-exact-match nil)
  :bind (:map corfu-map
              ("C-g" . corfu-quit))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  ;:custom
  ; (kind-icon-blend-background t)
  ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )

;; - misc
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; keybinds

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(global-unset-key (kbd "C-c C-f"))
(global-set-key (kbd "C-c C-f") #'consult-line)

(use-package move-text)
(global-set-key (kbd "M-p") #'move-text-up)
(global-set-key (kbd "M-n") #'move-text-down)

(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(use-package multiple-cursors
  :bind (:map global-map
              ("C->" . 'mc/mark-next-like-this)
              ("C-<" . 'mc/mark-previous-like-this)
              ("C-c C->" . 'mc/mark-all-like-this)
              :map mc/keymap
              ("<return>" . nil)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(miasma-theme vc-use-package))
 '(package-vc-selected-packages
   '((miasma-theme :vc-backend Git :url "https://github.com/daut/miasma-theme.el")
     (vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
