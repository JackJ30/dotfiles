(setq make-backup-files nil
      create-lockfiles nil
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
      display-line-numbers-type 'relative)

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

(elpaca-wait)

(use-package diminish)
(elpaca-wait)

(diminish 'abbrev-mode)
(auto-revert-mode 1)
(diminish 'auto-revert-mode)
(diminish 'eldoc-mode)
(diminish 'isearch-mode)
(diminish 'abbrev-mode)

(recentf-mode 1)

(use-package no-littering
  :config
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package gcmh
  :diminish gcmh-mode
  :init
  (gcmh-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 3))

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

(use-package savehist
  :ensure nil
  :diminish savehist-mode
  :init
  (savehist-mode 1))

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

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t)
  (doom-themes-org-config))

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

(use-package nerd-icons)
(use-package all-the-icons)

(use-package flyspell
  :ensure nil
  ;; :diminish flyspell-mode
  )

(use-package flyspell-correct
  :after flyspell)

(use-package consult-flyspell
  :ensure (consult-flyspell :host gitlab :repo "OlMon/consult-flyspell" :branch "master")
  :config
  ;; default settings
  (setq consult-flyspell-select-function (lambda () (flyspell-correct-at-point) (consult-flyspell))
        consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil))

(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config
  (add-hook 'prog-mode-hook #'(lambda () (emojify-mode -1))))

(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package writeroom-mode
  :diminish)

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (add-hook 'authinfo-mode-hook #'(lambda () (setq-local undo-tree-auto-save-history nil)))
  (defvar --undo-history-directory (concat user-emacs-directory "undotreefiles/")
    "Directory to save undo history files.")
  (unless (file-exists-p --undo-history-directory)
    (make-directory --undo-history-directory t))
  ;; stop littering with *.~undo-tree~ files everywhere
  (setq undo-tree-history-directory-alist `(("." . ,--undo-history-directory))))


;; Keybinds

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(global-set-key (kbd "C-/") #'undo-tree-undo)
(global-set-key (kbd "M-/") #'undo-tree-redo)
