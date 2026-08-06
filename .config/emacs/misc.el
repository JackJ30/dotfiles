;; some binds
(global-set-key (kbd "C-c f") 'ff-find-other-file)
(global-set-key (kbd "C-c c") 'recompile)
(global-set-key (kbd "C-<return>") 'browse-url-xdg-open)

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :hook
  ((magit-mode . (lambda () (setq-local cursor-type nil))))
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; consult
(use-package consult
  :ensure t
  :custom
  (consult-preview-key 'any)
  :bind (("M-y"   . 'consult-yank-pop)  ;; Paste by selecting the kill-ring
	 ("M-s"   . 'consult-line)      ;; Search current buffer swiper
	 ("C-c i" . 'consult-imenu)))   ;; Search the imenu

;; helpful
(use-package helpful
  :ensure t
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; editorconfig mode
(editorconfig-mode 1)
(add-hook 'prog-mode 'editorconfig-apply)

;; proj
(use-package proj
  :demand t
  :ensure nil
  :load-path "lisp/proj/"
  :config
  (global-set-key (kbd "C-x b") `proj-switch-to-buffer)
  (global-set-key (kbd "C-c b") `switch-to-buffer)
  (global-set-key (kbd "C-x k") `proj-kill-buffer)
  (global-set-key (kbd "C-c k") `kill-buffer)
  (setq proj-locations '(("~/development/" . 1) ("~/opt/" . 1) ("~/dotfiles/" . 0) ("~/dotfiles/.config/emacs/" . 0) ("~/dotfiles/.config/emacs/lisp/" . 1))
		proj-grep-function 'consult-ripgrep))

;; (use-package compile-angel
;;   :demand t
;;   :config
;;   (setq compile-angel-verbose t)

;;   ;; The following directive prevents compile-angel from compiling your init
;;   ;; files. If you choose to remove this push to `compile-angel-excluded-path-suffixes'
;;   ;; and compile your pre/post-init files, ensure you understand the
;;   ;; implications and thoroughly test your code. For example, if you're using
;;   ;; the `use-package' macro, you'll need to explicitly add:
;;   ;; (eval-when-compile (require 'use-package))
;;   ;; at the top of your init file.
;;   (push "/init.el" compile-angel-excluded-path-suffixes)
;;   (push "/early-init.el" compile-angel-excluded-path-suffixes)
;;   (push ".config/emacs/[^/]*.el" compile-angel-excluded-path-regexps)

;;   ;; Uncomment the line below to compile automatically when an Elisp file is saved
;;   ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

;;   ;; A global mode that compiles .el files when they are loaded
;;   ;; using `load' or `require'.
;;   (compile-angel-on-load-mode 1))
