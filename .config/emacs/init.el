(require 'package)

;; improve garbage collection
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

;; add my lisp directory and its subdirectories to the load path
(defvar my-lisp-dir (concat (getenv "XDG_CONFIG_HOME") "/emacs/lisp"))
(add-to-list 'load-path my-lisp-dir)
(let ((default-directory my-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; greener emacs
(use-package no-littering
  :ensure nil
  :demand t)

;; no custom and lockfiles
(setq custom-file null-device)
(setq create-lockfiles nil)

;; backups in one folder
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; better meta behaviour
(setq inhibit-startup-message t
          vc-follow-symlinks t
          use-short-answers t
		  enable-recursive-minibuffers t)

;; saveplace mode
(use-package saveplace
  :ensure nil
  :config
  (save-place-mode))

;; savehist mode
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

;; load my config files
(defun loadc (file) (load (expand-file-name file user-emacs-directory)))
(loadc "ui.el")
(loadc "text-editing.el")
(loadc "misc.el")
(loadc "style.el")
(loadc "file-management.el")  (context-menu-mode t)
(loadc "ide.el")
(loadc "evil.el")
(loadc "lang.el")

;; skipped: diminish, improved C-g, evil nerd commenter, ansi color and rainbow
;; delimiters, evil, rainbow, 
