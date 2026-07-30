;; add my lisp directory and its subdirectories to the load path
(defvar my-lisp-dir (concat (getenv "XDG_CONFIG_HOME") "/emacs/lisp"))
(add-to-list 'load-path my-lisp-dir)
(let ((default-directory my-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; stop garbage files
(require 'no-littering)

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

;; opt out of custom and lockfiles
(setq custom-file (make-temp-file "emacs-custom-"))
(setq create-lockfiles nil)

;; backups in one directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; better meta behaviour
(setq inhibit-startup-message t
      vc-follow-symlinks t
      use-short-answers t
	  enable-recursive-minibuffers t)

;; set up package managers
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; don't complain about bytecomp errors in a window
(add-to-list 'display-buffer-alist
			 '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
			   (display-buffer-no-window)
			   (allow-no-window . t)))

;; saves last place in file
(use-package saveplace
  :config
  (save-place-mode))

;; better C-g dwim
(defun keyboard-quit-dwim ()
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
(define-key global-map (kbd "C-g") #'keyboard-quit-dwim)

;; load my config files
(defun loadc (file) (load (locate-user-emacs-file file)))
(loadc "style.el")
(loadc "text-editing.el")
(loadc "minibuffer.el")
(loadc "completion-at-point.el")
(loadc "files.el")
(loadc "misc.el")
;; (loadc "ide.el")
;; (loadc "evil.el")
;; (loadc "lang.el")

(find-file user-init-file)

;; skipped: diminish, improved C-g, evil nerd commenter, ansi color and rainbow
;; delimiters, evil, rainbow, 
