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
      enable-recursive-minibuffers t
      y-or-n-p t)

;; set up package managers
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; emacs-31 compat
(use-package compat
  :ensure t
  :init
  (require 'compat)
  (require 'compat-31))

;; load my config files
(defun loadc (file) (load (locate-user-emacs-file file)))
(loadc "style.el")
(loadc "text-editing.el")
(loadc "minibuffer.el")
(loadc "completion-at-point.el")
(loadc "files.el")
(loadc "misc.el")
(loadc "windows.el")
;; (loadc "ide.el")
;; (loadc "evil.el")
;; (loadc "lang.el")

;; todo: minions for modeline, hl-line, terminal, capfs
;; skipped: diminish, ansi color and rainbow
;; delimiters, evil, rainbow, 
