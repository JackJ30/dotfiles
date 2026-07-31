;; opt out of custom and lockfiles
(setq custom-file (make-temp-file "emacs-custom-"))
(setq create-lockfiles nil)

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
(loadc "ide.el")
;; (loadc "evil.el")
;; (loadc "lang.el")

;; todo: ultra scroll, dumb-jump, scroll-on-jump, hl-line, minions for modeline, terminal, fork theme and make some improvements
;; skipped: diminish, evil, rainbow delimiters
