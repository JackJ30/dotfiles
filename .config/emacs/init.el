;; see early-init.el for basic configuration

;; better meta behaviour
(setq inhibit-startup-message t
      vc-follow-symlinks t
      use-short-answers t
      enable-recursive-minibuffers t
      y-or-n-p t)

;; set up package manager
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
(loadc "completion-sorting.el")
(loadc "files.el")
(loadc "misc.el")
(loadc "windows.el")
(loadc "ide.el")
;; (loadc "evil.el")
;; (loadc "lang.el")

;; todo: integrated terminal, org config with modules, full screen popups, shackle
;; todo styling: minions (modeline collapse), forked theme to enable: hl-line, spacious-padding, pulsar

