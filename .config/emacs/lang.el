;; c mode
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (setq c-default-style "k&r"
	    c-basic-offset 4
	    indent-tabs-mode t)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'case-label '+)
  (add-to-list 'c-offsets-alist '(arglist-close . c-lineup-close-paren)))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; odin mode
(use-package odin-mode
  :ensure nil)

;; jai mode
(use-package jai-mode
  :ensure nil)

;; markdown
(add-hook 'markdown-mode-hook #'auto-fill-mode)
