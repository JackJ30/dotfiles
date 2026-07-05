;; some binds
(global-set-key (kbd "C-c f") 'ff-find-other-file)
(global-set-key (kbd "C-c c") 'recompile)
(global-set-key (kbd "C-<return>") 'browse-url-xdg-open)

;; magit
(use-package magit
  :demand t
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; consult
(use-package consult
  :custom
  (consult-preview-key nil)
  :bind (("M-y"   . 'consult-yank-pop)  ;; Paste by selecting the kill-ring
		 ("M-s"   . 'consult-line)      ;; Search current buffer, like swiper
		 ("C-c i" . 'consult-imenu)     ;; Search the imenu
		 ))

;; proj
(use-package proj
  :demand t
  :config
  (global-set-key (kbd "C-x b") `proj-switch-to-buffer)
  (global-set-key (kbd "C-c b") `switch-to-buffer)
  (global-set-key (kbd "C-x k") `proj-kill-buffer)
  (global-set-key (kbd "C-c k") `kill-buffer)
  (setq proj-locations '(("~/development/" . 1) ("~/opt/" . 1) ("~/dotfiles/" . 0) ("~/dotfiles/.config/emacs/lisp/" . 1))
		proj-grep-function 'consult-ripgrep))

;; editorconfig mode
(editorconfig-mode 1)
(add-hook 'prog-mode 'editorconfig-apply)

;; helpful
(use-package helpful
  :ensure nil
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))
