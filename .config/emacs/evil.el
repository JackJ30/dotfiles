(use-package evil
  :demand t
  :ensure nil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  :custom
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-redo)
  (evil-move-beyond-eol nil)
  :bind
  ( :map evil-insert-state-map
	("C-d" . evil-delete-char))
  ( :map evil-motion-state-map
	("C-e" . nil)))

(use-package evil-collection
  :after evil
  :ensure nil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-paste-indent
  :after evil
  :ensure nil
  :diminish
  :config (global-evil-paste-indent-mode t))

;; improved C-g dwim
(defun my-keyboard-quit ()
  "Smart C-g: exits evil mode, closes minibuffer, or behaves normally."
  (interactive)
  (if (and (bound-and-true-p evil-local-mode) (not (evil-normal-state-p)))
	  (evil-force-normal-state)
	(if (active-minibuffer-window)
		(if (minibufferp)
			(minibuffer-keyboard-quit)
		  (abort-recursive-edit))
	  (keyboard-quit))))
(global-set-key (kbd "C-g") #'my-keyboard-quit)
