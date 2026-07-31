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

;; ui improvements

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
		("RET" . vertico-directory-enter)
		("DEL" . vertico-directory-delete-char)
		("M-DEL" . vertico-directory-delete-word))
  :config
  (setq read-extended-command-predicate #'command-completion-default-include-p
		minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package dash :ensure t)
(use-package stillness-mode
  :demand t
  :after dash
  ;; :vc (:url "https://github.com/neeasade/stillness-mode.el" :rev :newest)
  :ensure nil
  :load-path "lisp/stillness-mode.el/"
  :hook (after-init . stillness-mode))

;; sorting and history

;; https://kristofferbalintona.me/articles/complement-corfu-vertico-and-completion-preview-with-prescientel-sorting/

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

(use-package prescient
  :ensure t
  :custom
  (prescient-aggressive-file-save t)
  (prescient-sort-length-enable t) ; testing this out
  (prescient-sort-full-matches-first t)
  (prescient-history-length 200)
  (prescient-frequency-decay 0.997)
  (prescient-frequency-threshold 0.05)
  :config
  (prescient-persist-mode 1))

(use-package vertico-prescient
  :ensure t
  :demand t
  :after vertico prescient
  :custom
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil)
  (vertico-prescient-enable-filtering nil) ; We want orderless to do the filtering
  :config
  (vertico-prescient-mode 1))
