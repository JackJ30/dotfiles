
;; https://kristofferbalintona.me/articles/complement-corfu-vertico-and-completion-preview-with-prescientel-sorting/

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

;; Integration with corfu
(use-package corfu-prescient
  :ensure t
  :demand t
  :after corfu prescient
  :custom
  (corfu-prescient-enable-sorting t)
  (corfu-prescient-override-sorting nil) ; Don't override `display-sort-function'
  (corfu-prescient-enable-filtering nil) ; We want orderless to do the filtering
  :config
  (corfu-prescient-mode 1))

;; Integration with vertico
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

;; Have `completion-preview-mode' use prescient's sorting algorithm
(with-eval-after-load 'prescient
  (setopt completion-preview-sort-function #'prescient-completion-sort))

(add-variable-watcher 'corfu-sort-function
                      (lambda (_symbol newval operation where)
                        "Match the value of `completion-preview-sort-function' to `corfu-sort-function'.
If `corfu-sort-function' is set buffer-locally, also set
`completion-preview-sort-function' buffer-locally.  Otherwise, change
the default value of `completion-preview-sort-function' accordingly.

This action only applies when the value of `corfu-sort-function' is
set (i.e., OPERATION is \\='set).  This excludes, e.g., let bindings."
                        (when (equal operation 'set)
                          (if where
                              (with-current-buffer where
                                (setq-local completion-preview-sort-function newval))
                            (setopt completion-preview-sort-function newval)))))
