;; todo - check out shackle
;;        maybe I want to support a two popper buffers (extra buffer + compilation dir)

;; todo - function to switch to a buffer as a popper buffer
;; todo - function to switch to a popup with completing read

(use-package popper
  :ensure t
  :bind (("C-c w"   . popper-toggle)
         ("C-c C-w"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-display-control 'nil)
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     help-mode
     helpful-mode
     compilation-mode
     Man-mode))
  :init
  (popper-mode 1)
  (popper-echo-mode -1))
