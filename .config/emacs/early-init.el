;; add my lisp directory and its subdirectories to the load path
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; stop garbage files
(eval-and-compile ; ensure values don't differ at compile time.
  (setq no-littering-etc-directory
        (expand-file-name "junk/config/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "junk/data/" user-emacs-directory))
  (require 'no-littering))

;; put eln-cache in var
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "eln-cache/" no-littering-var-directory))))

;; put elpa in var
(setq package-user-dir
      (expand-file-name "elpa/" no-littering-var-directory))

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

;; file handler optimization — skip regex matching on every load
(defvar my--old-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my--old-file-name-handler-alist)))

;; configure frame parameters
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; performance
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      auto-mode-case-fold nil
      read-process-output-max (* 2 1024 1024)
      load-prefer-newer t)
(setq redisplay-skip-fontification-on-input t)

;; bidirectional text
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; pgtk latency improvement
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; utf-8
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; fonts - https://github.com/thep0y/monaco-nerd-font/releases/tag/v0.2.2
(let ((mono-spaced-font "Monaco Nerd Font Mono")
      (proportionately-spaced-font "MonacoLigaturized Nerd Font"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 105)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

;; native comp warnings
(setq native-comp-async-report-warnings-errors 'silent)
