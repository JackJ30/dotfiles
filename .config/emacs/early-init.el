;; add my lisp directory and its subdirectories to the load path
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; stop garbage files
(eval-and-compile ; ensure values don't differ at compile time.
  (setq no-littering-etc-directory
        (expand-file-name "junk/config/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "junk/data/" user-emacs-directory))
  (require 'no-littering))

;; put eln-cache in junk
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (no-littering-expand-var-file-name "eln-cache/"))))

;; put elpa in junk
(setq package-user-dir (no-littering-expand-var-file-name "elpa/"))

;; backups and autosaves in junk
(let ((backup-dir (no-littering-expand-var-file-name "backups/"))
      (auto-saves-dir (no-littering-expand-var-file-name "auto-saves/")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

;; put custom in junk
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

;; put lockfiles in nowhere
(setq create-lockfiles nil)

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

;; configure parameters for each frame created
(modify-all-frames-parameters
 '((menu-bar-lines . 0)
   (tool-bar-lines . 0)
   (vertical-scroll-bars)
   (horizontal-scroll-bars)))
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; set fonts for each frame created
;; using font - https://github.com/thep0y/monaco-nerd-font/releases/tag/v0.2.2
(defun my--set-font (frame)
  (let ((mono-spaced-font "Monaco Nerd Font Mono")
        (proportionately-spaced-font "MonacoLigaturized Nerd Font"))
    (set-face-attribute 'default nil :family mono-spaced-font :height 110)
    (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
    (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0)))
(add-hook 'after-make-frame-functions 'my--set-font)

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

;; silence native comp warnings (who cares)
(setq native-comp-async-report-warnings-errors 'silent)
