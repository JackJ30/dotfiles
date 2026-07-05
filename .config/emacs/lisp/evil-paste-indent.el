;;; evil-paste-indent.el --- Automatically indent text that is pasted with evil mode -*- lexical-binding: t -*-

;; Author: Jim Myhrberg <contact@jimeh.me>, Pascal Jaeger <pascal.jaeger@leimstift.de>
;; URL: https://github.com/Schievel1/evil-paste-indent
;; Keywords: convenience, paste, indent
;; Package-Requires: ((emacs "29.1") (evil "1.14.0"))
;; x-release-please-start-version
;; Version: 0.0.1
;; x-release-please-end

;; This file is not part of GNU Emacs.

;;; License:
;;
;; Copyright (c) 2023 Jim Myhrberg, Pascal Jaeger
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; Automatically indent pasted regions when evil-paste-indent-mode is enabled.

;;; Code:

(defgroup evil-paste-indent nil
  "Customization options for the evil-paste-indent package.

The evil-paste-indent package provides functionality to automatically
indent pasted text according to the current mode's indentation
rules. This group contains customization options for controlling
the behavior of `evil-paste-indent-mode' and `global-evil-paste-indent-mode'."
  :group 'editing)

(defcustom evil-paste-indent-threshold 5000
  "Max characters in pasted region to trigger auto indentation.

If the pasted region contains more characters than the value
specified by `evil-paste-indent-threshold', the automatic indentation
will not occur. This helps prevent performance issues when
working with large blocks of text."
  :type 'number)

(defcustom evil-paste-indent-global-derived-modes '(prog-mode tex-mode)
  "Derived major modes where `global-evil-paste-indent-mode' enables `evil-paste-indent-mode'.

When `global-evil-paste-indent-mode' is enabled, it activates
`evil-paste-indent-mode' in buffers with major modes derived from those
listed in this variable. This is useful when you want to enable
`evil-paste-indent-mode' for all modes that inherit from a specific
mode, such as `prog-mode' for programming modes or `text-mode'
for text editing modes."
  :type '(repeat symbol))

(defcustom evil-paste-indent-global-exact-modes '()
  "Major modes where `global-evil-paste-indent-mode' enables `evil-paste-indent-mode'.

When `global-evil-paste-indent-mode' is enabled, it activates
`evil-paste-indent-mode' in buffers with major modes listed in this
variable. Unlike `evil-paste-indent-global-derived-modes',
`evil-paste-indent-mode' will not be activated in modes derived from
those listed here. Use this variable to list specific modes where
you want `evil-paste-indent-mode' to be enabled without affecting their
derived modes."
  :type '(repeat symbol))

(defcustom evil-paste-indent-global-excluded-modes '(cmake-ts-mode
                                                     coffee-mode
                                                     conf-mode
                                                     haml-mode
                                                     makefile-automake-mode
                                                     makefile-bsdmake-mode
                                                     makefile-gmake-mode
                                                     makefile-imake-mode
                                                     makefile-makepp-mode
                                                     makefile-mode
                                                     python-mode
                                                     python-ts-mode
                                                     slim-mode
                                                     yaml-mode
                                                     yaml-ts-mode)
  "Major modes where `global-evil-paste-indent-mode' does not enable `evil-paste-indent-mode'.

`global-evil-paste-indent-mode' will not activate `evil-paste-indent-mode' in
buffers with major modes listed in this variable or their derived
modes. This list takes precedence over
`evil-paste-indent-global-derived-modes' and
`evil-paste-indent-global-exact-modes'. Use this variable to exclude
specific modes and their derived modes from having
`evil-paste-indent-mode' enabled."
  :type '(repeat symbol))

(defun evil-paste-indent--should-enable-p ()
  "Return non-nil if current mode should be indented."
  (and (not (minibufferp))
       (not (member major-mode evil-paste-indent-global-excluded-modes))
       (or (member major-mode evil-paste-indent-global-exact-modes)
           (apply #'derived-mode-p evil-paste-indent-global-derived-modes))))

;;;###autoload
(define-minor-mode evil-paste-indent-mode
  "Minor mode for automatically indenting pasted text.

When enabled, this mode indents the pasted region according to
the current mode's indentation rules, provided that the region
size is less than or equal to `evil-paste-indent-threshold' and no
prefix argument is given during pasting."
  :lighter " EPI"
  :group 'evil-paste-indent
  (unless global-evil-paste-indent-mode
    (user-error "evil-paste-indent-mode is deactivated when global-evil-paste-indent-mode is not active.")
    (evil-paste-indent-mode -1)))

;;;###autoload
(define-globalized-minor-mode global-evil-paste-indent-mode
  evil-paste-indent-mode
  (lambda ()
    (when (evil-paste-indent--should-enable-p)
      (evil-paste-indent-mode 1))) ;; turn-on function
  (if global-evil-paste-indent-mode
      (progn
        (advice-add 'evil-paste-after :around #'evil-paste-indent--advice)
        (advice-add 'evil-paste-before :around #'evil-paste-indent--advice))
    (progn
      (advice-remove 'evil-paste-after #'evil-paste-indent--advice)
      (advice-remove 'evil-paste-after #'evil-paste-indent--advice))))


(defun evil-paste-indent--advice (orig-fun &rest args)
  "Conditionally indent pasted text.

Indentation is triggered only if all of the following conditions
are met:

- `this-command' is `evil-paste-after' or `evil-paste-before'.
- `evil-paste-indent-mode' is enabled.
- Prefix argument was not provided.
- Region size that was pasted is less than or equal to
  `evil-paste-indent-threshold'."
  (with-undo-amalgamate
    (apply orig-fun args)
    (if (and evil-paste-indent-mode
         (not current-prefix-arg))
        (let ((beg (evil-get-marker ?\[))
              (end (evil-get-marker ?\]))
              (mark-even-if-inactive transient-mark-mode))
          (if (<= (- end beg) evil-paste-indent-threshold)
              (indent-region beg end))))))

(provide 'evil-paste-indent)
;;; evil-paste-indent.el ends here
