(require 'org)
(load-file ".config/emacs/sonokai_theme.el")
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
