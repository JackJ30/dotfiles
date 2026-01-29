(defvar toggle-window-configuration nil
  "Stores the window configuration")

;; should rewrite to not do the save configuration thing

(defun toggle-window ()
  (interactive)
  (if (count-windows)
	  (if (one-window-p)
		  ;; reset configuration if single window
		  (if toggle-window-configuration
			  (progn
				(set-window-configuration toggle-window-configuration)
				(setq toggle-window-configuration nil))
			(message "No previous window layout to restore."))
		;; if multiple windows save configuration
		(setq toggle-window-configuration (current-window-configuration))
		;; delete current or other window
		(if (derived-mode-p 'compilation-mode)
			(delete-window)
		  (delete-other-windows)))))

(provide 'toggle-window)
