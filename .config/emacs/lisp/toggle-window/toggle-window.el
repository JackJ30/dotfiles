(defvar toggle-window-configuration nil
  "Stores the window configuration")

(defun toggle-window ()
  (interactive)
  (if (count-windows)
	  (if (one-window-p)
		  ;; reset configuration if single window
		  (if toggle-window-configuration
			  (let* ((current-window (selected-window))
					 (current-buffer (window-buffer current-window)))
				(progn
				  (set-window-configuration toggle-window-configuration)
				  (set-window-buffer current-window current-buffer)
				  ;; make sure current window keeps state
				  (window-state-put current-window-state)
				  (setq toggle-window-configuration nil))
				)
			(message "No previous window layout to restore."))
		;; if multiple windows save configuration
		(setq toggle-window-configuration (current-window-configuration))
		;; delete current or other window
		(if (derived-mode-p 'compilation-mode)
			(delete-window)
		  (delete-other-windows)))))

(provide 'toggle-window)
