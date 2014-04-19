(defun pacmanfiles-ediff (button)
  (message (concat (button-label button) " clicked"))
)

(defun pacmanfiles()
  (interactive)
  (with-current-buffer (get-buffer-create"*pacnew*")
	(delete-region (point-min) (point-max))
	(let ((pac-files (split-string (shell-command-to-string "locate '*.pacnew'") "\n"))
		  (var))
	  (dolist (var pac-files)
		(insert-button var 'action 'pacmanfiles-ediff)
		(insert "\n")))))
