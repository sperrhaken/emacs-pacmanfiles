(defun pacmanfiles-ediff (button)
  (let* ((file-b (button-label button))
		 (file-a (concat "/sudo::" (replace-regexp-in-string "\\.pac\\(new\\|save\\|orig\\)$" "" file-b))))
	(ediff-files file-a file-b)))


(defun pacmanfiles()
  (interactive)
  (with-current-buffer (get-buffer-create"*pacnew*")
	(delete-region (point-min) (point-max))
	(let ((pac-files (split-string (shell-command-to-string "locate -e --regexp '\\.pac\\(new\\|save\\|orig\\)$'") "\n"))
		  (var))
	  (dolist (file-b pac-files)
		(let* ((mtime (lambda (path) (nth 4 (file-attributes path))))
			   (file-a (file-name-sans-extension file-b))
			   (file-a-mtime (funcall mtime file-a))
			   (file-b-mtime (funcall mtime file-b)))
		  (insert file-a
				  "\t"
				  (format-time-string "%d. %b %Y" file-a-mtime)
				  "\t")
		  (insert-button "<--ediff-->" 'action 'pacmanfiles-ediff)
		  (insert "\t"
				  (format-time-string "%d. %b %Y" file-b-mtime)
				  "\t"
				  file-b
				  "\n"))))
	(align-regexp (point-min) (point-max) "\\(\t\\)" 1 1 :repeat)))
