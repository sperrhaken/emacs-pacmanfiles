(defun pacmanfiles-current-line-file-a ()
  (save-excursion
	(let (point-beginning point-end)
	  (setq point-beginning (line-beginning-position))
	  (setq point-end (next-single-property-change point-beginning
												   'pacmanfiles-file-a))
	  (buffer-substring-no-properties (line-beginning-position) point-end))))


(defun pacmanfiles-current-line-file-b ()
  (save-excursion
	(let (point-beginning point-end)
	  (setq point-end (line-end-position))
	  (setq point-beginning (previous-single-property-change point-end
															 'pacmanfiles-file-b))
	  (buffer-substring-no-properties point-beginning (line-end-position)))))


(defun pacmanfiles-ediff (button)
  (let ((file-a (pacmanfiles-current-line-file-a))
		(file-b (pacmanfiles-current-line-file-b)))
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
		  (add-text-properties 0 (length file-a) '(pacmanfiles-file-a t rear-nonsticky t) file-a)
		  (put-text-property 0 (length file-b) 'pacmanfiles-file-b t file-b)
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
