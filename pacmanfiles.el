(defun pacmanfiles-current-line-file (prop)
  (save-excursion
	(let (search-start search-end single-property-change)
	  (cond
	   ((eq prop 'pacmanfiles-file-a)
		(setq search-start (line-beginning-position)
			  single-property-change 'next-single-property-change))
	   ((eq prop 'pacmanfiles-file-b)
		(setq search-start (line-end-position)
			  single-property-change 'previous-single-property-change))
	   (t (error "Don't know how to handle symbol %s" prop)))
	  
	  (setq search-end (funcall single-property-change search-start prop))
	  (unless search-end
		(error "Could not find a filename on this line(%s) for %s"
			   (line-number-at-pos)
			   prop))
	  (buffer-substring-no-properties search-start search-end))))


(defun pacmanfiles-current-line-file-a ()
  (pacmanfiles-current-line-file 'pacmanfiles-file-a))


(defun pacmanfiles-current-line-file-b ()
  (pacmanfiles-current-line-file 'pacmanfiles-file-b))


(defmacro pacmanfiles-maybe-add-sudo (filepath predicate)
  `(unless (,predicate ,filepath)
	 (setq ,filepath (concat "/sudo::" ,filepath))
	 (unless (,predicate ,filepath)
	   (error "%s failed on %s" (quote ,predicate) ,filepath))))


(defun pacmanfiles-ediff (button)
  (let ((file-a (pacmanfiles-current-line-file-a))
		(file-b (pacmanfiles-current-line-file-b)))
	(when (file-remote-p default-directory)
	  ;; just a placeholder otherwise it should be in pacmanfiles
	  (error "Running pacmanfiles remotely is not yet supported"))
	(pacmanfiles-maybe-add-sudo file-a file-writable-p)
	(pacmanfiles-maybe-add-sudo file-b file-readable-p)
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
