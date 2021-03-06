(defvar pacmanfiles-command
  "locate -e --regexp '\\.pac\\(new\\|save\\|orig\\)$'"

  "The shell command to find configuration files that have been
updated in the package and changed on the disk.  Should return
a newline separated list.")


(defvar pacmanfiles-buffer-name
  "*pacmanfiles*"

  "The buffer name to use when running `pacmanfiles'.")


(defun pacmanfiles-current-line-file (prop)
  "Return the filename on the current line marked by PROP.

PROP is either the symbol `pacmanfiles-file-a' or the symbol
`pacmanfiles-file-b'"
  (save-excursion
	(let (search-start search-end search-max single-property-change)
	  (cond
	   ((eq prop 'pacmanfiles-file-a)
		(setq search-start (line-beginning-position)
			  search-max   (line-end-position)
			  single-property-change 'next-single-property-change))
	   ((eq prop 'pacmanfiles-file-b)
		(setq search-start (line-end-position)
			  search-max   (line-beginning-position)
			  single-property-change 'previous-single-property-change))
	   (t (error "Don't know how to handle symbol %s" prop)))

	  (setq search-end (funcall single-property-change search-start prop
								(current-buffer) search-max))
	  (when (equal search-end search-max)
		(error "Could not find a filename on this line(%s) for %s"
			   (line-number-at-pos)
			   prop))
	  (buffer-substring-no-properties search-start search-end))))


(defmacro pacmanfiles-maybe-add-sudo (filepath predicate)
  "Concatenates \"/sudo::\" to FILEPATH if PREDICATE returns false

FILEPATH is a symbol referring to a variable.  PREDICATE can be
any function expecting a path to a file."
  `(unless (,predicate ,filepath)
	 (setq ,filepath (concat "/sudo::" ,filepath))
	 (unless (,predicate ,filepath)
	   (error "%s failed on %s" (quote ,predicate) ,filepath))))


(defun pacmanfiles-ediff-current-line (&optional button)
  "Run ediff with the files on the current line.

The optional argument BUTTON exists solely so that this function
can also be used for the 'action' property of `insert-button'.
The argument is not used in the function."
  (interactive)
  (let ((file-a (pacmanfiles-current-line-file 'pacmanfiles-file-a))
		(file-b (pacmanfiles-current-line-file 'pacmanfiles-file-b)))
	(when (file-remote-p default-directory)
	  ;; just a placeholder otherwise it should be in pacmanfiles
	  (error "Running pacmanfiles remotely is not yet supported"))
	(pacmanfiles-maybe-add-sudo file-a file-writable-p)
	(pacmanfiles-maybe-add-sudo file-b file-readable-p)
	(ediff-files file-a file-b)))


(defun pacmanfiles-revert-buffer (&optional ignore-auto noconfirm)
  "Clear the buffer, search for entries and reinsert.

Both IGNORE-AUTO and NOCONFIRM are requirements for a
`revert-buffer' function but are currently unused."
  (with-current-buffer (get-buffer-create pacmanfiles-buffer-name)
	(let ((inhibit-read-only t))
	  (delete-region (point-min) (point-max))
	  (let ((pac-files (split-string
						(shell-command-to-string pacmanfiles-command)
						"\n" :omit-nulls))
			(var))
		(dolist (file-b pac-files)
		  (let* ((mtime (lambda (path) (nth 4 (file-attributes path))))
				 (file-a (file-name-sans-extension file-b))
				 (file-a-mtime (funcall mtime file-a))
				 (file-b-mtime (funcall mtime file-b)))
			(add-text-properties 0 (length file-a)
								 '(pacmanfiles-file-a t rear-nonsticky t)
								 file-a)
			(put-text-property 0 (length file-b)
							   'pacmanfiles-file-b t
							   file-b)
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
	  (align-regexp (point-min) (point-max) "\\(\t\\)" 1 1 :repeat))))


(defun pacmanfiles-delete-package-file ()
  "Delete on the current line the file coming from the package."
  (interactive)
  (let ((file-b (pacmanfiles-current-line-file 'pacmanfiles-file-b)))
	(when (yes-or-no-p (format "Really delete %s? " file-b))
	  (pacmanfiles-maybe-add-sudo file-b file-writable-p)
	  (delete-file file-b)
	  (delete-region (line-beginning-position) (progn (forward-line)
													  (point))))))


(defun pacmanfiles ()
  "Create a new buffer in `pacmanfiles-mode' and fill it."
  (interactive)
  (switch-to-buffer pacmanfiles-buffer-name)
  (pacmanfiles-mode)
  (pacmanfiles-revert-buffer))


(define-derived-mode pacmanfiles-mode special-mode "pacmanfiles"
  "Major-mode for managing configuration files installed by the
package-manager 'pacman', when the with the package associated
file has been changed independently from the package by the
user."
  (setq revert-buffer-function 'pacmanfiles-revert-buffer)
  (define-key pacmanfiles-mode-map
	(kbd "RET") 'pacmanfiles-ediff-current-line)
  (define-key pacmanfiles-mode-map
	(kbd "d") 'pacmanfiles-delete-package-file))
