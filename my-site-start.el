;;; my-site-start.el --- set up personal elisp/site-start.d/
;; era 2008-12-19
;;

(require 'subr)

(defvar my-startup-terminal-setup-hook nil "\
Hook run at the end of loading user's startup files, if running on a terminal.

This provides a facility for deferring loading of features which are only
useful in interactive use, but not e.g. when batch-compiling Elisp files.

Technically, this hook is run from `term-setup-hook' in turn.")

(add-hook 'term-setup-hook
	  #'(lambda nil (run-hooks 'my-startup-terminal-setup-hook)) )

;(add-hook 'my-startup-terminal-setup-hook #'(lambda nil (message "fnord")))


;;;;;;;; FIXME: defcustom

(defvar my-site-start-paths nil "\
*List of paths to add to `load-path' in `my-site-start'.

Normally, this should be a list of strings; however, as a special case,
entries may be a cons cell, in which the car is the path name as a string
and the cdr is nil, to signify that no autoloading should be attempted
in the directory named by the string.  `my-site-start-autoload-file-p' is
used to determine which files to skip in this case.")

;;;;;;;; TODO: should there be one my-site-start-system-file-p too?

(defvar my-site-start-inhibit-p nil "\
*Set to non-nil to inhibit the running of `my-site-start' when loading.")

(defvar my-site-start-filename-mask-regex "\\`[0-9][0-9]*\.elc?$"
  "*Regular expression to select which files to `load' from `my-site-start'.

The regular expression is applied against each file's base name (the bare
file name with no directory path).")

(defun my-site-start nil "\
Library of functions for implementing a personal `site-start.d' directory tree.

You use this library by `load'ing it; when loaded, it will examine the value
of `my-site-start-paths', and load any file names matching the wildcard in
`my-site-start-filename-mask-regex' in every one of the directories in the
path.

The load order is based on file name; only file names with a numeric prefix
will be loaded, in the numeric prefix order, falling back to the sort order
of the whole file name, regardless of the directory name.

The function `my-site-start' is run each time when you `load' the library,
but if the value of the variable `my-site-start-inhibit-p' is non-`nil',
it will only report which files would have been loaded."
  (mapc #'my-site-start-load-dir (my-site-start-paths)) )

(defun my-site-start-load-dir (dir)
  "Load all files from DIR matching `my-site-start-filename-mask-regex'
unless `my-site-start-inhibit-p' is set, in which case only report which
files would have been loaded.

Also add DIR to `load-path' if it is not already present."
  (let ((files (directory-files dir nil my-site-start-filename-mask-regex))
	file noautoload)
    (when (consp dir)
      (setq dir (car dir)
	    noautoload t) )
    (add-to-list 'load-path dir)
    (while files
      (setq file (concat dir "/" (car files))
	    files (cdr files))
      (when (not (and noautoload (my-site-start-autoload-file-p file)))
	(if (my-site-start-inhibit-p)
	    (message
	     "Loading of '%s' inhibited by my-site-start-inhibit-p" file)
	  (load-file file) )) ) ))


(my-site-start)


;; This isn't really meant to be `require'd, but what the hey

(provide 'my-site-start)

;;; my-site-start.el ends here