;;; my-site-start.el --- set up personal elisp/site-start.d/
;;
;; Copyright (C) era eriksson <http://www.iki.fi/~era/> 2008-2009
;; "New-style" BSD license (no advertising clause)
;;
;;; Commentary
;;
;; The purpose of my-site-start is to simplify maintenance of user libraries.
;; Instead of indefinitely tweaking your .emacs, just add symlinks to the
;; libraries you want to load into Emacs.
;;
;; Of course, my-site-start itself needs to be configured in the old-fashioned
;; style;
;;
;;  (autoload 'my-site-start "my-site-start" nil t)
;;  (my-site-start "~/.emacs.d/site-start/")
;;
;; This will load all files matching `my-site-start-file-name-regex' in
;; .emacs.d/site-start/ including any symlinks and subdirectories.
;;
;; See the customization variables below for more flexible and/or more
;; restricted usage scenarios.
;;
;;;;;;;; TODO: home page
;;;;;;;; TODO: public repo
;;
;;; Code

;(require 'subr)		; Not necessary, and doesn't (provide 'subr)


;;;;;;;; FIXME: defcustom

(defvar my-site-start-inhibit-p nil "\
*Set to non-nil to inhibit the running of `my-site-start' when loading.")

(defvar my-site-start-file-name-regex
  "\\(\\`\\|/\\)[0-9][0-9][-A-Za-z0-9_+.#$%@]+\.elc?$"
  "*Regular expression to select which files to `load' from `my-site-start'.

The regular expression is applied against each file's base name (the bare
file name with no directory path).")

(defvar my-site-start-avoid-dir-regex
  (mapconcat
   #'regexp-quote
   '(
     "RCS"
     "CVS"
     ".svn"
     "_darcs"
     ;;;;;;;; TODO: git, hg, monotone, svk, arch, bzr, others ...?
     )
   "\\|")
  "*Regular expression of directory names to avoid in `my-site-start'
when recursing into a directory tree.

The regular expression is applied in a context where the match is
anchored to the beginning and end of the bare directory name, without
a full path.")

(defvar my-site-start-load-order-function #'my-site-start-sort-load-order
  "*Function accepting a list of strings specifying file names to be loaded,
and returning the list in sorted order.  Usef in `my-site-start' to decide
the order in which to load files.")


(defun my-site-start (dir &optional no-recursion) "\
Add DIR to `load-path' and load files matching `my-site-start-file-name-regex'.

The optional second argument NO-RECURSION says to not traverse any directories.

See also `my-site-start-interactive-file-function' for controlling deferred
loading of interactive features.

The final load order is primarily based on file name.  In its default
configuration, `my-site-start' will only load file names with a numeric prefix,
in the numeric prefix order, falling back to the sort order of the whole file
name, regardless of the directory name.  However, this can be reconfigured by
changing the values of the variables `my-site-start-file-name-regex' and 
`my-site-start-load-order-function'.

If the value of the variable `my-site-start-inhibit-p' is non-`nil',
`my-site-start' will only report which files would have been loaded."
  (mapc #'my-site-start-load (my-site-start-files dir no-recursion)) )

(defun my-site-start-load (file)
  "Load FILE, and add its path to `load-path' if it is missing.

If `my-site-start-inhibit-p' is non-nil, just print diagnostics indiciating
what would have been done."
  (let ((p (if (string-match "\\`\\(.*\\)?/[^/]*\\'" file)
	       (match-string 1 file)
	     ".")))
    (when (string-equal "" p) (setq p "."))
    (message (if my-site-start-inhibit-p
		 (if (member p load-path) "%s is already on load-path"
		   "Would add %s to load-path")
	       "Adding %s to load-path") p)
    (add-to-list 'load-path p) )
  (message (if my-site-start-inhibit-p "Would load %s" "Loading %s") file)
  (or my-site-start-inhibit-p (load-file file)) )

(defun my-site-start-files (dir no-recursion)
  "Return files in DIR which are eligible for loading, obeying NO-RECURSION
i.e. only scanning the current directory if non-nil, otherwise descending into
subdirectories.

See `my-site-start-file-name-regex' and `my-site-start-load-order-function'
for determining which files should be loaded, and in which order."
  (let ((files (directory-files dir 'full-path nil ; no regex to filter on
				'dont-sort))
	(avoid-re
	 (concat "\\(\\`\\|/\\)"
		 "\\("
		 "\\.\\.?"
		 "\\|"
		 my-site-start-avoid-dir-regex
		 "\\)\\'") )
	list file)
    (while files
      (setq file (car files)
	    files (cdr files))
      (cond
       ((file-directory-p file)
	(or no-recursion
	    (string-match avoid-re file)
	    (setq list (append list (my-site-start-files file nil))) ) )
       ((string-match my-site-start-file-name-regex file)
	(setq list (cons file list)) ) ) )
    (funcall my-site-start-load-order-function list) ))

(defsubst my-site-start-split-filename (filename)
  (if (string-match "\\(\\`\\|/\\)\\(\\([0-9][0-9]\\).*\\)\\.elc?\\'" filename)
      (cons (string-to-number (match-string 3 filename))
	    (match-string 2 filename) )
    (error "\"%s\" does not look like a valid .el/.elc file name" filename) ) )
(defun my-site-start-sort-load-order (list)
  "Return the file names in LIST sorted numerically by basename."
  (sort list
	(function
	 (lambda (aa bb)
	   (let ((a (my-site-start-split-filename aa))
		 (b (my-site-start-split-filename bb)))
	     (if (equal (car a) (car b))
		 (string-lessp (cdr a) (cdr b))
	       (< (car a) (car b)) ) ))) ))



(defvar my-startup-interactive-setup-hook nil "\
Hook run at the end of loading user's startup files, if running on a terminal.

This provides a facility for deferring loading of features which are only
useful in interactive use, but not e.g. when batch-compiling Elisp files.

Technically, this hook is run from `term-setup-hook' in turn.")

(add-hook 'term-setup-hook
	  #'(lambda nil (run-hooks 'my-startup-interactive-setup-hook)) )

;(add-hook 'my-startup-interactive-setup-hook #'(lambda nil (message "fnord")))


;; This isn't really meant to be `require'd, but what the hey

(provide 'my-site-start)

;;; my-site-start.el ends here