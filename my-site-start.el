;;; my-site-start.el --- set up personal .emacs.d/site-start.d/
;;
;; Copyright (C) era eriksson <http://www.iki.fi/~era/> 2008-2015
;; License: GPL v2
;; Version: see `my-site-start-version' below
;;
;;; Commentary:
;;
;; The purpose of my-site-start is to simplify maintenance of user libraries.
;; Instead of indefinitely tweaking your .emacs, just create a site-start.d
;; directory and add symlinks to the libraries you want to load into Emacs.
;;
;; See README.md for a more detailed introduction, and the customization
;; variables below for the rest.
;;
;;
;; Github repository / download: <http://github.com/tripleee/my-site-start/>
;;
;;; History:
;;
;; 2015-02-25 Version 0.003 -- moved to Github; update documentation and links
;; 2009-02-09 Version 0.002 -- various tweaks and updated documentation
;; 2009-02-05 Version 0.001 -- first public beta.
;;
;; See changelog and version control history for details.
;;
;;; Code:

;(require 'subr)		; Not necessary, and doesn't (provide 'subr)


(defconst my-site-start-version "0.003"
  "Version numer for `my-site-start' library.")


;;;;;;;; FIXME: defcustom


(defvar my-site-start-inhibit-p nil "\
*Set to non-nil to inhibit the running of `my-site-start' when loading.")

(defvar my-site-start-file-name-regex
  "\\(\\`\\|/\\)[0-9][0-9][-A-Za-z0-9_+.#$%@]+\.elc?$"
  "*Regular expression to select which files to `load' from `my-site-start'.

If you change this setting, you might also need to change other settings.
The assumption that all selected files will have one or more numbers at the
beginning of the file name is present in several other parts of `my-site-start'
and so you will need to change at least `my-site-start-load-order-function'
if this is not true.")

(defvar my-site-start-avoid-dir-regex
  (mapconcat
   #'regexp-quote
   '(
     "RCS"
     "CVS"
     ".git"
     ".svn"
     "_darcs"
     ;;;;;;;; TODO: hg, monotone, svk, arch, bzr, others ...?
     )
   "\\|")
  "*Regular expression of directory names to avoid in `my-site-start'
when recursing into a directory tree.

The regular expression is applied in a context where the match is
anchored to the beginning and end of the bare directory name, without
a full path.")

(defvar my-site-start-load-order-function #'my-site-start-sort-load-order
  "*Function accepting a list of strings specifying file names to be loaded,
and returning the list in sorted order.  Used in `my-site-start' to decide
the order in which to load files.")

(defvar my-site-start-defer-file-p-function #'my-site-start-defer-file-p
  "*Function to determine whether loading of a file name should be deferred.

See `my-site-start-do-deferred-loads'.")

(defvar my-site-start--deferred-load-files nil
  "List of files to load from `my-site-start-do-deferred-loads'.
\(Internal use only.\)")


(defun my-site-start (dir &optional no-recursion) "\
Add DIR to `load-path' and load files matching `my-site-start-file-name-regex'.

The optional second argument NO-RECURSION says to not traverse any directories.
Those other directories will also be prepended to `load-path'.

Files will be sorted according to the function pointed to by the variable
`my-site-start-load-order-function'.

See also `my-site-start-defer-file-p-function' for controlling deferred
loading of files.  The documentation for `my-site-start-do-deferred-loads'
contains further information about this feature.  If a file is determined to
belong in the deferred set, it will be loaded only later, from within the
`my-site-start-interactive-setup-hook' hook.

If the value of the variable `my-site-start-inhibit-p' is non-nil,
`my-site-start' will only report which files would have been loaded.
Changes to the `load-path' will also not be made, only reported."
  (mapc #'my-site-start-load
	(my-site-start-split-deferred
	 (funcall my-site-start-load-order-function
		  (my-site-start-files (expand-file-name dir) no-recursion) )
	 'my-site-start--deferred-load-files) ) )

(defun my-site-start-split-deferred (list variable)
  "Move deferred file names from LIST to VARIABLE, and return the rest.

Whether a file is to be deferred or not is determined by the function
pointed to by the variable `my-site-start-defer-file-p-function'."
  (let (l f d v)
    (while list
      (setq f (car list)
	    list (cdr list) )
      (setq v (if (funcall my-site-start-defer-file-p-function f) 'd 'l))
      (set v (cons f (symbol-value v))) )
    (set variable (append (symbol-value variable) (reverse d)))
    (reverse l) ))

(defun my-site-start-load (file)
  "Load FILE, unless `my-site-start' loading is inhibited.

If `my-site-start-inhibit-p' is non-nil, just print diagnostics indicating
what would have been done."
  (message (if my-site-start-inhibit-p "Would load %s" "Loading %s") file)
  (or my-site-start-inhibit-p (load-file file)) )

(defun my-site-start-files (dir no-recursion)
  "Return files in DIR which are eligible for loading, obeying NO-RECURSION
i.e. only scanning the current directory if non-nil, otherwise descending
into subdirectories.

DIR is also added to the front of `load-path' unless it is already on the
path \(or `my-site-start-inhibit-p' is non-nil, in which case only log
whether the path would have been added\).  If recursing, all traversed
directories will also be added to the path, under the same conditions.

DIR should be an absolute path name.

See `my-site-start-file-name-regex' for determining which files should be
loaded.

If both an `.el' and and `.elc' version of a file exists, only the newer
of the two is returned."

  (message (if my-site-start-inhibit-p
	       (if (member dir load-path) "%s is already on load-path"
		 "Would add %s to load-path")
	     "Adding %s to load-path") dir)
  (add-to-list 'load-path dir)

  (let ((files (directory-files dir 'full-path nil ; no regex to filter on
				'dont-sort))
	(avoid-re
	 (concat "\\(\\`\\|/\\)"
		 "\\("
		 "\\.\\.?"
		 "\\|"
		 my-site-start-avoid-dir-regex
		 "\\)\\'") )
	list file elc)
    (save-match-data
      (while files
	(setq file (car files)
	      files (cdr files))
	(cond
	 ((file-directory-p file)
	  (or no-recursion
	      (string-match avoid-re file)
	      (setq list (append list (my-site-start-files file nil))) ) )
	 ((string-match my-site-start-file-name-regex file)
	  (if (string-match "\\`\\(.*\\.el\\)c?\\'" file)
	      (setq file (match-string 1 file)
		    elc (concat file "c") )
	    (error "%s is neither .el nor .elc" file) )
	  (and (file-exists-p file)
	       (file-exists-p elc)
	       (file-newer-than-file-p elc file)
	       (setq file elc) )
	  (add-to-list 'list file) ) ) ) )
    list) )

(defsubst my-site-start-split-filename (filename)
  (if (string-match "\\(\\`\\|/\\)\\(\\([0-9][0-9]\\).*\\)\\.elc?\\'" filename)
      (cons (string-to-number (match-string 3 filename))
	    (match-string 2 filename) )
    (error "\"%s\" does not look like a valid .el/.elc file name" filename) ) )
(defun my-site-start-sort-load-order (list)
  "Return the file names in LIST sorted numerically by basename.

This function assumes file names adhere to the convention of having a leading
numeric prefix to decide the load order, and will fail if this is not the
case."
  (sort list
	(function
	 (lambda (aa bb)
	   (save-match-data
	     (let ((a (my-site-start-split-filename aa))
		   (b (my-site-start-split-filename bb)))
	       (if (= (car a) (car b))
		   (string-lessp (cdr a) (cdr b))
		 (< (car a) (car b)) ) ) ))) ) )


(defun my-site-start-do-deferred-loads ()
  "Load all files whose loading was deferred from `my-site-start'.

Load all files from the list `my-site-start--deferred-load-files'.
The value of `my-site-stat--deferred-load-files' is then set to nil.

The default `my-site-start-interactive-setup-hook' calls this function.

By convention, features which are only useful for interactive use should
be deferred, which in practice means they will only be loaded when Emacs
is started for interactive use \(and not, for example, from within a script,
say, to batch compile some Lisp files\).

Furthermore, the default `my-site-start-defer-file-p' function encodes the
convention that file names with a numeric prefix larger than 99 will be
deferred.  See furthermore `my-site-start-defer-file-p-function' if you
wish to override this behavior."
  (mapc #'my-site-start-load my-site-start--deferred-load-files)
  (setq my-site-start--deferred-load-files nil) )

(defun my-site-start-defer-file-p (file)
  "Return non-nil if FILE has a numeric prefix strictly bigger than 99.

This function is used as `my-site-start-defer-file-p-function' by default,
and implements the simple policy that a file name with a numeric prefix
larger than 99 names a file whose loading should be deferred."
  (save-match-data
    (string-match "\\`\\(.*/\\)?0*[1-9][0-9][0-9][^/]*\\'" file) ) )


(defvar my-site-start-interactive-setup-hook
  (list (function my-site-start-do-deferred-loads))
"Hook run at the end of loading user's startup files, if running on a terminal.

This provides a facility for deferring loading of features which are only
useful in interactive use, but not e.g. when batch-compiling Elisp files.

By default, the hook runs `my-site-start-do-deferred-loads'.

Technically, this hook is run from `term-setup-hook' in turn.")

(add-hook 'term-setup-hook
	  #'(lambda nil (run-hooks 'my-site-start-interactive-setup-hook)) )



;; This isn't really meant to be `require'd, but what the hey

(provide 'my-site-start)

;;; my-site-start.el ends here
