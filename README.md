my-site-start
=============

This is a simple Elisp library to help you keep your `init.el` clean
and modular.  In brief,

* Adding new libraries is simple -- just create or symlink a new
directory in your `site-start.d` tree.  No need to explicitly
update your `load-path` etc.

* Detailed yet uncomplicated control over what gets loaded at
start-up, what gets loaded only when needed, and what gets loaded only
when Emacs is started in interactive mode -- a simple and
straightforward naming scheme decides what to load when.

You can start using it right away, and migrate your existing
configuration to a more modular layout by and by.


Introduction
------------

The purpose of `my-site-start` is to simplify maintenance of user libraries.
Instead of indefinitely tweaking your `.emacs`, just create a `site-start.d`
directory and add symlinks to the libraries you want to load into Emacs.

Of course, `my-site-start` itself needs to be configured in the old-fashioned
style;

    (autoload 'my-site-start "path/to/my-site-start" nil t)
    (my-site-start "~/.emacs.d/site-start.d/")

This will load all files matching `my-site-start-file-name-regex` in
`.emacs.d/site-start.d/`, including any symlinks and subdirectories.

The default policy is to recursively add all directories in the
`site-start.d` tree to the `load-path`, and from within these, to load
all files matching `[0-9][0-9]*.elc?`, those with a numeric prefix
below 100 immediately, and others deferred.

Directories are traversed depth-first and added to `load-path` in the
order they were found.  (This is not currently configurable.)

For example, assume the following directory structure:

    ~/
      .emacs                <- legacy; modern default is .emacs.d/init.el
      .emacs.d/
        fnord/              <- not in site-start.d, so ignored
          10foo.el          <- just to illustrate that only site-start.d ...
          bar.el            <- ... is traversed by default
        site-start.d/
          01globals.el
          50autoloads.el
          1000interactive.el
          darcsum/          <- symlink to ~/hack/darcsum/local-trunk
            50darcsum.el
            darcsum.el
            changelog
          bibtex.el         <- patched version to override system bibtex.el
          my-local-prefs/   <- symlink to ~/hack/my-local-prefs/production
            00globals.el
            50autoloads.el
            900cperl.el
            950rst.el

The following directories will be added to the front of `load-path`:

     ~/.emacs.d/site-start.d
     ~/.emacs.d/site-start.d/darcsum
     ~/.emacs.d/site-start.d/my-local-prefs

(The last one is not really useful to have on `load-path`, because all the
files in that directory will be loaded by `my-site-start` with an explicit
path, but this is of course by and large harmless.)

The following files will be loaded immediately, in this order:

     ~/.emacs.d/site-start.d/my-local-prefs/00globals.el
     ~/.emacs.d/site-start.d/01globals.el
     ~/.emacs.d/site-start.d/50autoloads.el
     ~/.emacs.d/site-start.d/my-local-prefs/50autoloads.el
     ~/.emacs.d/site-start.d/darcsum/50darcsum.el

(By no coincidence, this naming convention copies the one used by Debian
in `/etc/emacs/site-start.d` -- if you have a local, private copy of
a package with Debian startup files, you can use it directly,
without modification.)

The following files will be loaded deferred, in this order:

     ~/.emacs.d/site-start.d/my-local-prefs/900cperl.el
     ~/.emacs.d/site-start.d/my-local-prefs/950rst.el
     ~/.emacs.d/site-start.d/1000interactive.el

Deferred loading happens only when Emacs is running in interactive
mode, when all other initialization has completed.  It is thus a
useful facility for loading some user-interface functionality only
when you need it (and avoid the overhead when running Emacs in
batch mode, i.e. when compiling packages etc) and also offers a
convenient place for Lisp snippets you only want to load when
everything else is done.

This is probably how you want to run `my-site-start` -- just set it up
and forget it, except when you want to add a new package, and instead
forget the tedium of polluting your Emacs startup file with yet another
trivial `load-path` tweak and keybindings you only use interactively.

However, see the documentation strings in the code for more flexible
and/or more restricted usage scenarios.

There is a helper script `add-to-site-start` which you might want to
symlink to your `$HOME/bin` for conveniently adding new directories
to `site-start.d` as symlinks.


What Now?
---------

Fundamentally, `my-site-start` is just a simple but versatile platform
for turning a monolithic Emacs configuration into a modular one.
The process is straightforward and can probably be completed in a
few hours, but if you don't want to go all-in, just start using
`my-site-start` and amortize on the modularization work whenever
you need to tweak your Emacs configuration anyway.

For example, assuming you have a section dedicated to TeX in your
`.emacs.d/init.el`, you might start by moving the entire section to
a new file `my-tex.el` in `site-start.d` -- it will be added to your
`load-path`, so now you could do something simple like

    ;;; 500my-tex.el -- load TeX stuff when visiting a .tex file

    (eval-after-load "auctex" '(load-library "my-tex"))

    ;;; 500my-tex.el ends here

and instantly remove all the interactive TeX helpers out of your way
when you are not using Emacs for TeX work.

Proceed with other sections of your Emacs startup file when you feel
the need or have the time -- fairly soon, you might find yourself with
an Emacs startup file which only contains the `my-site-start`
bootstrap code.


See Also
--------

This repository used to live at http://porkmail.org/elisp but the
old location is no longer kept up to date, and will disappear soon.

For the competition, see http://www.emacswiki.org/emacs/DotEmacsModular


License
-------

GPL v2.
