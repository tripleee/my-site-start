;;; dot.emacs --- skeletal .emacs for my-site-start	-*- emacs-lisp -*-

(defvar my-site-start-paths nil)	; Shut up compiler

(let ((base "/alt/var/tmp/indeed/hak/elisp"))
  (setq my-site-start-paths
	(list (cons base nil)		; Don't autoload
	      (cons (concat base	; Ditto
			    "/my-site-start") nil) ) )
  (nconc my-site-start-paths 
	 (mapc (function (lambda (p) (concat base "/" p)))
	       '("recover-buffers"
		 "darcsum/darcsum-local-stable"
		 "spam-complaint") ) ) )

(load-file "/opt/var/tmp/indeed/hak/elisp/my-site-start.el")

;(autoload 'recover-buffers "recover-buffers" nil t)
;(autoload 'spam-forward "spam-complaint" nil t)
;(autoload 'spam-forward-received "spam-complaint" nil t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cperl-close-paren-offset 0)
 '(cperl-continued-statement-offset 0)
 '(cperl-extra-newline-before-brace t)
 '(cperl-extra-newline-before-brace-multiline t)
 '(cperl-indent-level 4)
 '(cperl-label-offset -4)
 '(cperl-merge-trailing-else nil)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(emacs-lisp-mode-hook (quote (turn-on-eldoc-mode checkdoc-minor-mode)))
 '(gnus-select-method (quote (nnimap "mail.messagingengine.com" (nnimap-stream ssl))))
 '(message-default-mail-headers "Bcc: bcc@eeera.imap.cc
")
 '(save-place t nil (saveplace))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(use-dialog-box nil)
 '(user-full-name "era eriksson")
 '(user-mail-address "era@iki.fi"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)
(put 'overwrite-mode 'disabled t)

;;; dot.emacs ends here