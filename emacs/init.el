;;; -*- lexical-binding: t; -*-
(let ((gc-cons-threshold (* 5000 1024 1024))
      (debug-on-error t))

  (setq straight-use-package-by-default t)
  
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (org-babel-load-file
   (expand-file-name "emacs.org" user-emacs-directory))
  (garbage-collect))
