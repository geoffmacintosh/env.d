;;; -*- lexical-binding: t; no-byte-compile: t; -*-
(let ((gc-cons-threshold (* 5000 1024 1024))
      (debug-on-error t))

  (defmacro csetq (variable value)
    "Alternative to \"setq\" that works with custom-set.
Should be a drop-in replacement in absolutely all cases. Use
identically to setq, setting VARIABLE to VALUE."
    `(funcall (or (get ',variable 'custom-set)
                  'set-default)
              ',variable ,value))

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

  (straight-use-package 'use-package)
  (use-package no-littering
    :straight t)
  ;;(straight-use-package '(org-plus-contrib :includes (org)))
  (straight-use-package 'org)
  (mapc #'org-babel-load-file
        (directory-files user-emacs-directory t "\\.org$"))
  (garbage-collect))

(provide 'init)
;;; init.el ends here
