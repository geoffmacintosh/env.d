;;; -*- lexical-binding: t; -*-

(let ((gc-cons-threshold (* 5000 1024 1024))
      (debug-on-error t))

  (defmacro csetq (variable value)
    "Alternative to \"setq\" that works with custom-set.
Should be a drop-in replacement in absolutely all cases. Use
identically to setq, setting VARIABLE to VALUE."
    `(funcall (or (get ',variable 'custom-set)
		  'set-default)
	      ',variable ,value))

  (load (expand-file-name "actuator.el" user-emacs-directory)))
(garbage-collect)

(provide 'init)
;;; init.el ends here
