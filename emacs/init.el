;;; -*- lexical-binding: t; -*-

(let ((gc-cons-threshold (* 5000 1024 1024))
      (debug-on-error t))
  (load (expand-file-name "actuator.el" user-emacs-directory)))
(garbage-collect)
