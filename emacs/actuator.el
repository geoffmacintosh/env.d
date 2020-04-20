;;; -*- lexical-binding: t; -*-

(defmacro csetq (variable value)
  "Alternative to \"setq\" that works with custom-set.
Should be a drop-in replacement in absolutely all cases. Use
identically to setq, setting VARIABLE to VALUE."
  `(funcall (or (get ',variable 'custom-set)
		'set-default)
	    ',variable ,value))

(defvar straight-use-package-by-default t)
(defvar straight-enable-use-package-integration t)

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


(declare-function straight-use-package "ext:straight")
(straight-use-package 'use-package)

(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/build/use-package" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "straight/build/bind-key" user-emacs-directory))
  (require 'use-package))

(use-package no-littering
  :straight t)

(use-package bind-key
  :straight t)

(use-package exec-path-from-shell
  :straight t
  :hook (after-init . exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package
  :straight t)

(use-package system-packages
  :straight t
  :custom
  (system-packages-use-sudo nil)
  (system-packages-package-manager 'nix))

(load-theme 'actuator t)

(blink-cursor-mode -1)
(csetq cursor-type 'box)

(csetq scroll-conservatively 101) ; Move the buffer just enough to display point, but no more
(csetq scroll-margin 0)
(csetq mouse-wheel-scroll-amount '(1))

(csetq inhibit-startup-message t)
(csetq initial-scratch-message "")

(use-package xt-mouse
  :straight nil
  :unless window-system
  :config
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  :custom
  (mouse-sel-mode t))

(use-package flymake
  :straight nil
  :hook (emacs-lisp-mode . flymake-mode))

(use-package locate
  :straight nil
  :custom
  (locate-command "mdfind"))

(use-package org
  :straight org-plus-contrib
  :config
  (org-indent-mode 1)
  (add-to-list 'org-babel-default-header-args
	       '(:mkdirp . "yes"))
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (shell      . t)))

  :bind
  ("C-c c" . counsel-org-capture)
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)

  :custom
  (org-startup-folded 'content)
  (org-ellipsis "→")
  (org-startup-align-all-tables t)
  (org-startup-shrink-all-tables t)
  (org-startup-with-inline-images t)
  (org-startup-indented t)
  (org-hide-leading-stars t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-hide-emphasis-markers t)
  (org-image-actual-width 300)
  (org-fontify-done-headline t)
  (org-structure-template-alist '(("e" . "src emacs-lisp")
				  ("s" . "src shell")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-closed-keep-when-no-todo t)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-complete-tags-always-offer-all-agenda-tags t)
  (org-clone-delete-id t)
  (org-tags-column -60)
  (org-catch-invisible-edits 'show-and-error)
  (org-insert-heading-respect-content t)
  (org-ctrl-k-protect-subtree t)
  (org-M-RET-may-split-line '((default . nil)))
  (org-special-ctrl-k t)
  (org-special-ctrl-a/e t)
  ;;(org-agenda-files (list org-directory))
  (org-blank-before-new-entry '((heading         . t)
				(plain-list-item . auto)))
  (org-use-property-inheritance t)
  (org-modules nil)
  :hook
  (;;(midnight-mode . org-refile-get-targets)
  (org-mode . visual-line-mode)))

(use-package org-list
  :straight nil
  :custom
  (org-list-indent-offset 1))

(use-package org-keys
  :straight nil
  :custom
  (org-use-speed-commands t))

;; (use-package org-refile
;;   :straight nil
;;   :custom
;;   (org-refile-allow-creating-parent-nodes 'confirm)
;;   (org-outline-path-complete-in-steps nil)
;;   (org-refile-use-outline-path 'file)
;;   (org-refile-targets '((org-agenda-files :maxlevel . 3))))

(use-package org-src
  :straight nil
  :custom
  (org-edit-src-persistent-message nil)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'other-frame)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-src-fontify-natively t))

(use-package org-footnote
  :straight nil
  :custom
  (org-footnote-auto-adjust t)
  (org-footnote-define-inline t)
  (org-footnote-auto-label 'random))

(use-package ob-core
  :straight nil
  :custom
  (org-confirm-babel-evaluate nil)
  (org-babel-results-keyword "results"))

(use-package recentf
  :straight nil
  :after no-littering
  :config
  (recentf-mode)
  :custom
  (recentf-max-saved-items 500)
  (recentf-exclude `(,no-littering-var-directory
		     ,no-littering-etc-directory))
  :hook (midnight . recentf-cleanup))

(use-package cus-edit
  :straight nil
  :after no-littering
  :custom
  (custom-file (expand-file-name "custom.el" no-littering-var-directory))
  :config
  (load custom-file 'noerror))

(use-package tex
  :straight auctex
  :custom
  (TeX-engine 'luatex))

(use-package org-cliplink
  :straight t
  :bind ("C-x p i" . org-cliplink))

(use-package org-attach
  :straight nil
  :custom
  (org-attach-store-link-p t)
  (org-attach-dir-relative t)
  (org-attach-preferred-new-method 'dir)
  (org-attach-method 'mv)
  (org-attach-auto-tag "attach")
  (org-attach-archive-delete 'query))

(use-package org-download
  :straight t
  :config
  (defun actuator-org-dl-annotate (_link)
    "Minimal org-download header info."
    (format "#+DOWNLOADED: %s\n"
	    (format-time-string "%Y-%m-%d")))
  :custom
  (org-download-method 'attach)
  (org-download-timestamp "")
  (org-download-annotate-function #'actuator-org-dl-annotate))

(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◆" "◆" "◇" "◇" "◇")))

(global-unset-key (kbd "<C-wheel-down>"))
(global-unset-key (kbd "<C-wheel-up>"))

(bind-key "M-c" 'capitalize-dwim)
(bind-key "M-l" 'downcase-dwim)
(bind-key "M-u" 'upcase-dwim)

(setq help-window-select t) ; Select help window by default
(setq jit-lock-defer-time 0) ; Delay font-lock if its slow

(fset 'yes-or-no-p 'y-or-n-p)

(use-package autorevert
  :straight nil
  :config
  (global-auto-revert-mode 1)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (revert-without-query t)
  (auto-revert-check-vc-info t))

(global-set-key (kbd "M-o") #'other-window)

(delete-selection-mode t)
(midnight-mode 1)
(setq sentence-end-double-space nil)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq indent-tabs-mode nil) ; Never insert tabs with tab key
(setq require-final-newline t)

(save-place-mode 1)

(setq backup-by-copying    t)
(setq delete-old-versions  t)
(setq kept-new-versions    50)
(setq kept-old-versions    5) ; I don't know what an old version is
(setq version-control      t)
(setq vc-make-backup-files t)

(setq uniquify-buffer-name-style 'forward) ; Like a path, the way that makes sense
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-strip-common-suffix nil)

(setq find-file-visit-truename nil) ; Don't resolve symlinks
(setq confirm-kill-emacs 'y-or-n-p)

(abbrev-mode)
(setq save-abbrevs 'silently)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(setq history-length 10000)
(setq history-delete-duplicates t)
(setq message-log-max 10000)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)

(setq-local default-directory "~/")

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(use-package savehist
  :straight nil
  :config
  (savehist-mode)
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(mark-ring
     kill-ring
     Info-history-list
     last-kbd-macro
     kmacro-ring
     register-alist
     global-mark-ring
     regexp-search-ring
     file-name-history
     shell-command-history
     compile-history
     command-history
     extended-command-history)))

(defun display-startup-echo-area-message ()
  "Remove the GNU info from the minibuffer on startup.
All you have to do is create a function with this name. It's
called automatically."
  (message ""))

(setq default-frame-alist
      '((ns-transparent-titlebar . t)
	(ns-appearance           . 'light)))

(setq truncate-partial-width-windows nil)
(toggle-truncate-lines 1) ; Don't wrap lines by default (overridden a lot)

(setq completion-styles
      '(fuzzy
	basic
	partial-completion
	substring
	initials
	emacs22))

(defun actuator-font-exists-p (font)
  "Return non-nil if FONT is loaded."
  (member font (font-family-list)))

(defun actuator-frame-init (&optional _frame)
  "Initialize per-frame variables.
These variables need to be set every time a frame is created."
  (when (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'tooltip-mode)    (tooltip-mode    -1))
  (when (and (not (display-graphic-p))
	     (fboundp 'menu-bar-mode))
    (menu-bar-mode   -1))
  (when (actuator-font-exists-p "SF Mono")
    (set-frame-font "SF Mono-14" nil t)))

(add-hook 'after-make-frame-functions 'actuator-frame-init)
(actuator-frame-init)

(use-package emacs
  :straight nil
  :after no-littering
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name "auto-save/" no-littering-var-directory) t)))
  (auto-save-mode))

(use-package eshell
  :straight nil
  :config
  (defalias 'eshell/f  'find-file-other-window)
  (defalias 'eshell/ff 'find-file)
  (defalias 'eshell/v  'view-file-other-window)
  (defalias 'eshell/vv 'view-file)

  (defun actuator-fish-path (path max-len)
    "Return a potentially trimmed-down version of the directory
 PATH, replacing parent directories with their initial characters
 to try to get the character length of PATH (sans directory
 slashes) down to MAX-LEN."
    (let* ((components (split-string (abbreviate-file-name path) "/"))
	   (len (+ (1- (length components))
		   (cl-reduce '+ components :key 'length)))
	   (str ""))
      (while (and (> len max-len)
		  (cdr components))
	(setq str (concat str
			  (cond ((= 0 (length (car components))) "/")
				((= 1 (length (car components)))
				 (concat (car components) "/"))
				(t
				 (if (string= "."
					      (string (elt (car components) 0)))
				     (concat (substring (car components) 0 2)
					     "/")
				   (string (elt (car components) 0) ?/)))))
	      len (- len (1- (length (car components))))
	      components (cdr components)))
      (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

  (defun actuator-eshell-autocomplete ()
    "Enable tab autocompletion in eshell."
    (define-key
      eshell-mode-map (kbd "<tab>")
      (lambda () (interactive) (pcomplete-std-complete))))

  (defun actuator-eshell-prompt ()
    (concat
     (propertize
      (actuator-fish-path (eshell/pwd) 1) 'face `(:foreground "grey" ))
     (propertize
      (if (= (user-uid) 0)
	  " # "
	" ❯ ") 'face `(:foreground "black"))))

  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t)
  (eshell-hist-ignoredups t)
  (eshell-history-size 10000)
  (eshell-banner-message "")
  (eshell-prompt-function #'actuator-eshell-prompt)
  (eshell-highlight-prompt nil)
  (eshell-prompt-regexp "^.*?[#❯] ")
  :hook
  (eshell-mode . (lambda ()
		   (require 'em-smart)
		   (eshell-smart-initialize)))
  (eshell-mode . actuator-eshell-autocomplete))

;;:hook (eshell-mode . actuator-eshell-autocomplete)

(defun actuator-just-one-space ()
  "Insert just one space, killing ALL whitespace."
  (interactive)
  (just-one-space -1))

(global-set-key (kbd "<M-SPC>") 'actuator-just-one-space)

(defun actuator-unfill-paragraph ()
  "Unfills a paragraph."
  (interactive)
  (let ((fill-column 'most-positive-fixed-num))
    (fill-paragraph)))
(global-set-key (kbd "C-M-q") 'actuator-unfill-paragraph)

(use-package magit
  :after exec-path-from-shell
  :bind ("C-c g" . magit-status)
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-save-repository-buffers 'dontask)
  (magit-section-initial-visibility-alist
   '((untracked . show)
     (unstaged  . show)
     (upushed   . show)
     ;;    (unpulled  . show)
     ;;    (stashes   . show)
     (recent    . show)))
  (magit-push-always-verify nil)
  (magit-revert-buffers 'silent)
  (magit-no-confirm '(stage-all-changes
		      unstage-all-changes))
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package prescient
  :custom
  (prescient-persist-mode 1)
  (prescient-history-length 10000)
  (prescient-aggressive-file-save t))

(use-package counsel
  :functions counsel-mode
  :config
  (counsel-mode 1)
  :bind
  ("C-x C-r" . counsel-recentf)
  ("C-x C-f" . counsel-find-file)
  ("M-x"     . counsel-M-x)
  ("s-x"     . counsel-M-x)
  ("C-x l"   . counsel-locate)
  ("C-h f"   . counsel-describe-function)
  ("C-h v"   . counsel-describe-variable)
  ("C-h k"   . counsel-descbinds)
  ("M-y"     . counsel-yank-pop))

(use-package ivy
  :defines ivy-minibuffer-map
  :functions ivy-mode ivy-immediate-done ivy-alt-done ivy-next-line
  :config
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "M-y") #'ivy-next-line)
  :custom
  (ivy-use-ignore-default 'always)
  (ivy-ignore-buffers '("*elfeed-log*"))
  (ivy-use-virtual-buffers nil)
  (ivy-count-format "(%d/%d) ")
  (ivy-extra-directories nil)
  :bind
  ("C-x b" . ivy-switch-buffer))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package ivy-posframe
  :disabled t
  :after ivy
  :if (display-graphic-p)
  :config
  (ivy-posframe-mode 1)
  :custom
  (ivy-posframe-display-functions-alist
   '((swiper                   . nil)
     (counsel-M-x              . ivy-posframe-display-at-frame-top-center)
     (ivy-completion-in-region . ivy-posframe-display-at-point)
     (t                        . ivy-posframe-display-at-frame-top-center))))

(use-package ivy-prescient
  :after (ivy prescient)
  :functions ivy-prescient-mode
  :config
  (ivy-prescient-mode 1))

(use-package hippie-exp
  :straight nil
  :bind ("s-/" . hippie-expand)
  :custom
  (hippie-expand-verbose t)
  (hippie-expand-try-functions-list
   '(try-expand-all-abbrevs
     try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-line
     try-expand-line-all-buffers ;;slow
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol ;; many, many completions
     try-expand-list
     try-expand-list-all-buffers
     try-expand-whole-kill)))

(defun actuator-hippie-unexpand ()
  "Remove an expansion without having to loop around."
  (interactive)
  (hippie-expand 0))
(global-set-key (kbd "<backtab>") #'actuator-hippie-unexpand)

(use-package smart-tab
  :functions global-smart-tab-mode
  :config
  (global-smart-tab-mode 1)
  :custom
  (smart-tab-using-hippie-expand t)
  (smart-tab-completion-functions-alist nil))

(add-hook 'emacs-startup-hook #'actuator-startup-profile)

(defun actuator-startup-profile ()
  "Displays startup time garbage collections in the modeline."
  (message "Emacs ready in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(use-package paren
  :straight nil
  :config
  (show-paren-mode)
  (electric-pair-mode 1)
  :custom
  (blink-matching-paren nil)
  (show-paren-delay 0)
  (show-paren-style 'mixed))

(use-package fish-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)
(use-package lua-mode)
(use-package toml-mode)

(use-package org-agenda
  :straight nil
  :custom
  (org-agenda-dim-blocked-tasks t))

(use-package org-id
  :straight nil
  :custom
  (org-id-link-to-org-use-id t)
  :hook (midnight-mode . org-id-update-id-locations))

(defun actuator-org-capture-turn-off-header-line ()
  "Disable the header-line in a local mode.
This is used to disable the help line in `org-capture' buffers as
there's no variable that will do it."
  (setq-local header-line-format nil))
(add-hook 'org-capture-mode-hook #'actuator-org-capture-turn-off-header-line)

(defun actuator-org-src-line-wrap-setup ()
  "Set truncate-lines-mode in org-source-editing buffers."
  (setq-local truncate-lines t))
(add-hook 'org-src-mode-hook #'actuator-org-src-line-wrap-setup)

(use-package org-crypt
  :straight nil
  :init
  (require 'org-crypt)
  :config
  (org-crypt-use-before-save-magic)
  ;;(add-to-list 'org-modules 'org-crypt)
  :custom
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  (org-crypt-key nil))

(provide 'actuator)
;;; actuator.el ends here
