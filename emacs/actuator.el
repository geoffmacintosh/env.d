;;; -*- lexical-binding: t; -*-


(use-package use-package-ensure-system-package
  :straight t)

(use-package system-packages
  :straight t
  :custom
  (system-packages-use-sudo nil)
  (system-packages-package-manager 'nix))

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

(use-package prescient
  :straight t
  :custom
  (prescient-persist-mode 1)
  (prescient-history-length 10000)
  (prescient-aggressive-file-save t))

(use-package counsel
  :functions counsel-mode
  :custom
  (counsel-find-file-ignore-regexp "\(?:\‘[#.]\)\|\(?:[#~]\’\)\|\'\.") ;; Doesn't work
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
  (define-key ivy-minibuffer-map (kbd "<C-return>") #'ivy-immediate-done)
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

(use-package smart-tab
  :straight t
  :functions global-smart-tab-mode
  :config
  (global-smart-tab-mode 1)
  :custom
  (smart-tab-using-hippie-expand t)
  (smart-tab-completion-functions-alist nil))

(use-package fish-mode
  :straight t)
(use-package gitconfig-mode
  :straight t)
(use-package gitignore-mode
  :straight t)
(use-package lua-mode
  :straight t)
(use-package toml-mode
  :straight t)

(provide 'actuator)
;;; actuator.el ends here
