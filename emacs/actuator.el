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
