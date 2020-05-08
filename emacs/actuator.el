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
