(deftheme actuator)

(add-hook 'minibuffer-setup-hook 'actuator--minibuffer-setup)

(defun actuator--minibuffer-setup ()
       (set (make-local-variable 'face-remapping-alist)
          '((org-document-title :height 1.0))))

(let ((blue  "#29BEEA")
      (white "#FFFFFF")
      (dark  "#000000")
      (cyan  "#00FFFF")
      (red   "#8b0000")
      (grey  "#484e55")
      (lgrey "#bdc4cc")

      (highlight-dim "#B1F8FF")
      (code-background "#FFFFFF")

      (xgrey "#F5F6F8")
      (xbord "#EDF0F4"))

  (custom-theme-set-faces
   `actuator
   `(fixed-pitch ((t (:family "SF Mono" :weight light))))
   `(variable-pitch ((t (:family "SF Pro Text" :weight light))))
   `(default ((t (:background ,white :foreground ,dark :weight light))))
   `(cursor ((t (:background ,blue))))
   `(fringe ((t (:background ,white))))
   `(region ((t (:background ,highlight-dim))))
   `(highlight ((t (:background nil))))

   `(font-lock-builtin-face ((t (:weight bold))))
   `(font-lock-constant-face ((t (:weight bold))))
   `(font-lock-function-name-face ((t (:weight bold))))
   `(font-lock-keyword-face ((t (:weight bold))))
   `(font-lock-variable-name-face ((t (:weight bold))))
   `(font-lock-type-face ((t (:weight bold))))
   `(font-lock-string-face ((t (:weight thin))))

   `(font-lock-warning-face ((t (:foreground ,red))))
   `(font-lock-comment-face ((t (:weight thin))))
   `(font-lock-comment-delimiter-face ((t (:weight thin))))

   `(org-drawer ((t (:foreground ,lgrey))))
   `(org-special-keyword ((t (:inherit org-drawer
                                       :foreground ,dark))))
   `(org-block ((t (:background ,code-background :extend t))))
   `(org-block-begin-line ((t :inherit org-block)))
   `(link ((t :foreground ,dark
              :underline t)))
   `(org-document-title ((t (:foreground ,dark
                             :height 220))))


   `(mode-line ((t (:box (:line-width 5
                                      :color ,xgrey)
                         ;;:underline nil
                         ;;:overline ,xbord
                         :foreground ,dark
                         :background ,xgrey
                         :inherit variable-pitch
                         :height 110))))
   `(mode-line-inactive ((t (:inherit mode-line
                             :foreground ,lgrey))))


   ))

(provide-theme 'actuator)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; actuator-theme.el ends here
