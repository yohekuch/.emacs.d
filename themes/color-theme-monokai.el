; based on http://www.monokai.nl/blog/2006/07/15/textmate-color-theme/
(require 'color-theme)
(defun color-theme-monokai ()
  (interactive)
  (color-theme-install
   '(color-theme-monokai
;;     ((background-color . "#272822")
	 ((background-color . "black")
      (foreground-color . "#F8F8F2")
;;      (cursor-color . "#DAD085"))
      (cursor-color . "white"))
     (default ((t (nil))))
     (modeline ((t (:background "white" :foreground "black"))))
     (font-lock-builtin-face ((t (:foreground "#A6E22A"))))
     (font-lock-comment-face ((t (:foreground "#75715E"))))
     (font-lock-constant-face ((t (:foreground "#AE81FF"))))
     (font-lock-doc-string-face ((t (:foreground "#E6DB74"))))
     (font-lock-string-face ((t (:foreground "#E6DB74"))))
     (font-lock-function-name-face ((t (:foreground "#A6E22E"))))
     (font-lock-keyword-face ((t (:foreground "#F92672"))))
     (font-lock-type-face ((t (:foreground "#89BDFF"))))
     (font-lock-variable-name-face ((t (:foreground "#F92672"))))
     (font-lock-warning-face ((t (:bold t :foreground "#FD5FF1"))))
     (highlight-80+ ((t (:background "#F92672" :foreground "#F8F8F0"))))
     (hl-line ((t (:background "#222222"))))
     (region ((t (:background "#49483E"))))
     (ido-subdir ((t (:foreground "#F92672"))))
    )
  )
)
(provide 'color-theme-monokai)