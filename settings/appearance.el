;; Highlight current line
(global-hl-line-mode 1)

;; set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))

;; Add sub-directorys to custom-theme-directory
(dolist
	(path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
	(add-to-list 'custom-theme-load-path path)))

;; color-theme
;(require 'color-theme)
;(color-theme-initialize)

;; Default theme
(defun use-default-theme ()
  (interactive)
;  (color-theme-monokai)
  (load-theme 'default-black)
  (set-default-font "Monaco-13")
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    '("MeiryoKe_console" . "unicode-bmp")))


(use-default-theme)

;; transparent
;; The first number is for the active window and the second is for the inactive window.
;(add-to-list 'default-frame-alist '(alpha . (80 80)))

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
(set-face-foreground 'show-paren-match-face "black")
(set-face-background 'show-paren-match-face "SkyBlue1")

;(add-hook 'input-method-activate-hook '(lambda () (set-cursor-color "red")))
(add-hook 'input-method-inactivate-hook '(lambda () (set-cursor-color "white")))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(provide 'appearance)
