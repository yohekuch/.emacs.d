;; launch apps
(defun uenox-dired-winstart ()
  "Type '[uenox-dired-winstart]': win-start the current line's file."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename)))
        (w32-shell-execute "open" fname)
        (message "win-started %s" fname))))

;; keybinding
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "z" 'uenox-dired-winstart)))

(defun command-shell ()
  "opens a shell which can run programs as if run from cmd.exe from Windows"
  (interactive)
  (let ((explicit-shell-file-name "cmdproxy")
        (shell-file-name "cmdproxy") (comint-dynamic-complete t))
    (shell)))

;; Don't rob emacs of super key
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)

(provide 'cygwin)
