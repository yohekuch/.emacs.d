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

;; swap the win-key and apps-key by keyswap the win app.
(setq w32-apps-modifier 'super)

(provide 'cygwin)
