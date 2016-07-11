;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Use C-x C-m to do M-x per Steve Yegge's advice
(global-set-key (kbd "C-x C-m") 'smex)

;; File finding
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

(global-set-key (kbd "M-h") 'kill-region-or-backward-word)

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)

;; expand-regionn
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-M-'") 'er/contract-region)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(provide 'key-bindings)
