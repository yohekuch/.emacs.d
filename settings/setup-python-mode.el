(setq python-shell-interpreter "python3")
(pyvenv-activate "~/Dropbox/hacks/pyscrape/scraping/")

;; Traditional terminal console like keybindings
(define-key inferior-python-mode-map (kbd "C-p") 'comint-previous-input)
(define-key inferior-python-mode-map (kbd "C-n") 'comint-next-input)

(provide 'setup-python-mode)
