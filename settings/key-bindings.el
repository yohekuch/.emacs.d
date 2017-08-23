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

;; Replace rectangle-text with inline-string-rectangle
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; Move more quickly
(global-set-key (kbd "C-S-n") (λ (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (λ (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (λ (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (λ (ignore-errors (backward-char 5))))

;; Use Shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

;; Jump to a definition in the current file. (This is awesome)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-b") 'quick-switch-buffer)

(global-set-key (kbd "s-y") 'bury-buffer)

;; Navigation bindings
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; File finding
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

;; killing text
(global-set-key (kbd "M-h") 'kill-region-or-backward-word)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status-fullscreen)
(autoload 'magit-status-fullscreen "magit")

;; Eval buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Buffer file functions
(global-set-key (kbd "C-x t") 'touch-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; expand-regionn
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-M-'") 'er/contract-region)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Navigation bindings
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; prodigy
(global-set-key (kbd "C-x M-m") 'prodigy)

;; Visual regexp
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

;; Use tab to complete
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)

;; C-n, C-p to select
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; enable mozc-popup
(global-set-key "\C-o" 'mozc-mode)

;; Find file in project
(global-set-key (kbd "C-x o") 'find-file-in-current-directory)

;; Find file in project, with specific patterns
(global-unset-key (kbd "C-x C-o")) ;; which used to be delete-blank-lines (also bound to C-c C-<return>)
(global-set-key (kbd "C-x C-o ja") (ffip-create-pattern-file-finder "*.java"))
(global-set-key (kbd "C-x C-o js") (ffip-create-pattern-file-finder "*.js"))
(global-set-key (kbd "C-x C-o jn") (ffip-create-pattern-file-finder "*.json"))
(global-set-key (kbd "C-x C-o ht") (ffip-create-pattern-file-finder "*.html"))
(global-set-key (kbd "C-x C-o jp") (ffip-create-pattern-file-finder "*.jsp"))
(global-set-key (kbd "C-x C-o cs") (ffip-create-pattern-file-finder "*.css"))
(global-set-key (kbd "C-x C-o ft") (ffip-create-pattern-file-finder "*.feature"))
(global-set-key (kbd "C-x C-o cl") (ffip-create-pattern-file-finder "*.clj"))
(global-set-key (kbd "C-x C-o el") (ffip-create-pattern-file-finder "*.el"))
(global-set-key (kbd "C-x C-o ed") (ffip-create-pattern-file-finder "*.edn"))
(global-set-key (kbd "C-x C-o md") (ffip-create-pattern-file-finder "*.md"))
(global-set-key (kbd "C-x C-o rb") (ffip-create-pattern-file-finder "*.rb"))
(global-set-key (kbd "C-x C-o or") (ffip-create-pattern-file-finder "*.org"))
(global-set-key (kbd "C-x C-o ph") (ffip-create-pattern-file-finder "*.php"))
(global-set-key (kbd "C-x C-o tx") (ffip-create-pattern-file-finder "*.txt"))
(global-set-key (kbd "C-x C-o vm") (ffip-create-pattern-file-finder "*.vm"))
(global-set-key (kbd "C-x C-o xm") (ffip-create-pattern-file-finder "*.xml"))
(global-set-key (kbd "C-x C-o in") (ffip-create-pattern-file-finder "*.ini"))
(global-set-key (kbd "C-x C-o pr") (ffip-create-pattern-file-finder "*.properties"))
(global-set-key (kbd "C-x C-o gr") (ffip-create-pattern-file-finder "*.groovy"))
(global-set-key (kbd "C-x C-o ga") (ffip-create-pattern-file-finder "*.gradle"))
(global-set-key (kbd "C-x C-o sc") (ffip-create-pattern-file-finder "*.scala"))
(global-set-key (kbd "C-x C-o ss") (ffip-create-pattern-file-finder "*.scss"))
(global-set-key (kbd "C-x C-o co") (ffip-create-pattern-file-finder "*.conf"))
(global-set-key (kbd "C-x C-o j2") (ffip-create-pattern-file-finder "*.j2"))
(global-set-key (kbd "C-x C-o sh") (ffip-create-pattern-file-finder "*.sh"))
(global-set-key (kbd "C-x C-o ic") (ffip-create-pattern-file-finder "*.ico"))
(global-set-key (kbd "C-x C-o sv") (ffip-create-pattern-file-finder "*.svg"))
(global-set-key (kbd "C-x C-o py") (ffip-create-pattern-file-finder "*.py"))
(global-set-key (kbd "C-x C-o rc") (ffip-create-pattern-file-finder "*rc"))
(global-set-key (kbd "C-x C-o !") (ffip-create-pattern-file-finder "*"))

;; http://d.hatena.ne.jp/rubikitch/20101126/keymap
;; select window
(define-minor-mode overriding-minor-mode
  "override"
  t
  ""
  `((,(kbd "C-t") . other-window)
    (,(kbd "M-t") . (lambda ()
                      (interactive)
                      (other-window -1)))))

(provide 'key-bindings)
