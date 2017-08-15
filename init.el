(package-initialize)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set up appearance early
(require 'appearance)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
	(add-to-list 'load-path project)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(auto-complete
     dired-details              ;; make file details hide-able in dired
     elisp-slime-nav            ;; Make M-. and M-, work in elisp like they do in slime
     expand-region              ;; Increase selected region by semantic units.
     f                          ;; Modern API for working with files and directories
     fill-column-indicator      ;; Graphically indicate the fill column
     find-file-in-project       ;; Find files in a project quickly, on any OS
     flx
     flx-ido
     flycheck                   ;; On the fly syntax checking
     flycheck-pos-tip           ;; Flycheck errors display in tooltip
     gist                       ;; Emacs integration for gist.github.com
     guide-key
     highlight-escape-sequences ;; Highlight escape sequences
     htmlize                    ;; Convert buffer text and decorations to HTML 
     ido-at-point               ;; ido-style completion-at-point
     ido-vertical-mode
     magit
     multiple-cursors
     paredit
     perspective                ;; switch between named "perspectives" of the editor
     prodigy                    ;; Manage external services from within Emacs
     shell-command              ;; enables tab-completion for shell-command
     smartparens                ;; Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
	 smooth-scrolling
	 undo-tree
     visual-regexp
     whitespace-cleanup-mode    ;; Intelligently call whitespace-cleanup on save
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; C-h 2 backspace
(keyboard-translate ?\C-h ?\C-?)

;; C-x ? to help
(global-set-key "\C-x?" 'help-command)

;; Setup extentions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
;;(eval-after-load 'magit '(require 'setup-magit))
;;(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'shell '(require 'setup-shell))
;;(require 'setup-hippie)
;;(require 'setup-yasnippet)
;;(require 'setup-perspective)
;;(require 'ffip)
;;(require 'setup-html-mode)
;;(require 'setup-paredit)

(require 'prodigy)
(global-set-key (kbd "C-x M-m") 'prodigy)

;; Font lock dash.el
(eval-after-load "dash" '(dash-enable-font-lock))

;; Default setup of smartparens
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          restclient-mode-hook
          js-mode-hook
          ruby-mode
          markdown-mode)
  (add-hook it 'turn-on-smartparens-mode))

;; Language specific setup files
;;(eval-after-load 'js2-mode '(require 'setup-js2-mode))
;;(eval-after-load 'ruby-mode '(require 'setup-ruby-mode))
;;(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Load stuff on demand
;;(autoload 'skewer-start "setup-skewer" nil t)
;;(autoload 'skewer-demo "setup-skewer" nil t)
;;(autoload 'auto-complete-mode "auto-complete" nil t)
;;(eval-after-load 'flycheck '(require 'setup-flycheck))

;; Map files to modes
(require 'mode-mappings)

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Visual regexp
(require 'visual-regexp)
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'expand-region)
(require 'multiple-cursors)
(require 'delsel)
;;(require 'jump-char)
(require 'wgrep)
(require 'smart-forward)
(require 'change-inner)
;; (require 'multifiles)

;; Don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; Show expand-region command used
(setq er--show-expansion-message t)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Setup key bindings
(require 'key-bindings)

;; default browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program 
      (if (file-exists-p "/opt/firefox/firefox")
          "/opt/firefox/firefox"))


(require 'auto-complete-config)

(ac-config-default)

;; tabで補完を完了する
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)

;; C-n, C-pで補完メニューを選択する
(setq ac-use-menu-map t)
;; デフォルトで設定済み
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(add-to-list 'auto-mode-alist '("\\*rc$" . sh-mode))

