(require 'navi2ch)

;; 2chproxy.pl -> ENABLE_2CH_TO_nCH => 0
(setq navi2ch-list-valid-host-regexp
      (concat "\\("
              (regexp-opt '(".2ch.net" ".5ch.net" ".bbsp=ink.com" ".mach=ibbs.com" ".mach=i.to"))
              "\\)\\'"))
(setq navi2ch-list-bbstable-url "http://menu.5ch.net/bbstable.html")

;; Set http-proxy for navi2ch.
(setq navi2ch-net-http-proxy "127.0.0.1:8080")

;; Do not use proxy while sending message.
(setq navi2ch-net-send-message-use-http-proxy nil)

(defun navi2ch-article-id-color (id)
  "IDを元に色を表示する。最初の4桁でforeground、次の4桁でbackgroundを表す。
IDに使われる文字は +/[0-9][A-Z][a-z] の64文字、これが4文字分なので
64^4 = 2^24となり丁度24bitカラーで表現できる。"
  (let ((id-index 0)
        (id-char "")
        (color-code 0)
        (id-char-index 0))
    (while (< id-index 4)
      (setq id-char (substring id id-index (1+ id-index)))
      (setq color-code (* color-code 64))
      (setq id-char-index (string-match id-char "+/0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
      (when id-char-index
        (setq color-code (+ color-code id-char-index)))
      (setq id-index (1+ id-index)))
    (format "#%x" color-code)))

(defun navi2ch-article-default-header-format-function (number name mail date-id)
  "デフォルトのヘッダをフォーマットする関数。
ヘッダの face を付けるのもここで。"
  (when (string-match (concat "\\`" navi2ch-article-number-number-regexp
			      "\\'")
		      name)
    (navi2ch-article-set-link-property-subr (match-beginning 0)
					    (match-end 0)
					    'number
					    (match-string 0 name)
					    name))
  (let* ((from-header (navi2ch-propertize "From: "
					 'face 'navi2ch-article-header-face))
        (from (navi2ch-propertize (concat (format "[%d] " number)
					  name
					  (format " <%s>\n" mail))
				  'face 'navi2ch-article-header-contents-face))
        (date-header (navi2ch-propertize "Date: "
					 'face 'navi2ch-article-header-face))
	(date (navi2ch-propertize (concat (car (split-string date-id "ID:")) "ID:")
				  'face
				  'navi2ch-article-header-contents-face))
        (id-raw (if (equal (cadr (split-string date-id "ID:")) nil)
                    "++++++++"
                  (cadr (split-string date-id "ID:"))))
	(id (navi2ch-propertize id-raw
                                'face
                                (list t
                                      :box
                                      '(:line-width 1 :color "white")
                                      :foreground
                                      (navi2ch-article-id-color
                                       (substring id-raw 4 8))
                                      :background
                                      (navi2ch-article-id-color
                                       (substring id-raw 0 4)))))
	(start 0) next)
    (while start
      (setq next
	    (next-single-property-change start 'navi2ch-fusianasan-flag from))
      (when (get-text-property start 'navi2ch-fusianasan-flag from)
	(add-text-properties start (or next (length from))
			     '(face navi2ch-article-header-fusianasan-face)
			     from))
      (setq start next))
    (concat from-header from date-header date id "\n\n")))

(provide 'setup-navi2ch)
