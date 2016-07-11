;;; editing-defuns.el --- Basic text editing defuns -*- lexical-binding: t; -*-

;; kill region if active, otherwise kill backward word
(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
