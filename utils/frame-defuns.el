;;; Code:
;; Frame-related defuns

(defun distopico:opacity-modify (&optional dec)
  "Modify the transparency of the emacs frame; if DEC is t,
decrease the transparency, otherwise increase it in 5% steps.
Source: http://emacs-fu.blogspot.com/2009/02/transparent-emacs.html"
  (interactive "p")
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	 (oldalpha (if alpha-or-nil alpha-or-nil
		     100))
	 (newalpha (if dec (- oldalpha 5)
		     (+ oldalpha 5))))
    (when (and (>= newalpha frame-alpha-lower-limit)
	       (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(provide 'frame-defuns)

;;; end of frame-defuns.el
