(require 'json)
(require 'web-beautify)

;;(defun beautify-json ()
;;  (interactive)
;;  (let ((b (if mark-active (min (point) (mark)) (point-min)))
;;        (e (if mark-active (max (point) (mark)) (point-max))))
;;    (shell-command-on-region b e
;;     "python -mjson.tool" (current-buffer) t)))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c C-b f") 'web-beautify-js))

(defun json-pretty-print-buffer ()
  (interactive)
  (let ((json-encoding-pretty-print t))
      (let ((json-string (json-encode (json-read-from-string (buffer-string))))
            (buf (current-buffer)))
        (with-current-buffer buf
          (erase-buffer)
          (insert json-string)))))

(defun json-pretty-print ()
  (interactive)
  (unless mark-active
    (error "No region selected."))
  (let ((begin (region-beginning))
        (end (region-end)))
    (kill-region begin end)
    (let ((json-encoding-pretty-print t))
      (insert (json-encode (json-read-from-string (current-kill 0)))))))

(provide 'conf-json)
