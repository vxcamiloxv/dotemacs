;(require 'simple-httpd)

(require 'skewer-mode)
(require 'skewer-repl)
(require 'skewer-html)
(require 'skewer-css)

(defun skewer-start ()
  (interactive)
  (let ((httpd-port 8023))
    (httpd-start)
    (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))

(defun skewer-demo ()
  (interactive)
  (let ((httpd-port 8024))
    (run-skewer)
    (skewer-repl)))

(require 'mouse-slider-mode)

(add-to-list 'mouse-slider-mode-eval-funcs
             '(js2-mode . skewer-eval-defun))

(provide 'conf-skewer)
