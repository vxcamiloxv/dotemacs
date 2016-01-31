;;; Code:
(require 'skewer-mode)
(require 'skewer-repl)
(require 'mouse-slider-mode)
;;(require 'simple-httpd)
;;(require 'skewer-mode)
;; (require 'skewer-html)
;; (require 'skewer-css)

;; Skewer-mode
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; Functions
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

(add-to-list 'mouse-slider-mode-eval-funcs
             '(js2-mode . skewer-eval-defun))

(provide 'conf-dev-utils)
