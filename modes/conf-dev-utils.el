;;; Code:
(require 'skewer-mode)
(require 'skewer-repl)
;;(require 'simple-httpd)
;;(require 'skewer-mode)
;; (require 'skewer-html)
;; (require 'skewer-css)

;; Skewer-mode
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; Functions
(defun distopico:skewer-start ()
  (interactive)
  (let ((httpd-port 8023))
    (httpd-start)
    (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))

(defun distopico:skewer-demo ()
  (interactive)
  (let ((httpd-port 8024))
    (run-skewer)
    (skewer-repl)))

(defun distopico:restclient-mode-hook ()
  "Hooks for setup `restclient-mode'."
  (add-to-list (make-local-variable 'company-backends) 'company-restclient))

;; Hooks
(add-hook 'restclient-mode-hook #'distopico:restclient-mode-hook)

(provide 'conf-dev-utils)

;;; conf-dev-utils.el ends here
