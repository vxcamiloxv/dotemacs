;;; Code:
(require 'web-beautify);
(require 'js2-mode)
(require 'json-mode)
(require 'css-mode)
(require 'web-mode)

;; Formating beautify
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c C-b f") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c C-b f") 'web-beautify-js))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c C-b f") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c C-b f") 'web-beautify-css))


(provide 'conf-web-beautify)
