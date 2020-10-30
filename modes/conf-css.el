;;
;;; Commentary:
;;
;; CSS/less/sass Mode
;;
;;; Code:

(require 'css-mode)
(require 'rainbow-mode)
(require 'skewer-less)
(require 'web-beautify)

(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c C-b f") 'web-beautify-css))

;; Hook
(dolist ($hook '(css-mode-hook less-css-mode-hook))
  (add-hook
   $hook (lambda ()
           (rainbow-mode t)
           (rainbow-delimiters-mode t))))

(add-hook 'less-css-mode-hook
          '(lambda ()
             (skewer-less-mode)))


(provide 'conf-css)
