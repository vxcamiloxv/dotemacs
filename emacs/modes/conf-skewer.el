;;; Code:
(require 'skewer-mode)

;; skewer-mode
(when (require 'skewer-mode nil 'noerror)
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))


(provide 'conf-skewer)
