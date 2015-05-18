;;; Code:

(require 'emmet-mode)
;; (require 'ac-emmet)

(setq emmet-indentation 2)
(setq emmet-move-cursor-between-quotes nil)
(setq emmet-move-cursor-after-expanding t)

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; (add-hook 'web-mode-hook 'ac-emmet-html-setup)
;; (add-hook 'css-mode-hook 'ac-emmet-css-setup)
;; (setq ac-sources (append '(ac-source-emmet-css-snippets ) ac-sources))

(provide 'conf-emmet)
