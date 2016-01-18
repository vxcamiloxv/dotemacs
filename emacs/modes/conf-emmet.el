;;; Code:

(require 'emmet-mode)

(setq emmet-indentation 2)
(setq emmet-move-cursor-between-quotes nil)
(setq emmet-move-cursor-after-expanding t)

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'less-mode-hook  'emmet-mode)

(provide 'conf-emmet)
