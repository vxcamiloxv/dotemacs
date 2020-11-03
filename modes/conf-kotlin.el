;;; Code:
(require 'kotlin-mode)

;; Hooks
(add-hook 'kotlin-mode-hook #'eglot-ensure)

(provide 'conf-kotlin)
