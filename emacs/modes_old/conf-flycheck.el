(add-hook 'after-init-hook #'global-flycheck-mode)

;;Color
(require 'flycheck-color-mode-line)

(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(setq flycheck-check-syntax-automatically '(save))

(custom-set-variables
 '(flycheck-highlighting-mode (quote columns))
 '(flycheck-indication-mode (quote right-fringe))
)

(set-face-attribute 'flycheck-error nil :underline '(:style wave :color "Black1"))
(set-face-attribute 'flycheck-color-mode-line-error-face '(:inherit flycheck-fringe-error :foreground "red" :weight normal))

(provide 'conf-flycheck)
