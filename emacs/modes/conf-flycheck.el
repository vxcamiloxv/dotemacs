(provide 'conf-flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

;;Color
(require 'flycheck-color-mode-line)

(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(setq flycheck-check-syntax-automatically '(save))

(setq flycheck-highlighting-mode 'columns)
(setq flycheck-indication-mode 'right-fringe)

(set-face-attribute 'flycheck-error nil :underline '(:style wave :color "Black1"))
