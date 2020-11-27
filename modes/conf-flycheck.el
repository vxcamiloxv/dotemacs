;;; Code:
(require 'flycheck-color-mode-line)

(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; Config
(setq flycheck-check-syntax-automatically '(mode-enabled save)
      flycheck-highlighting-mode 'lines
      flycheck-indication-mode 'right-fringe)

;; Faces
(set-face-attribute 'flycheck-error nil :underline '(:style wave :color "#6D0900"))
(set-face-attribute 'flycheck-color-mode-line-error-face '(:inherit flycheck-fringe-error :foreground "red" :weight normal))

;; hooks
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'conf-flycheck)
;;; conf-flycheck ends here
