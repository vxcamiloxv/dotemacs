;;; Code:
(require 'kotlin-mode)
(require 'lsp-mode)

;; Configuration
(setq lsp-keep-workspace-alive nil
      lsp-imenu-sort-methods '(position))

;; Hooks
(add-hook 'kotlin-mode-hook #'lsp)

(provide 'conf-kotlin)
