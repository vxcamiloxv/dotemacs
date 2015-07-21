;;; Code:

(require 'magit)

(setq magit-last-seen-setup-instructions "1.4.0")

;; Return to magit status
(remove-hook 'server-switch-hook 'magit-commit-diff)

(provide 'conf-magit)
