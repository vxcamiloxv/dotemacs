;;; Code:

(require 'magit)

(setq magit-last-seen-setup-instructions "1.4.0"
      magit-revert-item-confirm t
      magit-completing-read-function 'magit-ido-completing-read)

;; Return to magit status
(remove-hook 'server-switch-hook 'magit-commit-diff)

(provide 'conf-magit)
