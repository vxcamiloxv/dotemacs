(require 'eclim)
(require 'eclimd)

;;; Code:

;; Basic
(setq eclimd-default-workspace "~/Documents/Development/workspace"
      eclimd-wait-for-process nil)

;; Automatic display help messages
(setq help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.2)

(help-at-pt-set-timer)

;; Autocomplete
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)


(provide 'conf-eclim)
