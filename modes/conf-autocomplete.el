;;; Code:

(require 'company)
(require 'company-dabbrev)
(require 'company-dabbrev-code)


;; Give a change to company-mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-other-buffers t
      company-dabbrev-code-other-buffers t
      ;;company-complete-number t
      company-show-numbers t
      company-minimum-prefix-length 2
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-idle-delay 0.2)


(provide 'conf-autocomplete)
