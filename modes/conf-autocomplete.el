;;; Code:

(require 'company)
(require 'company-statistics)
(require 'company-quickhelp)
(require 'company-dabbrev)
(require 'company-dabbrev-code)


;; Give a change to company-mode
(setq company-dabbrev-other-buffers t
      company-dabbrev-code-other-buffers t
      ;;company-complete-number t
      company-idle-delay 0.2
      company-show-numbers t
      company-quickhelp-delay nil
      company-minimum-prefix-length 1
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil
      company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-statistics-file (in-emacs-d ".cache/company-statistics-cache.el")
      abbrev-file-name (in-emacs-d ".cache/abbrev_defs"))

(add-to-list 'company-transformers
             'company-sort-by-occurrence 'append)

;; Custom shortcurts
(eval-after-load 'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

;; Run
(add-hook 'after-init-hook 'distopico:after-init-hook)

;; Functions
(defun distopico:after-init-hook ()
  "Hooks when Emacs init."
  (global-company-mode t)
  (company-quickhelp-mode t)
  (company-statistics-mode t))


(provide 'conf-autocomplete)
