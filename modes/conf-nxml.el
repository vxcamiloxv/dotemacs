;;; Code:
(require 'nxml-mode)
(require 'editorconfig)

;; Config
(setq nxml-slash-auto-complete-flag t)

;; I want keep the same indentation for attribute, by default is nxml-child-inden*2
(add-to-list 'editorconfig-indentation-alist
             '(nxml-mode nxml-child-indent nxml-attribute-indent))

;; Functions
(defun distopico:nxml-mode-hook ()
  "Hooks for  `nxml-mode'."
  ;; Company-mode
  (add-to-list (make-local-variable 'company-backends)
               '(company-nxml company-dabbrev :with company-yasnippet)))

;; Hooks
(add-hook 'nxml-mode-hook #'distopico:nxml-mode-hook)

(provide 'conf-nxml)
