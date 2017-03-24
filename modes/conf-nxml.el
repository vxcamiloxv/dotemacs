;;; Code:

;; Config
(setq nxml-slash-auto-complete-flag t)

;; Functions
(defun distopico:nxml-mode-hook ()
  "Hooks for  `nxml-mode'."
  ;; Company-mode
  (add-to-list (make-local-variable 'company-backends)
               '(company-nxml company-dabbrev :with company-yasnippet)))

;; Hooks
(add-hook 'nxml-mode-hook 'distopico:nxml-mode-hook)

(provide 'conf-nxml)
