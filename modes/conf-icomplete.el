;;; Code:
(require 'icomplete)

(setq icomplete-delay-completions-threshold 100
      icomplete-max-delay-chars 0
      icomplete-compute-delay 0.2
      completion-cycle-threshold 3
      ;; completion-show-help nil
      ;;TODO: values to explore
      ;; icomplete-show-matches-on-no-input
      ;; icomplete-tidy-shadowed-file-names
      ;; icomplete-in-buffer
      )

(icomplete-mode 1)
(fido-mode 1)

;; Functions
(defun distopico:icomplete-styles ()
  "Setup additional styles for icomplete/fido."
  (setq-local completion-styles '(basic initials partial-completion flex)))

;; Hooks
(add-hook 'icomplete-minibuffer-setup-hook 'distopico:icomplete-styles)

(provide 'conf-icomplete)
;;; conf-icomplete.el ends here
