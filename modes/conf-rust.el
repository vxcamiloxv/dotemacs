;;; Code:
(require 'rust-mode)
(require 'eglot)

(defun distopico:rust-mode-hook()
  "The `rust-mode' hook."
  (flycheck-mode -1) ;;; disable due eglot use flymake by default and I want to test it
  (ggtags-mode 1)
  (eglot-ensure))

;; Hooks
(add-hook 'rust-mode-hook #'distopico:rust-mode-hook)

(provide 'conf-rust)
;;; conf-rust.el ends here
