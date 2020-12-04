;;; Code:
(require 'rust-mode)
(require 'eglot)

(defun distopico:rust-mode-hook()
  "The `rust-mode' hook."
  (flycheck-mode -1) ;;; disable due eglot use flymake by default and I want to test it
  (ggtags-mode 1)
  (eglot-ensure))

(defun distopico:try-cargo-toml(dir)
  (when-let* (((eq major-mode 'rust-mode))
              (output
               (let ((default-directory dir))
                 (shell-command-to-string "cargo metadata --no-deps --format-version 1")))
              (json (ignore-errors (json-read-from-string output)))
              (found (cdr (assq 'workspace_root json))))
    (cons 'transient  found)))

;; Hooks
(add-hook 'rust-mode-hook #'distopico:rust-mode-hook)
(add-hook 'project-find-functions #'distopico:try-cargo-toml)

(provide 'conf-rust)
;;; conf-rust.el ends here
