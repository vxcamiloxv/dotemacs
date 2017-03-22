;;; Code:
(require 'jdee)

(setq jdee-mode-line-format (distopico:powerline-theme))

(defun distopico:java-mode-hook ()
  "The jdee-mode hook."
  (gtags-mode t)
  (gradle-mode t))

;; Hooks
(add-hook 'java-mode-hook #'distopico:java-mode-hook)

(provide 'conf-java)
