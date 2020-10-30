;;; Code:
(require 'geiser)
(require 'flycheck-guile)
(require 'rainbow-delimiters)

(setq geiser-mode-start-repl-p t
      geiser-default-implementation 'guile
      geiser-active-implementations '(guile racket))

(defun distopico:scheme-mode-hook ()
 "Scheme mode hook to enhance it."
 (enable-paredit-mode))

;; hooks
(add-hook 'scheme-mode-hook #'distopico:scheme-mode-hook)

(provide 'conf-scheme)
