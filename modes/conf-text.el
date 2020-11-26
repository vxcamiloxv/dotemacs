;;; Code:

;; act (more) like a word processor
(add-to-list 'auto-mode-alist '("\\.ll\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . text-mode))

;; Functions
(defun distopico:text-mode-hook ()
  "Hook when `text-mode' is enable."
  (auto-fill-mode 1)
  (flyspell-mode))

;; Hooks
(add-hook 'text-mode-hook #'distopico:text-mode-hook)

(provide 'conf-text)
