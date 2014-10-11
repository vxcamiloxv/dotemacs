(require 'python)
(require 'ac-python)

(add-hook 'python-mode-hook 'auto-complete-mode)

(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (setq python-indent-offset 4)
    (setq tab-width 4)
    (local-set-key (kbd "RET") 'newline-and-indent)
    (setq python-indent-guess-indent-offset nil)
    (setq auto-indent-assign-indent-level 4)
    (rainbow-delimiters-mode t)
    )
)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


(provide 'conf-python)
