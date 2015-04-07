(require 'python)
(require 'python-environment)
(require 'ac-python)

(add-hook 'python-mode-hook 'auto-complete-mode)

;; Default python
(setq python-indent-offset 4
      python-environment-virtualenv '("virtualenv2" "--system-site-packages" "--quiet")
      python-indent-guess-indent-offset nil
      python-environment-directory (in-emacs-d ".virtualenv/")
      python-environment-default-root-name "distopico")

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (setq auto-indent-assign-indent-level 4)
            (rainbow-delimiters-mode t)
            )
          )

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


(provide 'conf-python)
