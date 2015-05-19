;;; Code:

(require 'python)
(require 'python-environment)
(require 'pony-mode)
(require 'company)
(require 'ac-python)
;;(require 'python-django)

;; Default python
(setq python-indent-offset 4
      python-environment-virtualenv '("virtualenv2" "--system-site-packages" "--quiet")
      python-indent-guess-indent-offset nil
      python-environment-directory (in-emacs-d ".virtualenv/")
      python-environment-default-root-name "distopico")
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;(add-to-list 'auto-mode-alist '("\\.dtpl$" . pony-tpl-mode))

;; Hooks
(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (anaconda-mode t)
            (eldoc-mode t)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (setq auto-indent-assign-indent-level 4)
            (rainbow-delimiters-mode t)
            ;; Autocomplete with company in python
            (add-to-list 'company-backends 'company-jedi)
            (add-to-list 'company-backends 'company-anaconda)
            ))



(provide 'conf-python)
