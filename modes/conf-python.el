;;; Code:

(require 'python)
(require 'python-environment)
(require 'pony-mode)
(require 'company)
;;(require 'python-django)

;; Autoload
(autoload 'jedi "jedi" "Jedi for python" t)
(autoload 'jedi:setup "jedi" "Jedi setup" t)

;; Default python
(setq python-indent-offset 4
      python-environment-virtualenv '("virtualenv2" "--system-site-packages" "--quiet")
      python-indent-guess-indent-offset nil
      python-environment-directory (in-emacs-d ".virtualenv/")
      python-environment-default-root-name "distopico")
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;(add-to-list 'auto-mode-alist '("\\.dtpl$" . pony-tpl-mode))

;; Jedi
(setq jedi:setup-keys t
      jedi:complete-on-dot t
      jedi:tooltip-method '(popup)
      jedi:tooltip-show nil)
(eval-after-load "python"
  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))

;; Functions
(defun distopico:python-mode-hook()
  (jedi:setup)
  (anaconda-mode t)
  (eldoc-mode t)
  (column-marker-1 80)

  (setq indent-tabs-mode nil
        tab-width 4
        auto-indent-assign-indent-level 4)
  (local-set-key (kbd "RET") 'newline-and-indent)

  (rainbow-delimiters-mode t)
  ;; Autocomplete with company in python
  (add-to-list 'company-backends 'company-jedi)
  (add-to-list 'company-backends 'company-anaconda))

;; Hooks
(add-hook 'python-mode-hook 'distopico:python-mode-hook)
(add-hook 'jedi-mode-hook 'jedi-direx:setup)



(provide 'conf-python)
