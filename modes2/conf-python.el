;;; package --- Conf Python Mode
;;;
;;; Commentary:
;;; All configuration for python mode
;;;
;;; Code:

(require 'python-mode)
;; (require 'python-django)
;; (require 'pony-mode)
;; (require 'python-pep8)
;; (require 'python-pylint)
;; (require 'ac-python)

(setq python-indent-guess-indent-offset nil)

;; Python IDE
;; (autoload 'python-mode "python-mode.el" "Python mode." t)
;; (setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))
;; (add-hook 'python-mode-hook 'auto-complete-mode)
;; (setq py-smart-indentation t)

(provide 'conf-python)

;;; conf-python.el ends here
