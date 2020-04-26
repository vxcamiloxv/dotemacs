;;; Code:

(require 'company)
(require 'company-statistics)
(require 'company-quickhelp)
(require 'company-dabbrev)
(require 'company-dabbrev-code)

(defvar distopico:company-enable-yas t
  "Enable yasnippet for all backends.")
(defvar distopico:company-ignore-backend '(company-eclim)
  "Backends to be ignored by default.")

;; Give a change to company-mode
(setq company-dabbrev-other-buffers t
      company-dabbrev-code-other-buffers t
      ;;company-complete-number t
      company-idle-delay 0.2
      company-show-numbers t
      company-quickhelp-delay nil
      company-minimum-prefix-length 1
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil
      company-selection-wrap-around t
      company-tooltip-align-annotations t
      ;;company-transformers (company-sort-by-backend-importance company-sort-by-occurrence company-sort-prefer-same-case-prefix)
      company-statistics-file (in-emacs-d ".cache/company-statistics-cache.el")
      abbrev-file-name (in-emacs-d ".cache/abbrev_defs"))

(add-to-list 'company-transformers
             'company-sort-by-occurrence 'append)

;; Custom shortcurts
(eval-after-load 'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

;; Functions
;;;###autoload
(defun distopico:backend-with-yas (backends)
  "Add support of yasnippet to `BACKENDS'."
  (remove nil (mapcar (lambda (backend)
            (unless (member backend distopico:company-ignore-backend)
              (if (or (not 'distopico:company-enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
                  backend
                (append (if (consp backend) backend (list backend))
                        '(:with company-yasnippet)))))
          backends)))

(defun distopico:after-init-hook ()
  "Hooks when Emacs init."
  (setq company-backends (distopico:backend-with-yas company-backends))
  (global-company-mode t)
  (company-quickhelp-mode t)
  (company-statistics-mode t))

;; Run
(add-hook 'after-init-hook 'distopico:after-init-hook)

(provide 'conf-autocomplete)
