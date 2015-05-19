;;; Code:

(require 'auto-complete)
(require 'ac-capf)
(require 'company)
(require 'company-dabbrev)
(require 'company-dabbrev-code)

;; Give a change to company-mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-other-buffers t
      company-dabbrev-code-other-buffers t
      ;;company-complete-number t
      company-show-numbers t
      company-minimum-prefix-length 2
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-idle-delay 0.2)


;; (defun add-pcomplete-to-capf ()
;;   (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
;; (add-hook 'org-mode-hook #'add-pcomplete-to-capf)

;; ;; ;; Backends
(setq company-backends '(company-files)) ;;Now only need company-files for auto-complete dotn have this
;; ;; (push '(company-dabbrev :with company-yasnippet) company-backends)

;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda ()
;;              (set (make-local-variable 'company-backends) (append company-backends '(company-elisp)))))

(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories  "~/.emacs.d/ac-dict")
(ac-config-default)
;;(ac-linum-workaround)
;;(setq ac-delay 0.2)
;;(setq ac-auto-show-menu 0.3)
(setq ac-ignore-case nil
      ac-use-fuzzy t
      ac-use-comphist t)
;; Sources
(setq-default ac-sources (append ac-sources '(ac-source-imenu ac-source-filename ac-source-files-in-current-dir ac-source-yasnippet)))
(ac-capf-setup)
;; (require 'org-ac)
;; (org-ac/config-default)

(provide 'conf-autocomplete)
