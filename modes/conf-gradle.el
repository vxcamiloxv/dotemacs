;;; Code:
(require 'gradle-mode)
(require 'flycheck-gradle)

;; Control
(defconst distopico:gradle-file-regexp
  (concat "\\`" (regexp-quote "*.gradle") "\\'"))

(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

;; Functions
(defun distopico:gradle-hook ()
  "Hooks for gradle in modes like `nxml-mode',`java-mode',`kotlin-mode'."
  ;; Active gradle-mode if match manifest
  (cond
   ((distopico:locate-parent-file distopico:gradle-file-regexp)
    (gradle-mode t))))


;; Hooks
(mapc
 (lambda (mode)
   ;;(add-hook mode #'flycheck-gradle-setup)
   (add-hook mode #'distopico:gradle-hook))
 '(java-mode-hook kotlin-mode-hook nxml-mode-hook))

(provide 'conf-gradle)
