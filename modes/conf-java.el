;;; Code:
(require 'jdee)
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)

;; Control
(defconst distopico:androidmanifest-regexp
  (concat "\\`" (regexp-quote "AndroidManifest.xml") "\\'"))

;; Config
(setq jdee-mode-line-format (distopico:powerline-theme)
      jdee-server-dir (in-emacs-d "external/jdee-server/target/")
      jdee-complete-add-space-after-method t)

(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))


(defun distopico:java-mode-hook ()
  "The jdee-mode hook."
  (ggtags-mode t)
  (gradle-mode t)
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    ;; Remove eclim backend
    (setq company-backends (delete 'company-eclim company-backends)))
  ;; Active android-mode if match manifiest
  (cond
   ((distopico:locate-parent-file distopico:androidmanifest-regexp)
    (android-mode t))))

(defun distopico:nxml-mode-hook ()
  "Hooks for  `nxml-mode'."
  ;; Active android-mode if match manifiest
  (cond
   ((distopico:locate-parent-file distopico:androidmanifest-regexp)
    (android-mode t))))

;; Hooks
(add-hook 'java-mode-hook #'distopico:java-mode-hook)
(add-hook 'nxml-mode-hook 'distopico:nxml-mode-hook)

(provide 'conf-java)
