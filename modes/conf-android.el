;;; Code:
(require 'android-mode)
(require 'android-env)
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)

;; Control
(defconst distopico:android-manifest-regexp
  (concat "\\`" (regexp-quote "AndroidManifest.xml") "\\'"))

;; Functions
(defun distopico:android-hook ()
  "Hooks for android in modes like `nxml-mode',`java-mode',`kotlin-mode'."
  ;; Active android-mode if match manifest
  (cond
   ((distopico:locate-parent-file distopico:android-manifest-regexp)
    (android-mode t))))


;; Hooks
(mapc
 (lambda (mode)
   (add-hook mode #'distopico:android-hook))
 '(java-mode-hook kotlin-mode-hook nxml-mode-hook))

(provide 'conf-android)
