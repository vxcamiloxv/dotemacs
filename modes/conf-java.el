;;; Code:
(require 'jdee)
(require 'meghanada)
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)

;; Control
(defconst distopico:androidmanifest-regexp
  (concat "\\`" (regexp-quote "AndroidManifest.xml") "\\'"))

;; Config
(setq jdee-maven-disabled-p nil
      jdee-launch-beanshell-on-demand-p nil
      jdee-complete-add-space-after-method t
      jdee-mode-line-format (distopico:powerline-theme)
      jdee-server-dir (in-emacs-d "external/jdee-server/target/")
      meghanada-server-install-dir (in-emacs-d "external/meghanada-server/"))

(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

;; Remove auto jdee auto-init
(setq auto-mode-alist (remove '("\\.java\\'" . jdee-mode) auto-mode-alist))

;; Functions
(defun distopico:point-in-defun-declaration-p ()
  "Check if the line is a function declaration."
  (let ((bod (save-excursion (c-beginning-of-defun)
                             (point))))
    (<= bod
        (point)
        (save-excursion (goto-char bod)
                        (re-search-forward "{")
                        (point)))))

(defun distopico:arglist-cont-nonempty-indentation (arg)
  "Fix `ARG' list indentation."
  (unless (distopico:point-in-defun-declaration-p) '++))

(defun distopico:java-mode-hook ()
  "The jdee-mode hook."
  (ggtags-mode t)
  (gradle-mode t)

  ;; meghanada-mode on
  (meghanada-mode t)
  ;; Fix anotation indexation
  (make-local-variable 'c-comment-start-regexp)
  (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table)

  ;; Ignore some files by default
  (add-to-list (make-local-variable 'projectile-globally-ignored-directories) ".meghanada")
  ;; use code format
  ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
  
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
(add-hook 'nxml-mode-hook #'distopico:nxml-mode-hook)

(provide 'conf-java)
