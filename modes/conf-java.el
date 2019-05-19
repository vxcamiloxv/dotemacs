;;; Code:
(require 'jdee)
(require 'meghanada)
(require 'java-imports)

;; Configuration
(setq jdee-maven-disabled-p nil
      jdee-launch-beanshell-on-demand-p nil
      jdee-complete-add-space-after-method t
      jdee-mode-line-format (distopico:powerline-theme)
      java-imports-find-block-function 'java-imports-find-place-sorted-block
      jdee-server-dir (in-emacs-d "external/jdee-server/target/")
      meghanada-server-install-dir (in-emacs-d "external/meghanada-server/"))

;; Disable jdee but left package just to test with non-android projects
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
  ;; meghanada-mode another opportunity, I test lsp-mode with java but not works fine with android
  (meghanada-mode t)
  ;; Fix anotation indexation
  (make-local-variable 'c-comment-start-regexp)
  (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
  ;; use code format
  ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)

  ;; Ignore some files by default
  (add-to-list (make-local-variable 'projectile-globally-ignored-directories) ".meghanada"))

;; Hooks
(add-hook 'java-mode-hook #'distopico:java-mode-hook)
(add-hook 'java-mode-hook #'java-imports-scan-file)

(provide 'conf-java)
