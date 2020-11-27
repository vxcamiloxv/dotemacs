;;; Code:
(require 'meghanada)
(require 'java-imports)

;; Configuration
(setq java-imports-find-block-function 'java-imports-find-place-sorted-block
      meghanada-server-install-dir (in-emacs-d "external/meghanada-server/"))

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

(defun distopico:inside-defun-declaration-p ()
  "Return non-nil if the point is the first statement inside of a lambda."
  (save-excursion
    (back-to-indentation)
    (looking-at "\\(new\s+\\|(\\(.*\\))\s+\\)")))

(defun distopico:arglist-cont-nonempty-indentation (_)
  "Fix parameters list indentation in function."
  (if (distopico:inside-defun-declaration-p)
      '+
    (unless (distopico:point-in-defun-declaration-p)
      '+)))

(defun distopico:c-lineup-arglist-operators (_)
  "Check if there java operator to setup indentation."
  (save-excursion
    (back-to-indentation)
    (when (looking-at "[-+:?|&*%<>=]\\|\\(/[^/*]\\)")
      '+)))

(defun distopico:setup-java-style ()
  "Setup a more common/generic java indentation style."
  ;; Fix indentation annotation inside params/functions
  (c-set-offset 'inexpr-class '0)
  ;; Fix indentation multi-line args
  (c-set-offset 'arglist-intro '+)
  ;; Fix indentation args list
  (c-set-offset 'arglist-cont-nonempty '(distopico:arglist-cont-nonempty-indentation c-lineup-gcc-asm-reg c-lineup-arglist))
  ;; Fix indentation args operators
  (c-set-offset 'arglist-cont '(distopico:c-lineup-arglist-operators 0))
  ;; Fix indentation switch case
  (c-set-offset 'case-label '+))

(defun distopico:java-mode-hook ()
  "The `java-mode' hook."
  (ggtags-mode t)
  (gradle-mode t)
  ;; Setup custom java style
  (distopico:setup-java-style)
  ;; meghanada-mode another opportunity, I test lsp-mode with java but not works fine with android
  (meghanada-mode t)
  ;; use code format
  ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)

  ;; Ignore some files by default
  (add-to-list (make-local-variable 'projectile-globally-ignored-directories) ".meghanada"))

;; Hooks
(add-hook 'java-mode-hook #'distopico:java-mode-hook)
(add-hook 'java-mode-hook #'java-imports-scan-file)

(provide 'conf-java)
;;; conf-java.el ends here
