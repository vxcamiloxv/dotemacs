;;; Code:
(require 'web-mode)
(require 'emmet-mode)
(require 'web-beautify)
(require 'company-tern)

(define-derived-mode web-jsx-mode web-mode "web-jsx")

(add-to-list 'interpreter-mode-alist '("react" . web-jsx-mode))
(add-to-list 'interpreter-mode-alist '("jsx" . web-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-jsx-mode))
(add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . web-jsx-mode))
(add-to-list 'magic-mode-alist '("^import React" . web-jsx-mode))

;; Beautify with js
(eval-after-load 'web-jsx-mode
  '(define-key web-jsx-mode-map (kbd "C-c C-b f") 'web-beautify-js))

;; Flycheck
(with-eval-after-load 'flycheck
  (dolist (checker '(javascript-eslint javascript-standard javascript-jshint))
    (flycheck-add-mode checker 'web-jsx-mode)))

;; Functions
(defun distopico:web-jsx-mode-hook ()
  "Adjust web-mode to JSX from spacemacs."
  (emmet-mode t)
  (tern-mode t)
  (ggtags-mode t)
  ;; Support to imenu
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
  (setq-local emmet-expand-jsx-className? t)
  ;; Enable js-mode snippets
  (yas-activate-extra-mode 'js-mode)
  ;; Force jsx content type
  (web-mode-set-content-type "jsx")
  ;; Don't auto-quote attribute values
  (setq-local web-mode-enable-auto-quoting nil)
  ;; Enable JSDoc
  (setq-local web-mode-enable-comment-annotation t)
  ;; Fix some indentation problems
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  ;; Common JS setup
  (distopico:js-common-setup))

;; Hooks
(add-hook 'web-jsx-mode-hook 'distopico:web-jsx-mode-hook)


(provide 'conf-jsx)
