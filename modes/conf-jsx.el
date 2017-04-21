;;; Code:
(require 'web-mode)
(require 'emmet-mode)
(require 'web-beautify)
(require 'company-tern)

(define-derived-mode web-jsx-mode web-mode "web-jsx")

(add-to-list 'interpreter-mode-alist '("react" . web-jsx-mode))
(add-to-list 'interpreter-mode-alist '("jsx" . web-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.react.js\\'" . web-jsx-mode))
(add-to-list 'auto-mode-alist '("\\index.android.js\\'" . web-jsx-mode))
(add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . web-jsx-mode))
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
  "Adjust web-mode to accommodate jsx from spacemacs."
  (emmet-mode t)
  (tern-mode t)
  ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
  (setq-local emmet-expand-jsx-className? t)
  ;; Enable js-mode snippets
  (yas-activate-extra-mode 'js-mode)
  ;; Force jsx content type
  (web-mode-set-content-type "jsx")
  ;; Don't auto-quote attribute values
  (setq-local web-mode-enable-auto-quoting nil)

  ;; Add node_modules to exec path
  (distopico:add-node-modules-path)
  ;; Default checker
  (flycheck-select-checker 'javascript-jshint)
  ;; Add company backend for js
  (set (make-local-variable 'company-backends)
       '(company-bbdb
         company-nxml company-css
         company-semantic company-files
         (company-dabbrev-code company-gtags company-etags company-keywords company-tern :with company-yasnippet)
         (company-dabbrev company-capf company-keywords :with company-yasnippet)))
  ;; Enable checker by project
  (cond
   ((distopico:locate-parent-file distopico:eslint-regexp)
    (flycheck-select-checker 'javascript-eslint))))

;; Hooks
(add-hook 'web-jsx-mode-hook 'distopico:web-jsx-mode-hook)


(provide 'conf-jsx)
