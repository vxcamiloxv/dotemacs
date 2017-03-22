;;; Code:
(require 'js2-mode)
(require 'js2-refactor)
(require 'js2-imenu-extras)
(require 'ac-js2)
(require 'xref-js2)
(require 'web-beautify)
(require 'company-tern)


;; Control
(defconst distopico:jshint-regexp
  (concat "\\`" (regexp-quote ".jshintrc") "\\'"))
(defconst distopico:eslint-regexp
  (concat "\\`" (regexp-quote ".eslintrc") "\\(\\.\\(js\\|ya?ml\\|json\\)\\)?\\'"))

;; Base config
(setq js-indent-level 4
      js2-basic-offset 4
      js2-skip-preprocessor-directives t
      js2-include-node-externs t
      js2-include-browser-externs t
      js2-highlight-level 3
      ;;js2-move-point-on-right-click nil
      ;; Let flycheck parse errors
      ;;js2-idle-timer-delay 0.1
      js2-mode-show-parse-errors t
      js2-mode-show-strict-warnings t
      js2-strict-trailing-comma-warning t
      js2-strict-missing-semi-warning nil
      js2-strict-inconsistent-return-warning nil
      ac-js2-evaluate-calls t)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'magic-mode-alist '(".+node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("javascript" . js2-mode))

(custom-set-faces
 '(js2-highlight-vars-face ((t (:background "royal blue" :foreground "white")))))

;; unbind keys
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)
(define-key ac-js2-mode-map (kbd "M-.") nil)
(define-key ac-js2-mode-map (kbd "M-,") nil)

;; Custom keys
(js2r-add-keybindings-with-prefix "C-c C-m")
(define-key js2-mode-map (kbd "M-.") 'xref-find-definitions)
(define-key js2-mode-map (kbd "M-,") 'xref-pop-marker-stack)
(define-key js2-mode-map (kbd "C-c ci") 'js-doc-insert-function-doc)
(define-key js2-mode-map (kbd "C-c cf") 'js-doc-insert-file-doc)
(define-key js2-mode-map (kbd "C-c cs") 'js-doc-insert-function-doc-snippet)
(define-key js2-mode-map "@" 'js-doc-insert-tag)

;; Beautify with js
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c C-b f") 'web-beautify-js))

;; Functions
(defun distopico:locate-dominating-file (regexp)
  "Locate a directory with a file matching REGEXP."
  (locate-dominating-file
   default-directory
   (lambda (directory)
     (> (length (directory-files directory nil regexp t)) 0))))

(defun distopico:js2-mode-hook ()
  "The js2-mode hook."
  (js2-imenu-extras-mode t)
  (js2-imenu-extras-setup)
  (rainbow-delimiters-mode t)
  (tern-mode t)
  (ac-js2-mode t)
  (js2-refactor-mode t)
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
  ;; Default checker
  (flycheck-select-checker 'javascript-jshint)
  ;; Add company backend for js
  (make-local-variable 'company-backends)
  (setq company-backends
        '(company-bbdb
          company-nxml company-css
          company-semantic company-files
          (company-dabbrev-code company-gtags company-etags company-keywords company-tern)
          company-oddmuse company-dabbrev company-capf))
  ;; Enable checker by project
  (cond
   ((distopico:locate-dominating-file distopico:eslint-regexp)
    (flycheck-select-checker 'javascript-eslint))))

;; Hooks
(add-hook 'js2-mode-hook #'distopico:js2-mode-hook)

(provide 'conf-javascript)
