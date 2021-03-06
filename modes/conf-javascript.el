;;; Code:
(require 'js2-mode)
(require 'js2-refactor)
(require 'js2-imenu-extras)
(require 'xref-js2)
(require 'web-beautify)
(require 'rainbow-delimiters)
(require 'company-tern)
(require 'flycheck)

;; Control
(defconst distopico:jshint-regexp
  (concat "\\`" (regexp-quote ".jshintrc") "\\'"))
(defconst distopico:eslint-regexp
  (concat "\\`" (regexp-quote ".eslintrc") "\\(\\.\\(js\\|ya?ml\\|json\\)\\)?\\'"))

;; Base config
;; TODO: some cool stuff: https://github.com/foretagsplatsen/emacs-js
(setq js-indent-level 4
      js-indent-first-init "dynamic"
      ;; js-indent-align-list-continuation nil ;; TODO: align parameter to next line
      js2-basic-offset 4
      js2-skip-preprocessor-directives t
      js2-include-node-externs t
      js2-include-browser-externs t
      js2-highlight-level 3
      js2-move-point-on-right-click t
      ;; Let js2-mode warn some errors
      js2-mode-show-parse-errors t
      js2-mode-show-strict-warnings t
      js2-strict-trailing-comma-warning nil
      js2-strict-missing-semi-warning nil
      ;; js2-bounce-indent-p t TODO: works for better indentation?
      js2-strict-inconsistent-return-warning nil)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("javascript" . js2-mode))

(custom-set-faces
 '(js2-highlight-vars-face ((t (:background "royal blue" :foreground "white")))))

;; unbind keys
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

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
(defun distopico:js2-mode-hook ()
  "The js2-mode hook."
  (js2-imenu-extras-mode t)
  (js2-imenu-extras-setup)
  (tern-mode t)
  (js2-refactor-mode t)
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
  (distopico:js-common-setup))

(defun distopico:js2-company-setup ()
  "Setup JavaScript company/code completion back-ends.
All the inherits `company-yasnippet' back-end."
  (distopico:backend-with-yas
   '(company-semantic company-files
     (company-dabbrev-code company-gtags company-tern)
     (company-capf company-dabbrev company-keywords))))

;;;###autoload
(defun distopico:js-common-setup ()
  "Common setup to JS and JSX."
  ;; Add company backend for js
  (setq-local company-backends (distopico:js2-company-setup))
  ;; Add node_modules to exec path
  (distopico:add-node-modules-path)
  ;; Default checker
  (flycheck-select-checker 'javascript-jshint)
  ;; Enable checker by project
  (when (distopico:locate-parent-file distopico:eslint-regexp)
    (flycheck-select-checker 'javascript-eslint)))

;;;###autoload
(defun distopico:lint-from-node-modules ()
  "Set executable lint by project node_modules."
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
         (jshint (and root
                      (expand-file-name "node_modules/jshint/bin/jshint" root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))
    (when (and jshint (file-executable-p eslint))
      (setq-local flycheck-javascript-jshint-executable jshint))))

;;;###autoload
(defun distopico:add-node-modules-path ()
  "Add `node_modules' in current project to exec path."
  (interactive)
  ;; TODO: make async
  (let ((node-path (shell-command-to-string "npm bin")))
    (when node-path
      (progn
        (make-local-variable 'exec-path)
        (add-to-list 'exec-path (string-trim node-path))
        (message "added node_modules to exec-path")))))

;; Hooks
(add-hook 'js2-mode-hook #'distopico:js2-mode-hook)

(provide 'conf-javascript)
