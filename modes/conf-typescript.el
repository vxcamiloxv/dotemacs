(require 'tide)
(require 'web-mode)

(defun distopico:typescript-mode-hook ()
  ;; Add node_modules to exec path
  (distopico:add-node-modules-path)
  (tide-setup)
  ;;(flycheck-mode t)
  (eldoc-mode t)
  ;;(company-mode t)
  ;; Add company backend for js
  (set (make-local-variable 'company-backends)
       '(company-bbdb
         company-nxml company-css
         company-semantic company-files
         (company-dabbrev-code company-gtags company-etags company-keywords company-tern :with company-yasnippet)
         (company-dabbrev company-capf company-keywords :with company-yasnippet)))
  (tide-hl-identifier-mode t))

(defun distopico:web-mode-hook ()
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (distopico:typescript-mode-hook)))

;; TSX
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
(add-hook 'web-mode-hook #'distopico:web-mode-hook)

(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'distopico:typescript-mode-hook)
