;;; Code:
(require 'js2-mode)
(require 'js2-refactor)
(require 'js2-imenu-extras)
(require 'ac-js2)
(require 'web-beautify)
(require 'company-tern)

(js2r-add-keybindings-with-prefix "C-c C-j")

(setq-default js2-skip-preprocessor-directives t
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
              js2-strict-inconsistent-return-warning nil)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'magic-mode-alist '(".+node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("javascript" . js2-mode))

(custom-set-faces
 '(js2-highlight-vars-face ((t (:background "royal blue" :foreground "white")))))

;; Beautify with js
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c C-b f") 'web-beautify-js))

;; Enabled autocomplete
;; (setq ac-js2-evaluate-calls t)
(add-to-list 'company-backends 'company-tern)

;; Functions
(defun distopico:js2-mode-hook ()
  (js2-imenu-extras-mode t)
  (js2-imenu-extras-setup)
  (rainbow-delimiters-mode t)
  (tern-mode t)
  (ac-js2-mode t))

;; Hooks
(add-hook 'js2-mode-hook 'distopico:js2-mode-hook)

(provide 'conf-javascript)
