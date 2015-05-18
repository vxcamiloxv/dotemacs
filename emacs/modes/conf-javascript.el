;;; Code:

(require 'js2-mode)
(require 'js2-refactor)
(require 'js2-imenu-extras)
(require 'ac-js2)

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
              js2-strict-inconsistent-return-warning nil
              ;;js2-global-externs '("jQuery" "$")
              )

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'magic-mode-alist '(".+node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("javascript" . js2-mode))

(custom-set-faces
 '(js2-highlight-vars-face ((t (:background "royal blue" :foreground "white")))))

;; js2-mode hook
(add-hook 'js2-mode-hook
          '(lambda ()
             (js2-imenu-extras-mode)
             (js2-imenu-extras-setup)
             (rainbow-delimiters-mode t)
             ;;(rainbow-identifiers-mode t)
             ;; Todo Highlighting
             (todo-highlight)
             ))

;; Enabled ac-js2
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq ac-js2-evaluate-calls t)

(provide 'conf-javascript)
