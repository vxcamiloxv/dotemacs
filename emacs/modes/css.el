;; ---------
;; CSS Mode
;; ---------

(require 'css-mode)
(require 'rainbow-mode)

(autoload 'css-mode "css-mode" "CSS editing mode" t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

(add-hook 'css-mode-hook
  (lambda ()
    (rainbow-mode 1)))

