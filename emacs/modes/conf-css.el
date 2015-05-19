;;; Code:

;; ---------
;; CSS Mode
;; ---------

(require 'css-mode)
(require 'rainbow-mode)
(require 'helm-css-scss)
(require 'skewer-less)
(require 'auto-complete)

(autoload 'css-mode "css-mode" "CSS editing mode" t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))


;; Setings helm-css
(setq helm-css-scss-insert-close-comment-depth 2)
(setq helm-css-scss-split-with-multiple-windows nil)
(setq helm-css-scss-split-direction 'split-window-vertically)

;; (define-key helm-css-scss-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)



;; Hook
(dolist ($hook '(css-mode-hook less-css-mode-hook))
  (add-hook
   $hook (lambda ()
           (rainbow-mode t)
           (rainbow-delimiters-mode t)
           ;;(rainbow-identifiers-mode t)
           ;;(set (make-local-variable 'ac-sources) (append ac-sources '(ac-source-emmet-css-snippets))) necesary?
           (local-set-key (kbd "C-c m") 'helm-css-scss)
           (local-set-key (kbd "C-c i") 'helm-css-scss-from-isearch)
           (local-set-key (kbd "M-i") 'helm-css-scss-back-to-last-point))))

(add-hook 'less-css-mode-hook
          '(lambda ()
             (skewer-less-mode)))


(provide 'conf-css)
