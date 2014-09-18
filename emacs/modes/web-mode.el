(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode))

(setq web-mode-engines-alist
      ' ;(("django" . "\\.html\\'")
        ;("php" . "\\.phtml\\'")
        ;("php" . "\\.inc\\'")
        (("blade" . "\\.blade\\."))
      )

(defun web-mode-customizations ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (setq web-mode-comment-style 2)

  ;; Disabled smartparens in web-mode
  (setq smartparens-mode nil)
)

(add-hook 'web-mode-hook 'web-mode-customizations)

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)
;;(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-heredoc-fontification t)

;;(local-set-key (kbd "RET") 'newline-and-indent)


;; More tango-y colors
(custom-set-faces
 '(web-mode-html-tag-face
   ((t (:foreground "#729fcf"))))
 '(web-mode-html-tag-bracket-face
   ((t (:foreground "#FFE84B"))))
 '(web-mode-current-element-highlight-face
   ((t (:foreground "#FF8A4B"))))
 '(web-mode-current-element-highlight-face
   ((t (:background "#000000"
		:foreground "#FF8A4B"))))
)

;; zencoding
(require 'emmet-mode)
(require 'ac-emmet)
(setq emmet-indentation 2)

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(add-hook 'web-mode-hook 'ac-emmet-html-setup)
(add-hook 'css-mode-hook 'ac-emmet-css-setup)

(define-key emmet-mode-keymap (kbd "C-j") nil)
(define-key emmet-mode-keymap (kbd "<C-return>") nil)
(define-key emmet-mode-keymap (kbd "C-c e") 'emmet-expand-line)

;; js2-mode
(require 'js2-mode)
(require 'js2-refactor)

(js2r-add-keybindings-with-prefix "C-c C-j")

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;; (add-to-list 'magic-mode-alist '(".+node" . js3-mode))
(add-to-list 'interpreter-mode-alist '("javascript" . js2-mode))


(add-hook 'js2-mode-hook
             (setq js2-skip-preprocessor-directives t
                   js2-auto-indent-p t
                   js2-enter-indents-newline t
                   js2-indent-on-enter-key t
                   js2-missing-semi-one-line-override t
                   js2-include-node-externs t
                   js2-include-browser-externs t)
)

;; skewer-mode
(when (require 'skewer-mode nil 'noerror)
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

;; css Helm 
(add-hook 'css-mode-hook '(lambda ()
                            (define-key css-mode-map (kbd "M-i") 'helm-css-scss)))

;; Autocomplete
(add-hook 'web-mode-hook 'auto-complete-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'css-mode)


(provide 'web-mode)
