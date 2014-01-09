(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


(setq web-mode-engines-alist
      '(("django" . "\\.html$")))

(defun cwebber-web-mode-customizations ()
  "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-indent-style 2)  
    (setq web-mode-style-padding 1)
    (setq web-mode-script-padding 1)
    (setq web-mode-block-padding 0)
    (setq web-mode-comment-style 2)
)

(add-hook 'web-mode-hook 'cwebber-web-mode-customizations)

;; More tango-y colors
(set-face-attribute 'web-mode-html-tag-face nil
                    :foreground "#729fcf")
(set-face-attribute 'web-mode-html-tag-bracket-face nil
                    :foreground "#888a85")
                    
; zencoding
(require 'emmet-mode)
(setq emmet-indentation 2)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(define-key emmet-mode-keymap (kbd "C-j") nil)
(define-key emmet-mode-keymap (kbd "<C-return>") nil)
(define-key emmet-mode-keymap (kbd "C-c i") 'emmet-expand-line)

; js2-mode
(require 'js2-mode)
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-hook 'css-mode-hook '(lambda ()
                            (define-key css-mode-map (kbd "M-i") 'helm-css-scss))) 
                            
;Autocomplete
(add-to-list 'ac-modes 'web-mode)
