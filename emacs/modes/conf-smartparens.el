;;;;;;;;;
;; global
(require 'smartparens-config)
(smartparens-global-mode t)
(require 'smartparens-config)

;; highlights matching pairs
(show-smartparens-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management



;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; html-mode
(sp-with-modes '(html-mode web-mode php-mode)
  (sp-local-pair "?" "php  ?"))

(sp-local-tag '(php-mode web-mode) "<" "<_>" "</_>" :transform 'sp-match-sgml-tags)

;;; lisp modes
;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-(")
  (sp-local-pair "'" nil :actions nil))

;; Global 
(sp-pair "<#" "#>")
(sp-pair "<" ">")

;; Color
(set-face-attribute 'sp-pair-overlay-face nil
                    :inherit nil
                    :background  nil
                    :foreground "#7cfc00")
                    
(set-face-attribute 'sp-wrap-overlay-face nil
                    :inherit nil
                    :background  nil
                    :foreground "#ff4500")
(set-face-attribute 'sp-wrap-tag-overlay-face nil
                    :inherit nil
                    :background nil
                    :foreground "#ff1493")

(set-face-attribute 'sp-show-pair-match-face nil 
                    :inherit nil
                    :background nil
                    :foreground "#00ffff")     
                                                   
(set-face-attribute 'sp-show-pair-enclosing nil 
                    :inherit nil
                    :foreground "#000000"
                    :background "#ff6347")   
                                        
(provide 'conf-smartparens)
