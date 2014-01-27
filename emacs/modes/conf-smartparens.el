;;;;;;;;;
;; global
(require 'smartparens)
(smartparens-global-mode t)
(require 'smartparens-config)

;; highlights matching pairs
(show-smartparens-global-mode t)

(add-hook 'hy-mode 'smartparens-mode)
;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management

(sp-use-smartparens-bindings)


;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

(setq sp-show-enclosing-pair-commands t)
(setq sp-highlight-current-sexp t)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; php-mode
(sp-with-modes '(html-mode web-mode php-mode)
  (sp-local-pair "?" "php  ?"))

(sp-with-modes '(html-mode web-mode django-mode)
  (sp-local-pair "%" "%"))

(sp-local-tag '(php-mode web-mode) "<" "<_>" "</_>" :transform 'sp-match-sgml-tags)


;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-(")
  (sp-local-pair "'" nil :actions nil))

;; Global
(sp-pair "<#" "#>")
(sp-pair "<" ">")

;; Color

(custom-set-faces
    '(sp-show-pair-match-face ((t (
                    :inherit nil
                    :background nil
                    :foreground "#00ffff"))))

    '(sp-pair-overlay-face ((t (
                    :inherit nil
                    :background nil
                    :foreground "#7cfc00"))))

    '(sp-wrap-overlay-face ((t (
                    :inherit nil
                    :background nil
                    :foreground "#ff4500"))))

    '(sp-wrap-tag-overlay-face ((t (
                    :inherit nil
                    :background nil
                    :foreground "#ff1493"))))

    '(sp-show-pair-enclosing ((t (
                    :inherit nil
                    :foreground "#000000"
                    :background "#ff6347"))))
)

(provide 'conf-smartparens)
