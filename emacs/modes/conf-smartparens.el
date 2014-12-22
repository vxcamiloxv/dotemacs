;;;;;;;;;
;; global
(require 'smartparens)
(smartparens-global-mode t)
(require 'smartparens-config)

;; highlights matching pairs
(show-smartparens-global-mode t)

     
;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management

(sp-use-smartparens-bindings)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

(setq sp-show-enclosing-pair-commands t)
(setq sp-highlight-current-sexp t)


(defun sp-point-after-word-p (id action context)
  "Return t if point is after a word, nil otherwise.  This
predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (save-excursion
      (backward-char 1)
      (looking-back "\\sw\\|\\s_"))))


;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; html-mode
(sp-with-modes '(web-mode html-mode sgml-mode)
  (sp-local-pair "<" ">")
)

;;; php-mode
;(sp-with-modes '(html-mode web-mode php-mode)
;  (sp-local-pair "?" "php  ?"))

;; (sp-with-modes '(html-mode web-mode django-mode)
;;   (sp-local-pair "%" "%"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

;; Global
(sp-pair "<#" "#>")

;; Color

(custom-set-faces
    '(sp-show-pair-match-face ((t (
                    :inherit nil
                    :background "#00ffff"
                    :foreground "#000000"))))

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
