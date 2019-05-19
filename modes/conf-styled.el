(require 'multi-web-mode)

;; (define-hostmode poly-styled-hostmode
;;   :mode 'js2-mode)

;; (define-innermode poly-styled-innermode
;;   :mode 'scss-mode
;;   :head-mode 'host
;;   :tail-mode 'host
;;   :head-matcher "`\n"
;;   :tail-matcher "`")

;; (define-innermode poly-js-innermode
;;   :mode 'js-mode
;;   :head-mode 'scss-mode
;;   :tail-mode 'scss-mode
;;   :head-matcher "\${"
;;   :tail-matcher "}$")

;; (add-to-list 'auto-mode-alist '("\\.styles.js\\'" . poly-styled-mode))

;; ;;;###autoload  (autoload 'poly-styled-mode "poly-styled")
;; (define-polymode poly-styled-mode
;;   :hostmode 'poly-styled-hostmode
;;   :innermodes '(poly-styled-innermode
;;                 ;;poly-js-innermode
;;                 ))

(require 'mmm-auto)
(mmm-add-group
 'js-css
 '((css
    :submode scss-mode
    :face mmm-code-submode-face
    :front "[a-zA-Z0-9_-]`\\|`\n"
    :back "`\\,\\|`\\;")))

(setq mmm-global-mode 'web-mode)
(mmm-add-mode-ext-class 'web-mode "\\.styles.js\\'" 'js-css)

(provide 'conf-styled)
