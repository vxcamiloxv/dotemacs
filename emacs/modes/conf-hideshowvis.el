;(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
;(autoload 'hideshowvis-symbols "hideshowvis" "Highlight foldable regions")

(require 'hideshowvis)
(hideshowvis-symbols)

(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)

  (add-to-list 'hs-special-modes-alist
         '(ruby-mode
         "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
         (lambda (arg) (ruby-end-of-block)) nil))

  (dolist (hook (list 'emacs-lisp-mode-hook
                      'lisp-mode-hook
                      'ruby-mode-hook
                      'perl-mode-hook
                      'php-mode-hook
                      'html-mode-hook
                      ;'web-mode-hook
                      'sgml-mode-hook
                      'multi-web-mode-hook
                      'python-mode-hook
                      'lua-mode-hook
                      'c-mode-hook
                      'java-mode-hook
                      'js-mode-hook
        		      'js2-mode-hook
                      'js3-mode-hook
                      'css-mode-hook
                      'c++-mode-hook))
    (add-hook hook 'hideshowvis-enable))

;(define-fringe-bitmap 'hideshowvis-hideable-marker [0 0 254 124 56 16 0 0])
;(define-fringe-bitmap 'hs-marker [0 32 48 56 60 56 48 32])

(custom-set-faces
    '(hs-fringe-face ((t (:foreground "#afeeee" :box (:line-width 2 :color "grey75" :style released-button)))))
    '(hs-face ((t (:background "#444" :box t))))
    '(hideshowvis-hidable-face ((t (:foreground "#2f4f4f"))))
)


(provide 'conf-hideshowvis)
