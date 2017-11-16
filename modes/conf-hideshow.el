;;; Code:
(require 'hideshowvis)
(hideshowvis-symbols)

;; Enable default hideShow
(hs-minor-mode 1)
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))

;; Hideshowvis mode
(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)
(dolist (hook (list 'emacs-lisp-mode-hook
                    'lisp-mode-hook
                    'ruby-mode-hook
                    'perl-mode-hook
                    'php-mode-hook
                    'html-mode-hook
                    ;;'web-mode-hook
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

;; (when (fboundp 'define-fringe-bitmap)
;;(define-fringe-bitmap 'hideshowvis-hideable-marker [0 0 254 124 56 16 0 0])
;;(define-fringe-bitmap 'hs-marker [0 32 48 56 60 56 48 32]))

;; Faces
(custom-set-faces
 '(hs-fringe-face ((t (:foreground "#afeeee" :box (:line-width 2 :color "grey75" :style released-button)))))
 '(hs-face ((t (:background "#444" :box t))))
 '(hideshowvis-hidable-face ((t (:foreground "#2f4f4f")))))


;; Functions
(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(provide 'conf-hideshow)
