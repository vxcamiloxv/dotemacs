(require 'highlight-parentheses)

(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)

; Use naquadah colors
(setq hl-paren-colors
      (list
       (naquadah-get-colors 'gradient-1)
       (naquadah-get-colors 'gradient-2)
       (naquadah-get-colors 'gradient-3)
       (naquadah-get-colors 'gradient-4)
       (naquadah-get-colors 'gradient-5)
       (naquadah-get-colors 'gradient-6)
       (naquadah-get-colors 'gradient-7)
       (naquadah-get-colors 'gradient-8)
       (naquadah-get-colors 'gradient-9)
       (naquadah-get-colors 'gradient-10)
       (naquadah-get-colors 'gradient-11)
       ))

;; Make the parenthesis' matched by hl-paren-face bold.
;;
;; Not sure if I like this or not!  It looks nice but moves around
;; the text a bit.
; (set-face-attribute 'hl-paren-face nil :bold t)


;(add-hook 'highlight-parentheses-mode-hook
;          '(lambda ()
;             (setq autopair-handle-action-fns
;                   (append
;					(if autopair-handle-action-fns
;						autopair-handle-action-fns
;					  '(autopair-default-handle-action))
;					'((lambda (action pair pos-before)
;						(hl-paren-color-update)))))))

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; show-paren-mode
(setq show-paren-delay 0)
(setq blink-matching-paren t)
;(setq electric-pair-mode 1)
(set-face-background 'show-paren-match "cyan")
(set-face-foreground 'show-paren-match "#000000")
(set-face-attribute 'show-paren-match nil )

(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
    echo area. Has no effect if the character before point is not of
    the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))
