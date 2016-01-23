;;; Code:
(require 'highlight-parentheses)
(require 'highlight-indentation)
(require 'highlight-symbol)

;; Highlight parentheses
;; TODO: Check if highlight-sexps is useful
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
       (naquadah-get-colors 'gradient-11)))

;; show-paren-mode
(setq show-paren-delay 0
      blink-matching-paren t
      show-paren-style 'mixed)
(set-face-background 'show-paren-match "cyan")
(set-face-foreground 'show-paren-match "#000000")

;; Highlight symbol
(setq highlight-symbol-on-navigation-p t
      highlight-symbol-idle-delay 1
      highlight-symbol-foreground-color "white"
      highlight-symbol-colors
      '("DarkCyan" "DeepPink" "MediumPurple1"
        "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab"))

;; Highlight indentation
(set-face-background 'highlight-indentation-face "gray5")
(set-face-background 'highlight-indentation-current-column-face "#2f4f4f")

;; Functions
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))

(defun distopico:highlight-symbol-toggle ()
  "Toggle automatic and manual symbol highlighting for Emacs"
  (interactive)
  (highlight-symbol-mode)
  (highlight-symbol-nav-mode))

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


;; Hooks
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
;; Enable
(global-highlight-parentheses-mode t)

(provide 'conf-highlight-code)
