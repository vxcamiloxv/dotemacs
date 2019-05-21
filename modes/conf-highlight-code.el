;;; Code:
(require 'highlight-parentheses)
(require 'highlight-indent-guides)
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

;; Highlight symbol
(setq highlight-symbol-on-navigation-p t
      highlight-symbol-idle-delay 2
      highlight-symbol-foreground-color "white"
      highlight-symbol-colors
      '("DarkCyan" "DeepPink" "MediumPurple1"
        "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab"))

;; Highlight indentation
(setq highlight-indent-guides-method 'character)

;; Functions
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))

(defun distopico:highlight-symbol-toggle ()
  "Toggle automatic and manual symbol highlighting for Emacs."
  (interactive)
  (highlight-symbol-mode)
  (highlight-symbol-nav-mode))

;; Enable custom global mode
(global-highlight-parentheses-mode t)

(provide 'conf-highlight-code)
