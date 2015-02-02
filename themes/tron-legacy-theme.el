;;; tron-legacy-theme.el -- Tron Color Theme for Emacs.
;;
;; MIT License Copyright (c) 2012 Ivan Marcin <ivan at ivanmarcin dot com>
;;
;; All patches welcome

;;; Commentary:
;; --------------
;; This porting makes tron no longer rely on color-theme package,
;; since Emacs has it's theme mechanism from Emacs 24.

;; How to use:
;; copy the theme file to your themes folder or create one in your home directory.
;; edit init.el and add this 2 lines:
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-theme `tron t)
;;
;; or
;; load it manually by pressing
;; M-x load-theme, then choose tron, it should work
;; Or, simple use (load-theme 'tron t) to enable the theme from start.
;;
;; Modified By Distopico <distopico@riseup.net>
;;
;;; tron-theme

;;; Code:
(deftheme tron-legacy
  "Based on Color theme by Ivan Marcin,  created 2012-08-25")

(custom-theme-set-faces
 'tron-legacy
 '(default ((t (:background "black" :foreground "cadet blue" ))))
 '(bold ((t (:bold t))))
 '(bold-italic ((t (:bold t))))
 '(cursor ((t (:background "cyan"))))
 '(border-glyph ((t (nil))))
 '(fringe ((t (:background "CadetBlue"))))
 '(mode-line ((t (:foreground "#072d40" :background "#99bac7"))))
 '(region ((t (:background "steel blue"))))
 '(font-lock-builtin-face ((t (:foreground "#559ff1"))))
 '(font-lock-comment-face ((t (:foreground "#575b5b"))))
 '(font-lock-function-name-face ((t (:foreground "#ec9346"))))
 '(font-lock-keyword-face ((t (:foreground "#a4cee5"))))
 '(font-lock-string-face ((t (:foreground "#e8b778"))))
 '(font-lock-type-face ((t (:foreground"#74abbe"))))
 '(font-lock-constant-face ((t (:foreground "#eeedec"))))
 '(font-lock-variable-name-face ((t (:foreground "#9ebbc2"))))
 '(minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
 '(font-lock-warning-face ((t (:foreground "red" :bold t))))
)

(provide-theme 'tron-legacy)

;;eof
