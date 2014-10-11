(require 'highlight-symbol)
(require 'auto-highlight-symbol)

(setq highlight-symbol-on-navigation-p t)
(setq highlight-symbol-foreground-color "white")
(setq highlight-symbol-colors
  '("DarkCyan" "DeepPink" "MediumPurple1"
    "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab"))


;; auto-highlight-symbol-config

(define-key auto-highlight-symbol-mode-map (kbd "M-s-<left>" ) 'ahs-backward-definition )
(define-key auto-highlight-symbol-mode-map (kbd "M-s-<right>" ) 'ahs-forward-definition )

(eval-when-compile
(require 'cl)
(unless (fboundp 'auto-complete-mode)
(defun auto-complete-mode(arg))))

(defconst auto-highlight-symbol-config-vers "$Id: auto-highlight-symbol-config.el,v 70:542ae42370e4 2010-11-04 12:52 +0900 arch320 $"
"auto-highlight-symbol-config version")

(defvar ahs-ac-active-flag nil)
(make-variable-buffer-local 'ahs-ac-active-flag)

(defun ahs-avoid-auto-complete-menu ()
"Avoid auto-complete-mode menu for protect overlay"
(when (featurep 'auto-complete)
(setq ahs-ac-active-flag
(or ahs-ac-active-flag
(assoc-default 'auto-complete-mode (buffer-local-variables))))
(when ahs-ac-active-flag
(auto-complete-mode 0))))

(defun ahs-recover-auto-complete ()
"Recover auto-complete-mode"
(when (and (featurep 'auto-complete)
ahs-ac-active-flag)
(auto-complete-mode t)
(setq ahs-ac-active-flag nil)))

(custom-set-variables
 '(global-auto-highlight-symbol-mode nil))


(provide 'conf-highlight-symbol)
