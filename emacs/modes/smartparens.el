(require 'smartparens)
(require 'smartparens-config)

; Use default keygindings
(sp-use-smartparens-bindings)

;; highlights matching pairs
(show-smartparens-global-mode t)

;; Less nasty face for pair-overlay

(set-face-attribute 'sp-pair-overlay-face nil :background "#2d3233")

;;; Lots borrwowed from https://github.com/Fuco1/smartparens/wiki/Example-configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(add-hook 'hy-mode 'smartparens-mode)
