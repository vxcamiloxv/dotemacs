(provide 'conf-popwin)

(add-to-list 'load-path "~/.emacs.d/modes/popwin-el")
(add-to-list 'load-path "~/.emacs.d/modes/popwin-el/misc")
(require 'popwin)
(popwin-mode 1)

;; M-x anything
(setq anything-samewindow nil)
(push '("*anything*" :height 20) popwin:special-display-config)

;; M-x dired-jump-other-window
(push '(dired-mode :position top) popwin:special-display-config)

;; M-!
(push "*Shell Command Output*" popwin:special-display-config)

;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)

;; slime
(push "*slime-apropos*" popwin:special-display-config)
(push "*slime-macroexpansion*" popwin:special-display-config)
(push "*slime-description*" popwin:special-display-config)
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
(push "*slime-xref*" popwin:special-display-config)
(push '(sldb-mode :stick t) popwin:special-display-config)
(push 'slime-repl-mode popwin:special-display-config)
(push 'slime-connection-list-mode popwin:special-display-config)

;; vc
(push "*vc-diff*" popwin:special-display-config)
(push "*vc-change-log*" popwin:special-display-config)

;; Django
(push '("^*Django:/.*$" :width 0.3 :position right) popwin:special-display-config)

;; undo-tree
(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)

;;ibuffer
(push "*Ibuffer*" popwin:special-display-config)

;; Kill ring
(require 'popwin-browse-kill-ring)
(push "*Kill Ring*" popwin:special-display-config)

;;Terminal
(require 'popwin-term)
(push '(term-mode :position :top :height 10 :stick t) popwin:special-display-config)

;; Backtrace
(push '("*Backtrace*" :height 0.3 ) popwin:special-display-config)

;; Messages
(push '("*Messages*" :height 0.3 ) popwin:special-display-config)

;; Compile-Log
(push '("*Compile-Log*" :height 0.3 ) popwin:special-display-config)

;; Direx
(push '(direx:direx-mode :position left :width 35 :dedicated t)
      popwin:special-display-config)

;; Flycheck
(push '(flycheck-error-list-mode :stick t) popwin:special-display-config)   
                   
;;Others
(push '("collected.org" :position top :height 15) popwin:special-display-config)
(push '("*grep*" :position bottom :height 20 :stick t) popwin:special-display-config)
(push '("*imenu-tree*" :position left :width 50 :stick t) popwin:special-display-config)
 
 
;; popwin settings
(setq popwin:special-display-config
      '(("*Help*" :height 30 :stick t)
        ("*Completions*" :noselect t)
        ("*compilation*" :noselect t)
        ("*Messages*")
        ("*Occur*" :height 0.3  t)
        ("\\*Slime Description.*" :noselect t :regexp t :height 30)
        ("*magit-commit*" :noselect t :height 0.3 :width 80 :stick t)
        ("*magit-diff*" :noselect t :height 0.3 :width 80)
        ("*magit-edit-log*" :noselect t :height 0.2 :width 80)
        ("*magit-process*" :noselect t :height 0.2 :width 80)
        ("\\*Slime Inspector.*" :regexp t :height 30)
        ("*Ido Completions*" :noselect t :height 30)
        ;;("*eshell*" :height 20)
        ("\\*ansi-term\\*.*" :regexp t :height 30)
        ("*shell*" :height 30)
        (".*overtone.log" :regexp t :height 30)
        ("*gists*" :height 30)
        ("*sldb.*":regexp t :height 30)
        ("*Gofmt Errors*" :noselect t)
        ("\\*godoc*" :regexp t :height 30)
        ;("*Shell Command Output*" :noselect t)
        ("*nrepl-error*" :height 20 :stick t)
        ("*nrepl-doc*" :height 20 :stick t)
        ("*nrepl-src*" :height 20 :stick t)
        ("*Kill Ring*" :height 30)
        ("*project-status*" :noselect t)
        ("*Compile-Log" :height 20 :stick t)
        ("*pytest*" :noselect t)
        ("*Python*" :stick t)
        ("*jedi:doc*" :noselect t)
        )
)
          
(when (require 'popwin nil t)
  (setq anything-samewindow nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (push '("anything" :regexp t :height 0.5) popwin:special-display-config)
  (push '("helm" :regexp t :height 0.3) popwin:special-display-config)
  (push '("*magit-edit-log*" :height 0.3) popwin:special-display-config)
  (push '("magit" :regexp t :height 0.3) popwin:special-display-config)
  (push '("*Completions*" :height 0.4) popwin:special-display-config)
  (push '("*compilation*" :height 0.4 :noselect t :stick t) popwin:special-display-config)
  (push '("*Help*" :height 0.3 :stick t) popwin:special-display-config)
  (push '("*quickrun*" :height 0.3 :stick t) popwin:special-display-config)
)
