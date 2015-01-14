;;; Code:

(require 'popwin)
(popwin-mode t)
(global-set-key (kbd "C-x p") popwin:keymap)

;; M-x anything
(setq anything-samewindow nil)
(push '("*anything*" :height 0.5) popwin:special-display-config)

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
(push '("Django:" :regexp t :width 0.3 :position right) popwin:special-display-config)

;; undo-tree
(push '("*undo-tree*" :width 0.3 :position right) popwin:special-display-config)

;;ibuffer
(push "*Ibuffer*" popwin:special-display-config)

;; Kill ring
(require 'popwin-browse-kill-ring)
(push "*Kill Ring*" popwin:special-display-config)

;;Terminal
(require 'popwin-term)
(push '(term-mode :position :bottom :height 10 :stick t) popwin:special-display-config)

;; Debug
(push '("*Backtrace*" :height 0.3 ) popwin:special-display-config)
(push '("*Messages*" :height 0.3 ) popwin:special-display-config)
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
(push '("helm" :regexp t :height 0.3) popwin:special-display-config)
(push '("*magit-edit-log*" :height 0.3) popwin:special-display-config)
(push '("magit" :regexp t :height 0.3) popwin:special-display-config)
(push '("*Completions*" :height 0.4) popwin:special-display-config)
(push '("*compilation*" :height 0.4 :noselect t :stick t) popwin:special-display-config)
(push '("*quickrun*" :height 0.3 :stick t) popwin:special-display-config)

;; popwin settings
(setq popwin:special-display-config
      '(("*Help*" :height 0.4 :stick t)
        ("*Occur*" :position bottom :height 0.3)
        ("\\*Slime Description.*" :noselect t :regexp t :height 30)
        ("*magit-commit*" :noselect t :height 0.3 :width 80 :stick t)
        ("*magit-diff*" :noselect t :height 0.3 :width 80)
        ("*magit-edit-log*" :noselect t :height 0.2 :width 80)
        ("*magit-process*" :noselect t :height 0.2 :width 80)
        ("\\*Slime Inspector.*" :regexp t :height 30)
        ("*Ido Completions*" :noselect t :height 0.3)
        (".*overtone.log" :regexp t :height 30)
        ("*gists*" :height 0.3)
        ("*sldb.*":regexp t :height 30)
        ("*Gofmt Errors*" :noselect t)
        ("\\*godoc*" :regexp t :height 30)
        ("*nrepl-error*" :height 20 :stick t)
        ("*nrepl-doc*" :height 20 :stick t)
        ("*nrepl-src*" :height 20 :stick t)
        ("*Kill Ring*" :height 30)
        ("*project-status*" :noselect t)
        ("*Compile-Log" :height 20 :stick t)
        ("*pytest*" :noselect t)
        ("*Python*" :stick t)
        ("*jedi:doc*" :noselect t)
        ("*shell*" :height 0.3)
        ("\\*ansi-term.*\\*" :regexp t :height 0.3)
        ("\\*terminal.*\\*" :regexp t :height 0.3)
        (diary-fancy-display-mode :position left :width 50 :stick nil)
        (diary-mode :position bottom :height 15 :stick t)
        (calendar-mode :position bottom :height 15 :stick nil)
        (org-agenda-mode :position bottom :height 15 :stick t)
        ("*Org Agenda.*\\*" :regexp t :position bottom :height 15 :stick t)
        )
      )

;; Fix neotree
(when neo-persist-show
  (add-hook 'popwin:before-popup-hook
            (lambda () (setq neo-persist-show nil)))
  (add-hook 'popwin:after-popup-hook
            (lambda () (setq neo-persist-show t))))

(defun live-display-ansi ()
  (interactive)
  (popwin:display-buffer "*ansi-term*"))

  (defun popwin-term:ansi-term ()
  (interactive)
  (popwin:display-buffer-1
   (or (get-buffer "*ansi-term*")
       (save-window-excursion
         (interactive)
         (ansi-term "/bin/bash")))
   :default-config-keywords '(:position :bottom :height 10 :stick t)))


  (defun popwin-term:multi-term ()
  (interactive)
  (popwin:display-buffer-1
   (or (get-buffer "*terminal*")
       (save-window-excursion
         (call-interactively 'multi-term)))
   :default-config-keywords '(:position :bottom :height 10 :stick t)))

(provide 'conf-popwin)
