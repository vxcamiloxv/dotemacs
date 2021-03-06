;;; Code:

(require 'popwin)
(require 'setup-path)
;; Add popwin msic
(add-to-list 'load-path (distopico:path-join el-get-dir "popwin/misc"))
(require 'popwin-term)
(require 'popwin-browse-kill-ring)

(popwin-mode t)
(global-set-key (kbd "C-x p") popwin:keymap)


;; popwin settings
(setq popwin:special-display-config
      '(("*Help*" :height 0.4 :stick t)
        ("\\*.*" :regexp t :position bottom :noselect t :height 0.3 :stick t)
        ;; Debug
        ("*Warnings*" :position bottom :height 0.3 )
        ("*Backtrace*" :position bottom :height 0.3 )
        ("*Messages*" :position bottom :height 0.3 )
        ("*Compile-Log*" :position bottom :height 0.3 )
        ("*Shell Command Output*" :position bottom :height 0.3 )
        (".*overtone.log" :regexp t :height 0.3)
        ("collected.org" :position top :height 15)
        (flycheck-error-list-mode :position bottom :height 0.3 :stick t)
        (compilation-mode :position bottom :height 0.3 :noselect t)
        ;; Utils
        ("helm" :regexp t :height 0.3)
        ("*Occur*" :position bottom :height 0.3)
        ("\\*Slime Description.*" :noselect t :regexp t :height 0.3)
        ("*undo-tree*" :width 0.3 :position right)
        ("*grep*" :position bottom :height 0.2 :stick t)
        ("*Completions*" :height 0.4)
        ("*compilation*" :height 0.4 :noselect t :stick t)
        ("*quickrun*" :height 0.3 :stick t)
        ;; Magit/vc
        (magit-status-mode :position bottom :noselect t :height 0.3 :stick t)
        ("COMMIT_EDITMSG" :position bottom :noselect t :height 0.3 :stick t)
        ("*magit-commit*" :position bottom :noselect t :height 0.3 :stick t)
        ("\\*magit.*" :regexp t :position bottom :noselect t :height 0.3 :stick t)
        ("*magit-diff*" :position bottom :noselect t :height 0.3)
        ("*magit-edit-log*" :position bottom :noselect t :height 0.2)
        ("*magit-process*" :position bottom :noselect t :height 0.2)
        ("*vc-diff*" :position bottom :noselect t :height 0.2)
        ("*vc-change-log*" :position bottom :noselect t :height 0.2)
        ;; Navigator
        ("*Ibuffer*" :position bottom :height 0.2)
        ("*Ido Completions*" :noselect t :height 0.3)
        ("*imenu-tree*" :position left :width 50 :stick t)
        (direx:direx-mode :position left :width 50 :dedicated t)

        ("*gists*" :height 0.3)
        ("*sldb.*":regexp t :height 0.3)
        ("*Gofmt Errors*" :noselect t)
        ("\\*godoc*" :regexp t :height 0.3)
        ("*nrepl-error*" :height 0.2 :stick t)
        ("*nrepl-doc*" :height 0.2 :stick t)
        ("*nrepl-src*" :height 0.2 :stick t)
        ("*Kill Ring*" :height 0.3)
        ("*project-status*" :noselect t)
        ("*Compile-Log" :height 0.2 :stick t)
        ("*pytest*" :noselect t)
        ;; Programing
        ("Django:" :regexp t :width 0.3 :position right)
        ("*Python*" :stick t)
        ("*jedi:doc*" :noselect t)
        ;; Console
        ("*shell*" :height 0.3)
        ("\\*ansi-term.*\\*" :regexp t :height 0.3)
        ("\\*terminal.*\\*" :regexp t :height 0.3)
        (term-mode :position :bottom :height 10 :stick t)
        ;; Org/Organized
        (diary-fancy-display-mode :position left :width 50 :stick nil)
        (diary-mode :position bottom :height 15 :stick t)
        (calendar-mode :position bottom :height 15 :stick nil)
        (org-agenda-mode :position bottom :height 15 :stick t)
        ("*Org Agenda.*\\*" :regexp t :position bottom :height 15 :stick t)
        )
      )

(defun live-display-ansi ()
  (interactive)
  (popwin:display-buffer "*ansi-term*"))

(defun popwin-term:ansi-term ()
  (interactive)
  (popwin:display-buffer-1
   (or (get-buffer "*ansi-term*")
       (save-window-excursion
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
