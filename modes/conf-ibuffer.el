;;; Code:
(require 'ibuffer)
(require 'ibuf-ext)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Common"
                (or
                 (name . "^\\*scratch\\*$")
                 (name . "^\\*Help\\*$")))
               ("Dired"
                (or
                 (mode . dired-mode)
                 (mode . direx:direx-mode)))
               ("Web"
                (or
                 (mode . nxhtml-mode)
                 (mode . web-mode)
                 (mode . json-mode)
                 (mode . emmet-mode)
                 (mode . less-css-mode)
                 (mode . css-mode)
                 (mode . restclient-mode)))
               ("Programming"
                (or
                 (mode . python-mode)
                 (mode . php-mode)
                 (mode . emacs-lisp-mode)
                 (mode . sh-mode)
                 (mode . makefile-gmake-mode)
                 (mode . perl-mode)
                 (mode . c-mode)
                 (mode . c++-mode)
                 (mode . django-mode)
                 (mode . python-django)
                 (mode . javascript-mode)
                 (mode . js-mode)
                 (mode . js2-mode)
                 (mode . js3-mode)
                 (mode . web-jsx-mode)
                 (mode . js2-refactor)
                 (mode . java-mode)))
               ("Magit"
                (or
                 (name . "\.*magit.*\*")
                 (name . "COMMIT_EDITMSG")))
               ("Terminal"
                (mode . term-mode))
               ("Email"
                (or
                 (mode . mu4e-view-mode)
                 (mode . mu4e-main-mode)
                 (mode . mu4e-headers-mode)
                 (mode . mu4e-view-raw-mode)
                 (mode . mu4e-compose-mode)
                 (mode . message-mode)
                 (mode . mail-mode)))
               ("Erc"
                (or
                 (mode . erc-mode)
                 (name . "\\*irc.*\\*")
                 (name . "\\irc.*\\*")
                 (name . "^\\irc.*\\*")))
               ("Social"
                (or
                 (mode . gnu-social-mode)
                 (name . "\\*gnu-social.*\\*")))
               ("Im"
                (or
                 (mode . jabber-chat-mode)
                 (mode . jabber-roster-mode)))
               ("News"
                (or
                 (mode . elfeed-search-mode)
                 (mode . elfeed-show-mode)))
               ("OrgMode"
                (or
                 (mode . org-mode)
                 (mode . org-agenda-mode)))
               ("Planner"
                (or
                 (name . "^\\*Calendar\\*$")
                 (name . "^diary$")
                 (mode . muse-mode)))
               ("Debugger"
                (or
                 (name . "*Backtrace*")
                 (name . "*Compile-Log*")
                 (name . "*Messages*")
                 (name . "*Warnings*")
                 (name . "*Flycheck error messages*")
                 (name . "*fsm-debug*")
                 (name . "*Shell Command Output*")
                 (name . "\\*.*log.*\\*")))
               ))))

;; Functions
(defun distopico:ibuffer-mode-hook ()
  "Setup ibuffer groups."
  (ibuffer-switch-to-saved-filter-groups "default"))

;; Hooks
(add-hook 'ibuffer-mode-hook #'distopico:ibuffer-mode-hook)

(provide 'conf-ibuffer)
;;; conf-ibuffer.el ends here
