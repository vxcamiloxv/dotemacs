;;; Code:

(require 'tabbar)
;;(require 'tabbar-ruler)
;;(setq tabbar-ruler-global-tabbar t) ; If you want tabbar
;;(setq tabbar-ruler-global-ruler t) ; if you want a global ruler
;;(setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
;;(setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
;;(setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the scroll bar when your mouse is moving.

;; Enabled
(tabbar-mode)

;; Config
(setq tabbar-use-images t
      ;; ide-skel-tabbar-mwheel-mode nil
      tabbar-separator '(0.0)
      tabbar-background-color "#001214"
      tabbar-scroll-left-button '(("") "")
      tabbar-scroll-right-button '(("") "")
      tabbar-cycle-scope 'tabs)


;; Add a buffer modification state indicator in the tab label, and place a
;; space around the label to make it looks less crowd.
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat " + " (concat ad-return-value " "))
          (concat " " (concat ad-return-value " ")))))

;; Called each time the modification state of the buffer changed.
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

;; First-change-hook is called BEFORE the change is made.
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)

;; This doesn't work for revert, I don't know.
;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (member (buffer-name)
                 '("*HTTP Response*"))
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ((or
      (memq major-mode
            '(apropos-mode Info-mode Man-mode markdown-mode))
      ;; (tabbar-buffer-mode-derived-p major-mode
      ;;                               '(special-mode))
      (member (buffer-name)
              '("*scratch*" "*Help*"))
      )
     "Common"
     )
    ;;((string-equal "*" (substring (buffer-name) 0 1))
    ;; "Common"
    ;; )
    ((member (buffer-name)
             '("*Backtrace*" "*Compile-Log*" "*Messages*" "*Warnings*" "*Flycheck error messages*" "*fsm-debug*" "*Shell Command Output*"))
     "Debugger"
     )
    ((member (buffer-name)
             '("xyz" "day" "m3" "abi" "for" "nws" "eng" "f_g" "tim" "tmp"))
     "Main"
     )
    ((memq major-mode '(python-mode php-mode emacs-lisp-mode sh-mode makefile-gmake-mode perl-mode c-mode c++-mode django-mode python-django javascript-mode js3-mode js-mode js2-mode js2-refactor))
     "Programming"
     )
    ((memq major-mode '(nxhtml-mode web-mode json-mode emmet-mode less-css-mode css-mode restclient-mode))
     "Web"
     )
    ((memq major-mode
           '(mu4e-view-mode mu4e-main-mode mu4e-headers-mode mu4e-view-raw-mode mu4e-compose-mode message-mode mail-mode))
     "Email"
     )
    ((or (memq major-mode
               '(erc-mode))
         (string-match "\\*irc.*\\*" (buffer-name (current-buffer)))
         (string-match "\\irc.*\\*" (buffer-name (current-buffer))))
     "Irc"
     )
    ((or (memq major-mode
               '(jabber-chat-mode jabber-roster-mode))
         (string-match "\\*gnu-social.*\\*" (buffer-name (current-buffer))))
     "Social"
     )
    ((memq major-mode '(org-mode org-agenda-mode))
     "OrgMode"
     )
    ;; ((or (memq major-mode '(shell-mode term-mode eshell-mode multi-term-mode))
    ;;      (equal "*ansi-term*" (buffer-name (current-buffer)))
    ;;      (string-match "\\*terminal.*\\*" (buffer-name (current-buffer))))
    ;;  "Shell")
    ((or
      (memq major-mode '(lisp-interaction-mode dired-mode help-mode slime-mode slime-repl-mode sldb-mode
                                               slime-inspector-mode calendar-mode diary-mode
                                               diary-fancy-display-mode Custom-mode))
      (member (buffer-name) '("*Completions*" "*Alerts*" "*Clock Task Select*" "*slime-source*" "*inferior-lisp*" "*ielm*" "*Org tags*"))
      (string-match "\\*helm.*\\*" (buffer-name (current-buffer)))
      (string-match ".*cider.*" (buffer-name (current-buffer))) )
     "Interaction")
    ((or (memq major-mode '(magit-mode magit-log-mode magit-commit-mode magit-key-mode magit-diff-mode
                                       magit-wip-mode magit-wip-save-mode magit-status-mode magit-stath-mode
                                       magit-log-edit-mode magit-branch-manager-mode magit-wazzup-mode
                                       magit-reflog-mode gitignore-mode git-commit-mode
                                       git-rebase-mode gitattributes-mode gitconfig-mode))
         (member (buffer-name) '("*magit-process*" "COMMIT_EDITMSG"))
         (string-match "\\magit.*\\*" (buffer-name (current-buffer))))
     "Magit")
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the header line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

;; tabbar grouping method
(defun tabbar-buffer-groups-by-dir ()
  "Put all files in the same directory into the same tab bar"
  (with-current-buffer (current-buffer)
    (let ((dir (expand-file-name default-directory)))
      (cond ;; assign group name until one clause succeeds, so the order is important
       ((eq major-mode 'dired-mode)
        (list "Dired"))
       ((memq major-mode
              '(help-mode apropos-mode Info-mode Man-mode))
        (list "Help"))
       ((string-match-p "\*.*\*" (buffer-name))
        (list "Misc"))
       (t (list dir))))))

;; Show ALL Tabs
(setq tbbr-md "all")
(defun toggle-tabbar-mode ()
  "Toggles tabbar modes - all buffers vs. defined in the `tabbar-buffer-groups'."
  (interactive)
  (if (string= tbbr-md "groups")
      (progn ;; then
        (setq tabbar-buffer-groups-function
              (lambda ()
                (list "All")))
        (setq tbbr-md "all"))
    (progn ;; else
      (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
      (setq tbbr-md "groups"))))

(defun tabbar-switch-to-default-grouping ()
  (interactive)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups))

(defun tabbar-switch-to-grouping-by-dir ()
  (interactive)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-dir))

(defun switch-tabbar (num)
  (let* ((tabs (tabbar-tabs
                (tabbar-current-tabset)
                ;;(tabbar-get-tabset "All Buffers")
                ))
         (tab (nth
               (if (> num 0) (- num 1) (+ (length tabs) num))
               tabs)))
    (if tab (switch-to-buffer (car tab)))))

;;(global-set-key (kbd "C-c C-t") 'tabbar-ruler-move)

;; (set-face-attribute 'tabbar-default nil :background "black")
;; (set-face-attribute 'tabbar-unselected nil :background "black" :foreground "white" :box '(:line-width 1 :color "cyan" ))
;; (set-face-attribute 'tabbar-selected nil :background "cyan" :foreground "black" :box '(:line-width 1 :color "cyan" ))
;; (set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "black" :style released-button));
;; (set-face-attribute 'tabbar-highlight nil :underline nil)
;; (set-face-attribute 'tabbar-separator nil :height 0.5)

(provide 'conf-tabbar)
