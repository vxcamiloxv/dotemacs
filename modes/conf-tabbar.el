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

;; Control vars
(defvar distopico:tbbr-md "all")

;; Functions

(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  "Add a buffer modification state indicator in the tab label, and place a
space around the label to make it looks less crowd."
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat " + " (concat ad-return-value " "))
          (concat " " (concat ad-return-value " ")))))

(defun distopico:modification-state-change ()
  "Called each time the modification state of the buffer changed."
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

(defun distopico:on-buffer-modification ()
  "First-change-hook is called BEFORE the change is made."
  (set-buffer-modified-p t)
  (distopico:modification-state-change))

;; Tabbar grouping
(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((or (member (buffer-name)
                 '("*HTTP Response*"))
         ;; (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process")
    ((or
      (memq major-mode
            '(apropos-mode Info-mode Man-mode markdown-mode))
      ;; (tabbar-buffer-mode-derived-p major-mode
      ;;                               '(special-mode))
      (member (buffer-name)
              '("*scratch*" "*Help*"))
      )
     "Common")
    ;;((string-equal "*" (substring (buffer-name) 0 1))
    ;; "Common"
    ;; )
    ((or (member (buffer-name)
                 '("*Backtrace*" "*Compile-Log*" "*Messages*" "*Warnings*" "*Flycheck error messages*"
                   "*fsm-debug*" "*Shell Command Output*"))
         (string-match "\\*.*log.*\\*" (buffer-name (current-buffer))))
     "Debugger")
    ((member (buffer-name)
             '("xyz" "day" "m3" "abi" "for" "nws" "eng" "f_g" "tim" "tmp"))
     "Main")
    ((memq major-mode
           '(dired-mode direx:direx-mode))
     "Dired")
    ((memq major-mode '(python-mode php-mode emacs-lisp-mode sh-mode makefile-gmake-mode perl-mode c-mode c++-mode django-mode python-django javascript-mode js3-mode js-mode js2-mode js2-refactor java-mode))
     "Programming"
     )
    ((memq major-mode '(nxhtml-mode web-mode json-mode emmet-mode less-css-mode css-mode restclient-mode))
     "Web")
    ((memq major-mode
           '(mu4e-view-mode mu4e-main-mode mu4e-headers-mode mu4e-view-raw-mode mu4e-compose-mode message-mode mail-mode))
     "Email")
    ((or (memq major-mode
               '(erc-mode))
         (tabbar-buffer-mode-derived-p major-mode '(erc-mode))
         (member (buffer-name)
                 '("irc.freenode.net:6667"))
         (string-match "\\*irc.*\\*" (buffer-name (current-buffer)))
         (string-match "\\irc.*\\*" (buffer-name (current-buffer))))
     "Irc")
    ((or (memq major-mode
               '(gnu-social-mode))
         (string-match "\\*gnu-social.*\\*" (buffer-name (current-buffer))))
     "Social")
    ((memq major-mode
           '(jabber-chat-mode jabber-roster-mode))
     "Im")
    ((memq major-mode
           '(elfeed-search-mode elfeed-show-mode))
     "News")
    ((memq major-mode '(org-mode org-agenda-mode))
     "OrgMode")
    ;; ((or (memq major-mode '(shell-mode term-mode eshell-mode multi-term-mode))
    ;;      (equal "*ansi-term*" (buffer-name (current-buffer)))
    ;;      (string-match "\\*terminal.*\\*" (buffer-name (current-buffer))))
    ;;  "Shell")
    ((or
      (memq major-mode '(lisp-interaction-mode help-mode slime-mode slime-repl-mode sldb-mode
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
         (member (buffer-name) '("COMMIT_EDITMSG"))
         (string-match "\\*magit.*" (buffer-name (current-buffer))))
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

(defun tabbar-switch-to-default-grouping ()
  (interactive)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups))

(defun tabbar-switch-to-grouping-by-dir ()
  (interactive)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-dir))

;; Toggle group
(defun toggle-tabbar-mode ()
  "Toggles tabbar modes - all buffers vs. defined in the `tabbar-buffer-groups'."
  (interactive)
  (if (string= distopico:tbbr-md "groups")
      (progn ;; then
        (setq tabbar-buffer-groups-function
              (lambda ()
                (list "All")))
        (setq distopico:tbbr-md "all"))
    (progn ;; else
      (if (string= distopico:tbbr-md "all")
          (progn ;; then
            (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-dir)
            (setq distopico:tbbr-md "dir"))
        (progn ;; else
          (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
          (setq distopico:tbbr-md "groups"))))))


;; Rewrite add tab, short by name
;; (defun tabbar-add-tab (tabset object &optional append_ignored)
;;   "Add to TABSET a tab with value OBJECT if there isn't one there yet.
;;  If the tab is added, it is added at the beginning of the tab list,
;;  unless the optional argument APPEND is non-nil, in which case it is
;;  added at the end."
;;   (let ((tabs (tabbar-tabs tabset)))
;;     (if (tabbar-get-tab object tabset)
;;         tabs
;;       (let ((tab (tabbar-make-tab object tabset)))
;;         (tabbar-set-template tabset nil)
;;         (set tabset (sort (cons tab tabs)
;;                           (lambda (a b) (string< (buffer-name (car a)) (buffer-name (car b))))))))))

(defun switch-tabbar (num)
  "Switch tabbar by postiion"
  (let* ((tabs (tabbar-tabs
                (tabbar-current-tabset)
                ;;(tabbar-get-tabset "All Buffers")
                ))
         (tab (nth
               (if (> num 0) (- num 1) (+ (length tabs) num))
               tabs)))
    (if tab (switch-to-buffer (car tab)))))

;; Hooks
(add-hook 'after-save-hook 'distopico:modification-state-change)
(add-hook 'first-change-hook 'distopico:on-buffer-modification)
;;(add-hook 'after-revert-hook 'distopico:modification-state-change) ;; This doesn't work for revert, I don't know.

;;(global-set-key (kbd "C-c C-t") 'tabbar-ruler-move)


(provide 'conf-tabbar)
