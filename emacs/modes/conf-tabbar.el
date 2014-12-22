(provide 'conf-tabbar)

                                        ;(require 'tabbar-ruler)
(setq tabbar-ruler-global-tabbar t) ; If you want tabbar
;;(setq tabbar-ruler-global-ruler t) ; if you want a global ruler
;;(setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
;;(setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
;;(setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the scroll bar when your mouse is moving.

(custom-set-variables
 '(tabbar-mode t nil (tabbar))
 '(ide-skel-tabbar-mwheel-mode nil (tabbar))
 '(tabbar-scroll-left-button (quote (("") "")))
 '(tabbar-scroll-right-button (quote (("") "")))
 '(tabbar-button-highlight ((t (:inherit tabbar-button))))
 '(tabbar-highlight ((t nil)))
 )

                                        ;(setq tabbar-cycle-scope (quote tabs))
                                        ;(setq table-time-before-update 0.1)
                                        ;(setq tabbar-use-images t)

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
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ((member (buffer-name)
             '("*scratch*" "*Help*" "*Completions*"))
     "Common"
     )
                                        ;((string-equal "*" (substring (buffer-name) 0 1))
                                        ; "Common"
                                        ; )
    ((member (buffer-name)
             '("*Backtrace*" "*Compile-Log*" "*Messages*"))
     "Debugger"
     )
    ((member (buffer-name)
             '("xyz" "day" "m3" "abi" "for" "nws" "eng" "f_g" "tim" "tmp"))
     "Main"
     )
    ((memq major-mode '(python-mode php-mode emacs-lisp-mode sh-mode makefile-gmake-mode perl-mode c-mode c++-mode django-mode python-django))
     "Programming"
     )
    ((memq major-mode '(nxhtml-mode web-mode avascript-mode js3-mode js-mode js2-mode javascript js2-refactor emmet-mode less-css-mode css-mode))
     "Web"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Common"
     )
    ((memq major-mode
           '(mu4e-view-mode mu4e-main-mode mu4e-headers-mode mu4e-view-raw-mode mu4e-compose-mode message-mode mail-mode))
     "Activity"
     )
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode"
     )
    ((memq major-mode '(shell-mode term-mode eshell-mode multi-term-mode))
     "Shell")
    ((or
      (memq major-mode '(slime-mode slime-repl-mode sldb-mode slime-inspector-mode))
      (equal "*slime-source*" (buffer-name (current-buffer)))
      (string-match ".*cider.*" (buffer-name b))
      (equal "*inferior-lisp*" (buffer-name (current-buffer)))
      (equal "*ielm*" (buffer-name (current-buffer))))
     "Interaction")
    ((or (memq major-mode '(magit-mode magit-log-mode magit-commit-mode magit-key-mode magit-diff-mode
                                       magit-wip-mode magit-wip-save-mode magit-status-mode magit-stath-mode
                                       magit-log-edit-mode magit-branch-manager-mode magit-wazzup-mode
                                       magit-reflog-mode))
         (equal "*magit-process*" (buffer-name (current-buffer)))
         (string-match "\\*Magit.*\\*" (buffer-name (current-buffer))))
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

                                        ;(defun tabbar-switch-to-default-grouping ()
                                        ;  (interactive)
                                        ;  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups))

(defun tabbar-switch-to-grouping-by-dir ()
  (interactive)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-dir))

(defun switch-tabbar (num)
  (let* ((tabs (tabbar-tabs
                (tabbar-current-tabset)
                ;; (tabbar-get-tabset "All Buffers")
                ))
         (tab (nth
               (if (> num 0) (- num 1) (+ (length tabs) num))
               tabs)))
    (if tab (switch-to-buffer (car tab)))))

;; Shortcuts
(global-set-key (kbd "C-1") (lambda () (interactive) (switch-tabbar 1)))
(global-set-key (kbd "C-2") (lambda () (interactive) (switch-tabbar 2)))
(global-set-key (kbd "C-3") (lambda () (interactive) (switch-tabbar 3)))
(global-set-key (kbd "C-4") (lambda () (interactive) (switch-tabbar 4)))
(global-set-key (kbd "C-5") (lambda () (interactive) (switch-tabbar 5)))
(global-set-key (kbd "C-6") (lambda () (interactive) (switch-tabbar 6)))
(global-set-key (kbd "C-7") (lambda () (interactive) (switch-tabbar 7)))
(global-set-key (kbd "C-8") (lambda () (interactive) (switch-tabbar 8)))
(global-set-key (kbd "C-9") (lambda () (interactive) (switch-tabbar 9)))
(global-set-key (kbd "C-0") (lambda () (interactive) (switch-tabbar -1)))

(global-set-key (kbd "C-x <up>") 'tabbar-forward-tab)
(global-set-key (kbd "C-x <down>") 'tabbar-backward-tab)
(global-set-key (kbd "C-x <right>") 'tabbar-forward-group)
(global-set-key (kbd "C-x <left>") 'tabbar-backward-group)

(global-set-key (kbd "C-M-g") 'toggle-tabbar-mode)
(global-set-key (kbd "M-g") 'tabbar-switch-to-grouping-by-dir)
                                        ;(global-set-key (kbd "C-M-g") 'tabbar-switch-to-default-grouping)

(global-set-key (kbd "C-c C-t") 'tabbar-ruler-move)

;; Apariencia
(setq tabbar-separator '(0.0))
(setq tabbar-background-color "#001214") ;; the color of the tabbar background

(set-face-attribute 'tabbar-default nil :background "black")
(set-face-attribute 'tabbar-unselected nil :background "black" :foreground "white" :box '(:line-width 1 :color "cyan" ))
(set-face-attribute 'tabbar-selected nil :background "cyan" :foreground "black" :box '(:line-width 1 :color "cyan" ))
(set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "black" :style released-button));
(set-face-attribute 'tabbar-highlight nil :underline nil)
(set-face-attribute 'tabbar-separator nil :height 0.5)
