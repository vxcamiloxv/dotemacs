;;; Code:
(require 'org)
(require 'appt)
(require 'diary-lib)
(require 'lunar)
(require 'diary-loaddefs)
(require 'org-clock)
(require 'org-contacts)
(require 'org-habit)
(require 'org-gnus)
(require 'org-notify)
(require 'org-depend)
(require 'org-mime)
(require 'org-crypt)
(require 'org-protocol)
(require 'org-projectile)
(require 'org-annotate-file)
(require 'ox-rst)
(require 'notifications)
(require 'midnight)


;; Vars
(defvar distopico:org-directory (expand-file-name "~/Documents/org/"))
(defvar distopico:contacts-files (concat distopico:org-directory "contacts.org" ))
(defvar distopico:icon-org-mode (in-emacs-d "themes/icons/"))
(defvar distopico:org-clock-default-effort "1:00")

(setq org-modules
      (append org-modules
              '(org-contacts
                org-habit
                org-gnus
                org-toc
                org-notify
                ;;org-pomodoro
                org-depend
                org-mime
                org-crypt
                org-protocol
                org-projectile
                ;;org-panel
                )))

(setq org-note-abort nil ;; Keep change when finalized
      org-log-done 'time
      org-log-into-drawer t
      org-startup-indented t
      org-hide-leading-stars t
      org-completion-use-ido t
      org-auto-align-tags t
      org-support-shift-select t
      org-track-ordered-property-with-tag t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-yank-adjusted-subtrees t
      org-use-speed-commands t
      org-special-ctrl-k t
      org-special-ctrl-a/e t
      ;; nil
      org-reverse-note-order nil
      org-M-RET-may-split-line nil
      org-refile-use-outline-path nil
      ;; org-indirect-buffer-display 'current-window
      org-cycle-separator-lines 1
      org-tags-match-list-sublevels 'indented
      org-blank-before-new-entry '((heading . auto) (plain-list-item . auto))
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
      org-goto-interface 'outline
      org-goto-max-level 10
      org-tags-column -120
      ;; org-src-window-setup 'current-window
      org-stuck-projects
      '("TODO=\"PROJECT\""
        ("TODO" "STARTED" "NEXT" "REWORK" "VERIFY" "DELEGATED")
        nil "")
      org-todo-state-tags-triggers '(("CANCELLED" ("ARCHIVE" . t))))

;; Default directory
(setq org-directory distopico:org-directory)

;; Contacts
(setq org-contacts-files (list distopico:contacts-files))

;;Archive
(setq org-archive-location (expand-file-name "archive/archive_%s::" org-directory))

;; General Notes
(setq org-default-notes-file (expand-file-name "organizer.org" org-directory))

;; Annotation
(setq org-annotate-file-storage-file (expand-file-name "annotated.org" org-directory)
      org-annotate-file-add-search t)

;; Agenda
(setq org-agenda-files (file-expand-wildcards (concat org-directory "*.org"))) ;; I'm not sure if it's good idea include all files

(setq org-agenda-window-setup 'current-window
      ;; org-agenda-restore-windows-after-quit t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-timestamp-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-insert-diary-extract-time t
      org-agenda-todo-list-sublevels t
      org-agenda-dim-blocked-tasks t
      ;; nil
      org-agenda-include-diary nil
      org-agenda-sticky nil
      org-agenda-remove-tags nil
      ;; Other
      org-agenda-tags-column -102
      org-agenda-span 2
      org-agenda-day-face-function 'distopico:org-agenda-day-face-holidays-function
      org-agenda-category-icon-alist
      '(("Emacs" "/usr/share/icons/hicolor/16x16/apps/emacs.png" nil nil :ascent center)
        ("\\(Holidays\\|Vacation\\)"  "~/.emacs.d/themes/icons/holidays.png" nil nil :ascent center)
        (".*" '(space . (:width (16)))))
      org-agenda-custom-commands
      '(("p" "Projects" todo "PROJECT"
         ((org-agenda-dim-blocked-tasks t)
          (org-agenda-skip-scheduled-if-done nil)
          (org-agenda-skip-deadline-if-done nil)
          (org-agenda-todo-ignore-with-date nil)
          (org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)))
        ("b" "Things to buy any time" tags-todo "+tobuy+SCHEDULED=\"\"")
        ("y" "Syadmin stuff to do" tags-todo "+sysadmin+SCHEDULED=\"\"")
        ("d" "Daily tasks:" tags "daily")
        ))

;; Enable display of the time grid so we can see the marker for the current time
;; (setq org-agenda-time-grid (quote ((daily today remove-match)
;;                                    #("----------------" 0 16 (org-heading t))
;;                                    (0900 1100 1300 1500 1700))))

;; Diary TODO: Move to calendar.el
(setq diary-file (concat org-directory "diary")
      calendar-mark-diary-entries-flag t
      calendar-mark-holidays-flag t
      diary-number-of-entries 2
      lunar-phase-names
      '("● New Moon"
        "☽ First Quarter Moon"
        "○ Full Moon"
        "☾ Last Quarter Moon")
      )
(add-to-list 'auto-mode-alist '("diary" . diary-mode))
(add-hook 'diary-display-hook 'appt-make-list)
(add-hook 'diary-hook (lambda()
                        (tabbar-local-mode -1)))
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;;Appoiments
(setq appt-audible nil
      appt-display-diary nil
      appt-display-mode-line t
      appt-display-format 'window
      appt-message-warning-time 15
      appt-display-interval 15
      appt-disp-window-function
      (lambda (left time message)
        (notifications-notify
         :title (concat "Appointment "
                        (cond
                         ((equal left "0")
                          "now!")
                         ((equal left "1")
                          "in 1 minute!")
                         (t
                          (format "in %s minutes" left))))
         :body message
         :timeout 5000
         :urgency "normal"
         :category "emacs.message"
         :app-icon (concat distopico:icon-org-mode "org-mode.png"))
        )
      appt-delete-window-function (lambda ()))

;; Notify
(setq org-notify-audible nil)
(org-notify-add 'appt
                '(:time "-1s" :period "20s" :duration 10
                        :actions (-message -ding))
                '(:time "15m" :period "2m" :duration 100
                        :actions -notify)
                '(:time "2h" :period "5m" :actions -message)
                '(:time "3d" :actions -email))

;; Clocks
(setq org-clock-auto-clock-resolution 'when-no-clock-is-running
      org-clock-clocktable-default-properties '(:maxlevel 3 :scope file)
      org-clock-history-length 20
      org-clock-in-resume t
      org-clock-in-switch-to-state 'distopico:clock-in-to-next
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done '("HOLD" "WAITING" "CANCELLED" "DONE")
      org-clock-into-drawer t
      org-clock-persist 'history
      org-clock-report-include-clocking-task t
      org-show-notification-handler
      (lambda (notification)
        (notifications-notify
         :title "Org-mode"
         :body notification
         :timeout 5000
         :urgency "normal"
         :category "emacs.message"
         :app-icon (concat distopico:icon-org-mode "org-mode.png"))
        )
      org-clock-persist-file (in-emacs-d ".cache/org-clock-save.el")
      org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Habits
(setq org-habit-graph-column 60
      org-habit-show-done-always-green t)

;; Todo
(setq org-use-fast-todo-selection t
      org-enforce-todo-dependencies t
      org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "STARTED(s!)" "|" "DONE(d!)")
        (sequence "HOLD(h)" "WAITING(w@/!)" "|" "CANCELED(c@/!)")
        (sequence "PROJECT(p!)" "|" "SOMEDAY(m)")
        ))

;; Custom faces
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#df3800" :weight bold))
        ("STARTED" . (:foreground "#566ea2" :weight bold))
        ("NEXT" . (:foreground "DeepPink2" :weight bold))
        ("HOLD" . (:foreground "#b68800" :weight bold))
        ("WAITING" . (:foreground "#4d9694" :weight bold))
        ("CANCELED" . (:foreground "#e00051" :weight bold))
        ("DONE" . (:foreground "#448c27" :weight bold))
        ("PROJECT" . (:foreground "orchid" :weight bold))))

;; Priorities
(setq org-default-priority ?D
      org-lowest-priority ?E
      org-priority-faces
      '((?A . "#f01a0f")
        (?B . "#f0640f")
        (?C . "light sea green")
        (?D . "slate blue")))

;; (setq org-todo-keyword-faces            ; What they look like.
;;       '(("TODO"        :foreground "red")
;;         ("IN PROGRESS" :foreground "yellow")
;;         ("DONE"        :foreground "forest green")
;;         ("SUCCEEDED"   :foreground "forest green")
;;         ("WAITING"     :foreground "orange")
;;         ("CANCELLED"   :foreground "orangered")
;;         ("FAILED"      :foreground "orangered")))

;; Default tags
(setq org-tag-persistent-alist '(("@work"      . ?b)
                                 ("@home"      . ?h)
                                 ("@writing"   . ?w)
                                 ("@errands"   . ?e)
                                 ("@drawing"   . ?d)
                                 ("@haking"    . ?c)
                                 ("@reading"   . ?r)
                                 ("@laptop"    . ?l)
                                 ("@pc"        . ?p)
                                 ("lowenergy"  . ?0)
                                 ("highenergy" . ?1)
                                 ("TOC"        . ?T)))

;; Templates
(setq org-capture-templates
      '(("f" "Todo" entry
         (file+function (concat org-directory "todo.org") distopico:org-ask-location)
         "** TODO %?\n %A \n %i \n" :empty-lines-after 2 :empty-lines-before 1 :clock-resume t)
        ("t" "Tasks" entry
         (file+headline (concat org-directory "todo.org") "Various Tasks")
         "** TODO %^{Task}%?\n SCHEDULED: %^t\n" :empty-lines-after 2 :empty-lines-before 1)
        ("T" "Quick task" entry
         (file+headline (concat org-directory "todo.org") "Quick Task")
         "** TODO %^{Task}" :immediate-finish t)
        ("n" "Note" entry
         (file+headline org-default-notes-file "General Notes")
         "* %^{Title}\n  :PROPERTIES:\n  :CreationTime:  %U\n  :END:\n\n  %i\n\n  %a"
         :prepend t :empty-lines 1)
        ("Q" "Quick note" item
         (file+headline org-default-notes-file "Quick notes"))
        ("i" "Idea" entry
         (file+headline org-default-notes-file "Idea")
         "* %^{Title}\n  %i\n %a" :prepend t :empty-lines 1)
        ("c" "Contacts" entry
         (file+headline distopico:contacts-files "General")
         "** %(org-contacts-template-name)%?\n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:NICKNAME: \n:END:\n"
         :empty-lines 1)
        ("h" "Habit" entry
         (file+headline (concat org-directory "myday.org") "Habits")
         "* TODO %?\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %h %H:%M .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n:Captured_at: %U\n:END:"
         :empty-lines-after 2 :empty-lines-before 1 :clock-resume t)
        ("l" "Life Task" entry
         (file+headline (concat org-directory "life.org") "Life Tasks")
         "* TODO %?\n  %i\n  %a" :empty-lines 1)
        ("b" "Stuff to buy" entry
         (file+headline (concat org-directory "life.org") "Stuff to buy")
         "* TODO %^{Title}%?\n" :prepend t :empty-lines 1)
        ("o" "Organizer" entry
         (file+function org-default-notes-file distopico:org-ask-location)
         "** %?\n<%<%Y-%m-%d %a %T>>" :empty-lines 1)
        ("e" "Event" entry
         (file+headline (concat org-directory "calendar.org")  "Events")
         "* %^{Event}%? %^g\n %^T\n %i\n %^{APPT_WARNTIME}p\n" :empty-lines 1)
        ("E" "Event Link" entry
         (file+headline (concat org-directory "calendar.org")  "Events")
         "* %^{Event}%? %^g\n %A\n %^T\n %i\n:PROPERTIES:\n:APPT_WARNTIME: %^{prompt}\n:END:\n" :empty-lines 1)
        ("M" "Messages" entry
         (file+headline org-default-notes-file "Messages")
         "* NEXT Respond to %:fromaddress on %:subject\nSCHEDULED: %t\n%U\n%a\n\n")
        ))

(setq org-capture-templates-contexts
      '(("M" ((in-mode . "mu4e-headers-mode")
              (in-mode . "mu4e-view-mode")) )))

;; Table of Contects
;;      (if (require 'toc-org nil t)
;;          (add-hook 'org-mode-hook 'toc-org-enable)
;;        (warn "toc-org not found"))

;; Org Projectile
(setq org-projectile:projects-file  (expand-file-name "todo.org" org-directory))
(add-to-list 'org-capture-templates
             (org-projectile:project-todo-entry "p" "* TODO %? %a" "Project Todo"))


;; Export
(dolist (source '(md gfm rst))
  (add-to-list 'org-export-backends source t))

;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (js . t)
   (css . t)
   (latex . t)
   (python . t)
   (sh . t)))

;; Crypt
(org-crypt-use-before-save-magic)

(setq org-tags-exclude-from-inheritance '("crypt")
      org-crypt-disable-auto-save t)


;; General Hooks
(add-hook 'org-mode-hook 'distopico:org-init-hook)

(with-eval-after-load 'org-capture
  (add-hook 'org-capture-mode-hook #'flyspell-mode 'append)
  (add-hook 'org-capture-before-finalize-hook #'org-align-all-tags 'append)
  (add-hook 'org-capture-after-finalize-hook #'distopico:org-update-agenda-views 'append))

(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (distopico:org-update-appt)) 'append))

(with-eval-after-load 'org-clock
  (org-clock-persistence-insinuate)
  (add-hook 'org-clock-in-hook
            (lambda ()
              (pomodoro)
              (add-hook 'pomodoro-break-finished-hook 'org-clock-in-last)
              (add-hook 'pomodoro-finished-hook 'org-clock-out)
              (distopico:org-clock-in-set-state-to-started)) 'append)
  (add-hook 'org-clock-out-hook
            (lambda ()
              (distopico:remove-empty-drawer-on-clock-out)) 'append)
  (add-hook 'org-clock-cancel-hook
            (lambda ()
              (pomodoro-stop)
              (remove-hook 'pomodoro-break-finished-hook 'org-clock-in-last)
              (remove-hook 'pomodoro-finished-hook 'org-clock-out)
              (distopico:remove-empty-drawer-on-clock-out)) 'append))

(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (distopico:org-toggle-next-tag)
            (distopico:org-insert-trigger)) 'append)

;;(add-hook 'org-clock-in-prepare-hook 'distopico:org-mode-add-default-effort) I need?

;; midnight
(setq midnight-mode t)
(remove-hook 'midnight-hook 'clean-buffer-list)
(add-hook 'midnight-hook (lambda()
                           (distopico:org-update-appt)
                           (org-agenda-list)) 'append)

;; or (run-at-time "24:01" nil 'distopico:org-update-appt)

;; Custom map key
(define-key org-mode-map (kbd "C-s-<return>")  'distopico:org-insert-subheading)
(define-key org-mode-map (kbd "C-M-s-<return>")  'distopico:org-insert-todo-subheading)
(define-key org-mode-map (kbd "C-M-<return>")  'org-insert-heading-respect-content)
(define-key org-mode-map (kbd "M-s-<return>")  'distopico:org-insert-heading-for-next-day)
(define-key org-mode-map (kbd "C-<tab>")  'pcomplete)
(define-key org-mode-map (kbd "C-c k")  'org-cut-subtree)
(define-key org-mode-map (kbd "C-c q")  'org-copy-subtree)
(define-key org-mode-map (kbd "C-c y")  'org-paste-subtree)

(define-key org-agenda-mode-map (kbd "C-<tab>") 'org-agenda-cycle-show)
(define-key org-agenda-mode-map (kbd "M-d") 'distopico:org-agenda-done)
(define-key org-agenda-mode-map "d" 'distopico:org-agenda-mark-done-and-add)
(define-key org-agenda-mode-map "N" 'distopico:org-agenda-new)
(define-key org-agenda-mode-map "Y" 'org-agenda-todo-yesterday)
(define-key org-agenda-mode-map "I" 'org-pomodoro)
(define-key org-agenda-mode-map (kbd "M-q") 'org-agenda-quit)
(define-key org-agenda-mode-map (kbd "C-q") 'bury-buffer)
(define-key org-agenda-mode-map "q" 'bury-buffer)

(add-to-list 'org-speed-commands-user '("n" distopico:org-show-next-heading-tidily))
(add-to-list 'org-speed-commands-user '("p" distopico:org-show-previous-heading-tidily))
;; ------------
;; Functions
;; ------------

(defun distopico:org-init-hook ()
  ;; Org mime messages to html
  (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)
  ;;(turn-on-visual-line-mode)
  (distopico:org-saveplace)
  ;; (distopico:org-before-save-hook)
  ;; (and buffer-file-name
  ;;      (file-exists-p buffer-file-name)
  ;;      (save-buffer))
  (add-hook 'before-save-hook 'distopico:org-before-save-hook nil 'make-it-local)
  (add-hook 'after-save-hook 'distopico:org-after-save-hook nil 'make-it-local))

(defun distopico:org-after-save-hook ()
  "Hook for after save in org-mode"
  (when (eq major-mode 'org-mode)
    (distopico:org-update-appt)
    (distopico:org-remove-done-trigger)
    (distopico:org:remove-empty-propert-drawers)
    (message (concat "Wrote " (buffer-file-name)))))

(defun distopico:org-before-save-hook ()
  "Hook for before save in org-mode"
  (when (eq major-mode 'org-mode)
    (and buffer-file-name
         (file-exists-p buffer-file-name)
         (org-remove-redundant-tags))
    (org-align-all-tags)
    (org-update-all-dblocks)))

(org-no-warnings (defvar date))
(defun org-lunar-phases ()
  "Show lunar phase in Agenda buffer."
  (require 'lunar)
  (let* ((phase-list (lunar-phase-list (nth 0 date) (nth 2 date)))
         (phase (cl-find-if (lambda (phase) (equal (car phase) date))
                            phase-list)))
    (when phase
      (setq ret (concat (lunar-phase-name (nth 2 phase)) " "
                        (substring (nth 1 phase) 0 5))))))

(defun org-remove-redundant-tags ()
  "Remove redundant tags of headlines in current buffer.
A tag is considered redundant if it is local to a headline and
inherited by a parent headline.
From https://github.com/thisirs/dotemacs/blob/master/init-org.el"
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries
       (lambda ()
         (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
               local inherited tag)
           (dolist (tag alltags)
             (if (get-text-property 0 'inherited tag)
                 (push tag inherited) (push tag local)))
           (dolist (tag local)
             (if (member tag inherited) (org-toggle-tag tag 'off)))))
       t nil))))

(defun distopico:org-saveplace ()
  "Fix a problem with saveplace.el putting you back in a folded position"
  (when (outline-invisible-p)
    (save-excursion
      (outline-previous-visible-heading 1)
      (org-show-subtree))))

(defun distopico:org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (org-cycle-show-empty-lines t)
      (message "Boundary reached"))
    (org-overview)
    (org-cycle-show-empty-lines t)
    (org-reveal t)
    (org-show-entry)
    (show-children)
    (org-cycle-show-empty-lines t) ))

(defun distopico:org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-on-heading-p))
      (goto-char pos)
      (hide-subtree)
      (org-cycle-show-empty-lines t)
      (message "Boundary reached"))
    (org-overview)
    (org-cycle-show-empty-lines t)
    (org-reveal t)
    (org-show-entry)
    (show-children)
    (org-cycle-show-empty-lines t) ))

(defun distopico:org-insert-subheading (arg)
  "Insert a new subheading and demote it same to org-insert-subheading
but remove blank lines from level 1.
Works for outline headings and for plain lists alike."
  (interactive "P")
  (org-insert-heading arg)
  (let ((pos (point)))
    (let ((cur-level (org-current-level))
          (prev-level (org-get-previous-line-level)))
      (when (= prev-level 1)
        (forward-line -1)
        (delete-blank-lines)
        (goto-char (min (point) pos)))))
  (cond
   ((org-at-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item)))
  (end-of-line))

(defun distopico:org-insert-todo-subheading (arg)
  "Insert a new subheading with TODO keyword or checkbox and demote it,
same to org-insert-subheading but remove blank lines from level1.
Works for outline headings and for plain lists alike."
  (interactive "P")
  (org-insert-todo-heading arg)
  (let ((pos (point)))
    (let ((cur-level (org-current-level))
          (prev-level (org-get-previous-line-level)))
      (when (= prev-level 1)
        (forward-line -1)
        (delete-blank-lines)
        (goto-char (min (point) pos)))))
  (cond
   ((org-at-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item)))
  (end-of-line))

(defun distopico:org-insert-trigger ()
  "Automatically insert chain-find-next trigger when entry becomes NEXT with org-depend"
  (cond ((equal org-state "NEXT")
         (unless org-depend-doing-chain-find-next
           (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-down)")))
        ((not (member org-state org-done-keywords))
         (org-delete-property "TRIGGER")
         (message (concat "Change to " org-state)))))

(defun distopico:org-remove-done-trigger ()
  "*Remove all trigger is done in current file."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "You need to turn on Org mode for this function."))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "DONE" nil t)
      (save-excursion
        (org-delete-property "TRIGGER" )))))

(defun distopico:org:remove-empty-propert-drawers ()
  "*Remove all empty property drawers in current file."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "You need to turn on Org mode for this function."))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":PROPERTIES:" nil t)
      (save-excursion
        (org-remove-empty-drawer-at (match-beginning 0))))))

(defun distopico:org-toggle-next-tag ()
  "Org possibly toggle next tag based on todo kewyord"
  (save-excursion
    (org-back-to-heading)
    (let ((todo-is-next (equal (org-get-todo-state) "NEXT"))
          (next-in-tags (member "NEXT" (org-get-tags))))
      (if (or (and todo-is-next (not next-in-tags))
              (and (not todo-is-next) next-in-tags))
          (org-toggle-tag "next" 'on)
        (org-toggle-tag "next" 'off)
        ))))

(defun distopico:org-add-default-effort ()
  "Add a default effort estimation."
  (unless (org-entry-get (point) "Effort")
    (org-set-property "Effort" distopico:org-clock-default-effort)))


(defun distopico:org-agenda-redo-in-other-window () ;; Test
  "Call org-agenda-redo function even in the non-agenda buffer."
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))
(run-at-time nil 300 'distopico:org-agenda-redo-in-other-window)

(defun distopico:org-show-agenda (&optional open-same-window)
  "Switch to the org agenda, or prompt for new one if one does not exist"
  (interactive "P")
  (let ((agenda-buffer (get-buffer "*Org Agenda*")))
    (if agenda-buffer
        (if open-same-window
            (switch-to-buffer agenda-buffer)
          (switch-to-buffer-other-window agenda-buffer))
      (org-agenda-list))
    (tabbar-local-mode 1)))

(defun distopico:org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file.
from: http://pages.sachachua.com/.emacs.d/Sacha.html"
  (interactive "P")
  (org-agenda-todo "DONE"))

(defun distopico:org-agenda-mark-done-and-add ()
  "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings.
from: http://pages.sachachua.com/.emacs.d/Sacha.html"
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0"t"))

(defun distopico:org-agenda-new ()
  "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))

(defun distopico:org-agenda-day-face-holidays-function (date)
  "Compute DATE face for holidays."
  (unless (org-agenda-todayp date)
    (dolist (file (org-agenda-files nil 'ifmode))
      (let ((face
             (dolist (entry (org-agenda-get-day-entries file date))
               (let ((category (with-temp-buffer
                                 (insert entry)
                                 (org-get-category (point-min)))))
                 (cond ((or (string= "Holidays" category)
                            (string= "Birthday" category)
                            (string= "Vacation" category))
                        (return 'org-agenda-date-weekend))
                       ))) ))
        (when face (return face))))))

(defun distopico:org-update-agenda-views ()
  "Update all org agenda buffers (if any)."
  (save-window-excursion
    (mapc
     (lambda (buf)
       (with-current-buffer buf
         (org-agenda-redo t)
         ))
     (get-buffers-with-major-mode 'org-agenda-mode))) )

(defun distopico:remove-empty-drawer-on-clock-out ()
  "Delete clocking drawer if it is empty."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":LOGBOOK:" nil t) ;
      (save-excursion
        (org-remove-empty-drawer-at (match-beginning 0))))
    (while (re-search-forward ":CLOCK:" nil t) ;
      (save-excursion
        (org-remove-empty-drawer-at (match-beginning 0))))))

(defun distopico:clock-in-to-next (state) ;
  "Switch task from TODO to NEXT when clocking in.
Skips capture tasks and tasks with subtasks"
  (if (and (string-equal state "TODO")
           (not (or (string-equal "*Remember*" (buffer-name))
                    (string-prefix-p "CAPTURE-" (buffer-name)))))

      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (has-subtask nil))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-not-done-keywords)
              (setq has-subtask t))))
        (when (not has-subtask)
          "NEXT"))))

(defun distopico:org-clock-in-set-state-to-started ()
  "Mark STARTED when clocked in."
  (save-excursion
    (catch 'exit
      (cond
       ((derived-mode-p 'org-agenda-mode)
        (let* ((marker (or (org-get-at-bol 'org-marker)
                           (org-agenda-error)))
               (hdmarker (or (org-get-at-bol 'org-hd-marker) marker))
               (pos (marker-position marker))
               (col (current-column))
               newhead)
          (org-with-remote-undo (marker-buffer marker)
            (with-current-buffer (marker-buffer marker)
              (widen)
              (goto-char pos)
              (org-back-to-heading t)
              (if (org-get-todo-state)
                  (org-todo "STARTED"))))))
       (t (if (org-get-todo-state)
              (org-todo "STARTED")))))))

(defun distopico:org-insert-heading-for-next-day ()
  "Insert a same-level heading for the following day.
from: http://pages.sachachua.com/.emacs.d/Sacha.html"
  (interactive)
  (let ((new-date
         (seconds-to-time
          (+ 86400.0
             (float-time
              (org-read-date nil 'to-time (elt (org-heading-components) 4)))))))
    (org-insert-heading-after-current)
    (insert (format-time-string "%Y-%m-%d\n" new-date))))

(defun distopico:org-capture-refile-and-jump ()
  (interactive)
  (org-capture-refile)
  (org-refile-goto-last-stored))

(defun distopico:org-tree-to-indirect-buffer-renamed (subname)
  "Like org-tree-to-indirect-buffer, with the option to give a \"subname\"
from: https://github.com/cwebber/cwebbers-emacs-config/blob/master/modes/org.el"
  (interactive "sNew buffer subname?: ")
  (let ((orig-buffer-name (buffer-name (current-buffer))))
    (org-tree-to-indirect-buffer)
    (other-window 1)
    (if (not (equal subname ""))
        (rename-buffer (format "%s(%s)" orig-buffer-name subname)))))

(defun distopico:org-inherited-no-file-tags ()
  "preserves the logic of level one groupings."
  (let ((tags (org-entry-get nil "ALLTAGS" 'selective))
        (ltags (org-entry-get nil "TAGS")))
    (mapc (lambda (tag)
            (setq tags
                  (replace-regexp-in-string (concat tag ":") "" tags)))
          (append org-file-tags (when ltags (split-string ltags ":" t))))
    (if (string= ":" tags) nil tags)))

(defadvice org-archive-subtree (around distopico:org-archive-subtree-low-level activate)
  "Preserve top level headings when archiving to a file."
  (let ((tags (distopico:org-inherited-no-file-tags))
        (org-archive-location
         (if (save-excursion (org-back-to-heading)
                             (> (org-outline-level) 1))
             (concat (car (split-string org-archive-location "::"))
                     "::* "
                     (car (org-get-outline-path)))
           org-archive-location)))
    ad-do-it
    (with-current-buffer (find-file-noselect (org-extract-archive-file))
      (save-excursion
        (while (org-up-heading-safe))
        (org-set-tags-to tags)))))

(defun distopico:org-ask-location ()
  "Ask targget location"
  (let* ((org-refile-targets '((nil :maxlevel . 9)))
         (hd (condition-case nil
                 (car (org-refile-get-location nil nil t t))
               (error (car org-refile-history)))))
    (goto-char (point-min))
    (outline-next-heading)
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd))
         nil t)
        (goto-char (point-at-bol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "")))
  (end-of-line))

(defun distopico:org-update-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun distopico:org-run-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt)
  (appt-activate t))

(defun distopico:org-appt-add-hook-async ()
  (async-start
   `(lambda ()
      (require 'org-agenda)
      ,(add-hook 'after-init-hook
                 `(lambda ()
                    ,(setq appt-time-msg-list nil)
                    ,(org-agenda-to-appt)
                    ,(appt-activate t)
                    ))) 'ignore))

(defun distopico:org-add-appt ()
  (interactive)
  (org-set-property
   "APPOINTMENT"
   (concat "<" (org-read-date t) ">")))

(defun distopico:org-reset-appts ()
  "This also reverts all files, but it does update the appt list"
  (interactive)
  (setq appt-time-msg-list nil)
  (cl-flet ((yes-or-no-p (x) t))
    (org-revert-all-org-buffers))
  (org-agenda-to-appt))

(defun distopico:open-diary ()
  "Open diary file with popwin and prevent run diary-fancy-mode"
  (interactive)
  (if (file-regular-p diary-file)
      (progn
        (find-file-noselect diary-file)
        (message "Opening diary...")
        (popwin:display-buffer-1
         (or (get-buffer "diary"))
         :default-config-keywords '(:position :bottom :height 10 :stick t))
        )
    (message "Diary file no exist")))

;; ----------


(distopico:org-run-appt) ;; Run Appt

(provide 'conf-org)
