;;; Code:
;; ------
;; Require misc stuff
;; ------
(require 'async)
(require 'epa-file)
(require 'ibuffer)

(defvar backup-dir (in-emacs-d ".cache/backup/")
  "Place backups in `~/.backups/' directory, like a civilized program")
(defvar distopico:after-init-load-hook nil
  "Hook called after the `init.el' load all required dependencies")

;;Keep cache
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; Enable async byte-compile
(async-bytecomp-package-mode t)

;; Create directory if no exist
(when (and (not (file-directory-p backup-dir)))
  (make-directory backup-dir t))

;; Defined backup directory
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix backup-dir)
;;(setq auto-save-file-name-transforms '((".*" ,backup-dir t)))


(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 3    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Delete old backups
(if (file-directory-p backup-dir)
    (message "Deleting old backup files...")
  (let ((week (* 60 60 24 7))
        (current (float-time (current-time))))
    (dolist (file (directory-files backup-dir t))
      (when (and (backup-file-name-p file)
                 (> (- current (float-time (fifth (file-attributes file))))
                    week))
        (message "%s" file)
        (delete-file file))))
  )


;; ---------
;; Make debian/ubuntu work nicely with cvs emacs
;; ---------
;; (let ((startup-file "/usr/share/emacs/site-lisp/debian-startup.el"))
;;   (if (and (or (not (fboundp 'debian-startup))
;;                (not (boundp  'debian-emacs-flavor)))
;;            (file-readable-p startup-file))
;;       (progn
;;         (load-file startup-file)
;;         (setq debian-emacs-flavor 'emacs22)
;;         (debian-startup debian-emacs-flavor)
;;         (mapcar '(lambda (f)
;;                    (and (not (string= (substring f -3) "/.."))
;;                         (file-directory-p f)
;;                         (add-to-list 'load-path f)))
;;                 (directory-files "/usr/share/emacs/site-lisp" t)))))

;; ------
;; General config BS
;; ------

(setq fill-column 79
      pop-up-frames nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq max-specpdl-size 9000)

(setq font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;;Delete trailing space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Resaltado linea
(global-hl-line-mode t)

;; Semantic | CEDET
;;(semantic-mode 1)
;;(global-semantic-idle-completions-mode t)
;;(global-semantic-decoration-mode t)
;;(global-semantic-highlight-func-mode t)
;;(global-semantic-show-unmatched-syntax-mode t)

;; winner mode
(winner-mode 1)

;;(set-window-dedicated-p (selected-window) t)

(when (not window-system)
  ;;allow you to see the region when in console mode
  (setq transient-mark-mode t))

;; Shell stuff
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")

(setq ispell-alternate-dictionary "/etc/dictionaries-common/words")

(setq tex-dvi-view-command
      (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))

                                        ; tab auto-completion cycling is evil.
(setq pcomplete-cycle-completions nil)

;; Make sure that pressing middle mouse button pastes right at point,
;; not where the mouse cursor is.
(setq mouse-yank-at-point t)

;; Don't show my password when I'm entering it, kthx.
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)


(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq inhibit-splash-screen t)

(setq visual-line-fringe-indicators '(t t))

;; Don't switch to another frame with iswitchb
;;(setq iswitchb-default-method 'samewindow)

;; Use diff -u

(setq diff-switches "-u")


;; ------
;; Initialize some things
;; ------

;; (display-time)
(server-start)

;; Mouse scrolling
(mwheel-install)

;; Don't minimize my emacs! Honestly wtf
(when window-system
  (progn
    (setq scroll-bar-mode nil)
    (tool-bar-mode nil)
    (menu-bar-mode nil)))


;; ---------
;; Custom funcs
;; ---------

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(defun warn-if-symlink ()
  (if (file-symlink-p buffer-file-name)
                                        ;progn here to execute both as part of else statement together
      (message "File is a symlink")))

(add-hook 'find-file-hooks 'warn-if-symlink)

;; Highlight previous (or current?) line
(defun distopico:pg-uline (ulinechar)
  "Underline the current or the previous line with ULINECHAR"
  (interactive "cUnderline with:")
  (if (looking-at "^$")
      (next-line -1))
  (end-of-line)
  (let ((linelen (current-column)))
    (insert "\n")
    (while (> linelen 0)
      (setq linelen (1- linelen))
      (insert ulinechar)))
  (insert "\n"))

;; Swapping buffers!  Can't live without this

(setq cwebber/swapping-buffer nil)
(setq cwebber/swapping-window nil)

(defun cwebber/swap-buffers-in-windows ()
  "Swap buffers between two windows"
  (interactive)
  (if (and cwebber/swapping-window
           cwebber/swapping-buffer)
      (let ((this-buffer (current-buffer))
            (this-window (selected-window)))
        (if (and (window-live-p cwebber/swapping-window)
                 (buffer-live-p cwebber/swapping-buffer))
            (progn (switch-to-buffer cwebber/swapping-buffer)
                   (select-window cwebber/swapping-window)
                   (switch-to-buffer this-buffer)
                   (select-window this-window)
                   (message "Swapped buffers."))
          (message "Old buffer/window killed.  Aborting."))
        (setq cwebber/swapping-buffer nil)
        (setq cwebber/swapping-window nil))
    (progn
      (setq cwebber/swapping-buffer (current-buffer))
      (setq cwebber/swapping-window (selected-window))
      (message "Buffer and window marked for swapping."))))


;; other stuff
(defun add-spaces-to-region (beginning end numspaces)
  "Add spaces to a whole region of text"
  (interactive "r\nnNumber of spaces: ")
  (save-excursion
    (goto-char beginning)
    (beginning-of-line)
    (while (< (point) end)
      (let ((bol-point 0)
            (eol-point 0))
        (save-excursion
          (end-of-line)
          (setq eol-point (point))
          (beginning-of-line)
          (setq bol-point (point)))
        (if (not (equal bol-point eol-point))
            (progn
              (beginning-of-line)
              (dotimes (i numspaces)
                (insert " ")))))
      (forward-line)
      (beginning-of-line))))

(defun rename-buffer-with-directory (&optional arg)
  "Useful for when you're dealing with multiple files with the
  same name in different directories.  No more file.txt<2>!

  Running this will append the previous directory to the end of
  the filename.  So for example, if you opened the emacs
  ChangeLog (living in the emacs/ directory), you'll get
  'ChangeLog(emacs/)'.  Using a prefix arg will give you number
  of subdirectories, if you need it.

  If you are accessing a file over Tramp, it will add 'host:' to
  the parenthesis.. so if you were accessing
  /ssh:example.org:/home/foobar/emacs/ChangeLog, you'd get:
  'ChangeLog(example.org:emacs/)'"
  (interactive "^p")
  (let ((dir-name nil)
        (split-path (eshell-split-path
                     (file-name-directory (buffer-file-name))))
        (tramp-data (condition-case nil
                        (tramp-dissect-file-name (buffer-file-name))
                      (error nil))))
    (setq dir-name
          (apply 'concat (nthcdr (- (length split-path) arg) split-path)))
    (rename-buffer
     (concat (file-name-nondirectory (buffer-file-name))
             (if tramp-data
                 (concat
                  "(" (aref tramp-data 2) ":" dir-name ")")
               (concat "(" dir-name ")"))))))


(defun insert-virtualenv-load-line (virtualenv-dir)
  (interactive "DVirtualenv directory: ")
  (let ((activate-file (expand-file-name (concat virtualenv-dir "./bin/activate_this.py"))))
    (if (file-exists-p activate-file)
        (insert
         (concat "execfile('" activate-file
                 "', dict(__file__='" activate-file "'))"))
      (error "No ./bin/activate_this.py in that virtualenv (maybe update virtualenv?)"))))


;; En1arg3 y0ur w1|\|dow!!!
(defun undo-or-shrink-horizontally ()
  "Either undo or shrink horizontally, depending on whether we're
in X or in a terminal"
  (interactive)
  (if (window-system)
      (shrink-window-horizontally)
    (undo)))

;; Apparently I'm a crufty old timer who likes the way the old mouse
;; and x11 pasting worked.  This sets it back
(setq mouse-drag-copy-region nil
      select-active-regions nil
      x-select-enable-primary t
      x-select-enable-clipboard t)

;; ---------
;; Load some custom stuff
;; ---------

;;Turn off tool-bar-mode, which is slow
(call-interactively 'tool-bar-mode)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


                                        ; lock files borking git-annex-assistant autocommits.  Disabling for now.
;; We can re-enable both of these once .gitignore support comes to git-annex
(setq create-lockfiles nil)
                                        ; Similarly with auto-save files :(
(setq auto-save-default nil)

;; Undo Redo
;;(load-file "~/.emacs.d/modes/undo-tree.el")
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Shutdown Server
;; define function to shutdown emacs server instance
(defun shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Transparently open compressed files
(auto-compression-mode t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Auto refresh buffers
(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Remove .elc in save
                                        ;(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)


;; ignore byte-compile warnings
(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete
                                  noruntime
                                  cl-functions
                                  interactive-only))

(provide 'setup-general)