;;; Code:
(require 'zoom-window)
(require 'windresize)
(require 'zoom-frm) ;; Make zooming affect frame instead of buffers
(require 'scroll-bell-fix)
(require 'fill-column-indicator)
(require 'highlight-escape-sequences)
(require 'browse-kill-ring)

;;; Font
(add-to-list 'default-frame-alist
             '(font . "Hack-10.5"))
(setq-default line-spacing 1)

;;; Disabled or enabled debug:
(setq debug-on-error nil
      debug-on-signal nil
      debug-on-quit nil)

;; Initial buffer
(setq initial-major-mode 'org-mode
      initial-scratch-message (purecopy "\
# Scratch!
# --------
# This buffer is for notes you don't want to save, etc.
# If you want to create a file, visit that file with C-x C-f."))

;; Custom action for scratch buffer
(with-current-buffer "*scratch*"
  (if (not (eq major-mode initial-major-mode))
      (funcall initial-major-mode))
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

;; Smoother scrolling (no multiline jumps.)
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 1)
;; Mouse scrolling
(if (load "mwheel" t)
    (mwheel-install))

;; Don't minimize my emacs!
(setq scroll-bar-mode nil)
;; Turn off tool-bar-mode
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Horizontal scroll
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

(if (boundp 'truncate-lines)
    (setq-default truncate-lines t) ; always truncate
  (progn
    (hscroll-global-mode t)
    (setq hscroll-margin t)
    (setq hscroll-step t)
    (setq auto-hscroll-mode t)
    (setq automatic-hscrolling t)))

(custom-set-variables
 '(current-language-environment "UTF-8")
 '(system-time-locale "C")
 '(column-number-mode t)
 '(ediff-custom-diff-program "diff")
 '(ediff-diff-program "diff")
 '(ediff-diff3-program "diff3")
 '(indicate-buffer-boundaries (quote right))
 '(linum-delay nil)
 '(linum-eager t)
 '(sml-modeline-mode 1)
 '(window-left-margin 0)
 '(display-time-mode t)
 '(flycheck-highlighting-mode (quote lines))
 '(show-paren-mode 1)
 '(size-indication-mode t)
 '(inhibit-startup-screen t)
 '(cursor-type 'bar)
 '(blink-cursor-mode t)
 '(savehist-mode t nil (savehist))
 '(savehist-file (in-emacs-d ".cache/history"))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(windmove-wrap-around t)
 '(enable-local-variables :all))

;; hostname and buffer-name in frame title
(setq-default frame-title-format
              '(:eval
                (if (string-match-p "^\\*.+\\*$" (buffer-name)) "%b" ; buffer name
                  (format "%s:%s"
                          (or (file-remote-p default-directory 'host) system-name)
                          (buffer-name)))))
                                        ;(format "%s@%s:%s"
                                        ;      (or (file-remote-p default-directory 'user) user-login-name)

                                        ;(setq frame-title-format '("%b %I %+%@%t%Z %m %n %e"))

;;-----------------------------------------------------------------
;; Default Indentation
;;-----------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default sgml-basic-offset 4)
(setq-default css-indent-offset 4)
(setq-default nxml-child-indent 4)
(setq-default py-indent-offset 4)
(setq-default tab-stop-list (number-sequence 4 120 4))

;; Highlight
(hes-mode) ;; escape sequences
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)
(global-hl-line-mode t) ;; line

;; Windows
(setq zoom-window-mode-line-color (face-background 'mode-line))
(winner-mode 1)

;; allow "restricted" features
(put 'set-goal-column 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(put 'scroll-left 'disabled nil)

;; Browse kill ring
(setq browse-kill-ring-quit-action 'save-and-restore)
(browse-kill-ring-default-keybindings)

;; Whitespace style
(setq whitespace-style '(trailing underline spaces tabs newline space-mark tab-mark newline-mark))


;; Functions
(defun kill-scratch-buffer ()
  "If the *scratch* buffer is killed, recreate it automatically.
FROM: Morten Welind: http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/"
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (funcall initial-major-mode)
  (insert initial-scratch-message)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

(defun copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2)) ) )

(defun cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2)) ) )

(defun toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )

(defun ignore-error-wrapper (fn)
  "FN return new function that ignore errors.
The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

;; Modified default
(defadvice kill-ring-save (after keep-transient-mark-active ())
  "Override the deactivation of the mark."
  (setq deactivate-mark nil))
(ad-activate 'kill-ring-save)

;; Adjust Margin
(defadvice indent-rigidly (after deactivate-mark-nil activate)
  (if (called-interactively-p)
      (setq deactivate-mark nil)))

(provide 'setup-gui)
