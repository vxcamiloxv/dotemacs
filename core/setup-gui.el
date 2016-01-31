;; Context Menu
(require 'context-menu)
(require 'zoom-window)
(require 'windresize)

;; Smoother scrolling (no multiline jumps.)
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 1)


;; Initial buffer
(setq initial-major-mode 'org-mode
      initial-scratch-message (purecopy "\
# Scratch!
# --------
# This buffer is for notes you don't want to save, etc.
# If you want to create a file, visit that file with C-x C-f."))

(with-current-buffer "*scratch*"
  (if (not (eq major-mode initial-major-mode))
      (funcall initial-major-mode)))

;; If the *scratch* buffer is killed, recreate it automatically
;; FROM: Morten Welind: http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (funcall initial-major-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
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

(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 1))

(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 1))

;;(setq scroll-step 1)
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

;; Enable CEDET
;;(semantic-mode t) ;; Disable or bug with menu-bar-mode
;;(global-semantic-idle-completions-mode)
;;(global-semantic-decoration-mode)
;;(global-semantic-highlight-func-mode)
;;(global-semantic-show-unmatched-syntax-mode)

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

;; Keep cursor away from edges when scrolling up/down
                                        ;(require 'smooth-scrolling)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "UTF-8")
 '(system-time-locale "C")
 ;;'(Linum-format "%7i ")
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(custom-safe-themes t)
 '(ediff-custom-diff-program "diff")
 '(ediff-diff-program "diff")
 '(ediff-diff3-program "diff3")
 '(fci-rule-character-color "#202")
 '(fci-rule-color "#202")
 '(indicate-buffer-boundaries (quote right))
 '(js-indent-level 2)
 '(js3-enter-indents-newline t)
 '(js3-indent-level 2)
 '(jshint-configuration-path "~/.jshintrc")
 '(linum-delay nil)
 '(linum-eager t)
 ;;'(visible-bell t)
 '(sml-modeline-mode 1)
 '(window-left-margin 0)
 '(display-time-mode t)
 '(flycheck-highlighting-mode (quote lines))
 '(show-paren-mode 1)
 '(size-indication-mode t)
 '(inhibit-startup-screen t)
 '(cursor-type 'bar)
 '(blink-cursor-mode t)
 ;;'(iswitchb-mode t)
 '(savehist-mode t nil (savehist))
 '(savehist-file (in-emacs-d ".cache/history"))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(windmove-wrap-around t)
 '(enable-local-variables :all)
 ;;'(safe-local-variable-values '(engine . swig))
 )

;; In emacs24
(setq completion-styles '(partial-completion initials))
(setq completion-pcm-complete-word-inserts-delimiters t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error-face ((t (:inherit error :background "gray27" :foreground "IndianRed1" :underline (:color "red" :style wave)))))
 '(flycheck-warning-face ((t (:inherit warning :foreground "yellow1"))))
 '(completions-common-part ((t (:foreground "forest green"))))
 '(completions-first-difference ((t (:bold t :weight bold :foreground "salmon"))))
 )

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

;; window opacity utilities
(require 'nifty-defuns)
;;(mouse-avoidance-mode 'animate)

;;; Disabled or enabled debug:
(setq debug-on-error nil
      debug-on-signal nil
      debug-on-quit nil)


;; Copy/Cut/Paste Line
(defadvice kill-ring-save (after keep-transient-mark-active ())
  "Override the deactivation of the mark."
  (setq deactivate-mark nil))
(ad-activate 'kill-ring-save)

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

;; Adjust Margin
(defadvice indent-rigidly (after deactivate-mark-nil activate)
  (if (called-interactively-p)
      (setq deactivate-mark nil)))

(defun toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )

;; Ignore errors
(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

;;-----------------------------------------------------------------
;; Indentation
;;-----------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq sgml-basic-offset 4)
(setq css-indent-offset 4)
(setq nxml-child-indent 4)
(setq py-indent-offset 4)
(setq-default tab-stop-list (number-sequence 4 120 4))

;;----------------
;; MenuBar+
;;-----------------
(require 'menu-bar+)
(eval-after-load "menu-bar" '(require 'menu-bar+))

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)

;; Unclutter the modeline
(require 'diminish)
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "tagedit" '(diminish 'tagedit-mode))
(eval-after-load "skewer-mode" '(diminish 'skewer-mode))
(eval-after-load "skewer-css" '(diminish 'skewer-css-mode))
(eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))

;; Windows
(setq zoom-window-mode-line-color (face-background 'mode-line))

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

(require 'squeeze-view) ;; squeeze view, give yourself a write-room/typewriter like writing page
(require 'scroll-bell-fix)
(require 'dropdown-list)
;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)
(browse-kill-ring-default-keybindings)

;; Whitespace style
(setq whitespace-style '(trailing underline spaces tabs newline space-mark tab-mark newline-mark))

;; icomplete useful
(icomplete-mode)

(provide 'setup-gui)
