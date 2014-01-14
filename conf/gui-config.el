;; navigate thru windows using M-<arrow>
(windmove-default-keybindings 'meta)

;; Context Menu
(load-file "~/.emacs.d/modes/context-menu.el")

;; Smooth Scroll

;; Smoother scrolling (no multiline jumps.)
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)


(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 1))

(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 1))

(global-set-key [next] 'gcm-scroll-down)
(global-set-key [prior]  'gcm-scroll-up)

(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

                  
;, Horizontal scroll 
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

(if (boundp 'truncate-lines)
    (setq-default truncate-lines t) ; always truncate
  (progn
    (hscroll-global-mode t)
    (setq hscroll-margin 1)
    (setq hscroll-step 1)
    (setq auto-hscroll-mode 1)
    (setq automatic-hscrolling t)
   ))

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "UTF-8")
 ;'(Linum-format "%7i ") 
 '(column-number-mode t)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
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
 ;'(visible-bell t)
 '(sml-modeline-mode 1)
 '(window-left-margin 0)  
 '(display-time-mode t)
 '(flycheck-highlighting-mode (quote lines))
 '(show-paren-mode 1)
 '(size-indication-mode t)
 '(inhibit-startup-screen t)
 '(iswitchb-mode t)
 '(savehist-mode t nil (savehist))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(windmove-wrap-around t)
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error-face ((t (:inherit error :background "gray27" :foreground "IndianRed1" :underline (:color "red" :style wave)))))
 '(flycheck-warning-face ((t (:inherit warning :foreground "yellow1"))))
 '(completions-common-part ((t (:foreground "forest green"))))
 '(completions-first-difference ((t (:bold t :weight bold :foreground "salmon"))))
 '(minibuffer-prompt ((t (:foreground "dark cyan" :weight bold))))
 )

;; hostname and buffer-name in frame title
(setq-default frame-title-format
         '(:eval
                (if (string-match-p "^\\*.+\\*$" (buffer-name)) "%b" ; buffer name
                 (format "%s:%s"
                         (or (file-remote-p default-directory 'host) system-name)
                         (buffer-name)))))
                  ;(format "%s@%s:%s"
                  ;        (or (file-remote-p default-directory 'user) user-login-name)

;(setq frame-title-format '("%b %I %+%@%t%Z %m %n %e"))

;; window opacity utilities
(load-file "~/.emacs.d/conf/nifty.el")
(require 'nifty)


;;; Code:
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)


;; Copy/Cut/Paste Line

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
(defun toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )


;-----------------------------------------------------------------
; Indentation
;-----------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq default-tab-indent 4)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq sgml-basic-offset 4)
(setq css-indent-offset 4)
(setq nxml-child-indent 4)

;----------------
; MenuBar+
;-----------------
(load-file "~/.emacs.d/modes/menu-bar+.el")
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
(eval-after-load "guide-key" '(diminish 'guide-key-mode))

;; Windows Number
(require 'window-number)
(window-number-mode)

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
