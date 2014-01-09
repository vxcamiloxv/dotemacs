;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; navigate thru windows using M-<arrow>
(windmove-default-keybindings 'meta)

;; Context Menu
(load-file "~/.emacs.d/modes/context-menu.el")

;; Tabs | this has impact in performance
(require 'conf-tabbar)

;; Smooth Scroll
(setq scroll-step 1
      scroll-conservatively 1000)

;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time 
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse    
         
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "UTF-8")
 '(display-time-mode t)
 ;;'(custom-enabled-themes (quote (tango-dark)))
 '(flycheck-highlighting-mode (quote lines))
 '(show-paren-mode 1)
 '(size-indication-mode t)
 ;'(tool-bar-mode nil)
 ;'(column-number-mode t)
 '(inhibit-startup-screen t)
 '(iswitchb-mode t)
 '(savehist-mode t nil (savehist))
 '(transient-mark-mode t)
 '(truncate-lines t)
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
