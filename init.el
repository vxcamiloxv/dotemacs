(require 'cl)
(require 'cl-lib)

(load-file "~/.emacs.d/conf/path.el")

(require 'conf-theme)
(require 'base-color)

;; Packages
(require 'setup-package)
(require 'setup-elget)


;; Load up the general config
(require 'general)
(require 'conf-init)
(require 'shortcuts)
(require 'conf-fringe)

;; Python IDE
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))
(add-hook 'python-mode-hook 'auto-complete-mode)
(setq py-smart-indentation t)

;(package-initialize)
;(require 'elpy)
;(elpy-enable)

;(load-file "~/.emacs.d/modes/emacs-for-python/epy-init.el")

(require 'python-django)
(require 'pony-mode)

(require 'python-pep8)
(require 'python-pylint)

;; Auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories  "~/.emacs.d/ac-dict")
(autoload 'auto-complete-mode "auto-complete" t)
(ac-config-default)
(setq ac-delay 0.2)
(setq ac-auto-show-menu 0.3)

;;Flycheck
;(require 'conf-flycheck)

;; Line Numeber
(require 'conf-linum)

;; Auto Pair
;(require 'conf-autopair)
(autopair-global-mode 0)
(require 'conf-smartparens)

;; Install a custom mode line
;(require 'conf-powerline)
(load-file "~/.emacs.d/emacs/mode-line.el")

;; Finalize things
;; (load-file "~/.emacs.d/emacs/finalize.el")

;; Colour theme and other gui related config
(load-file "~/.emacs.d/conf/gui-config.el")

;;Color shell text
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Tree Directory
(require 'nav)
 ;;(nav-disable-overeager-window-splitting)

(autoload 'dirtree "dirtree" "Add directory to tree view")
(defun ep-dirtree ()
  (interactive)
    (dirtree-in-buffer eproject-root t))

;; Sr SpeedBar
;(load-file "~/.emacs.d/modes/sr-speedbar.el")
;(require 'sr-speedbar)

;; Aditionals Modes
(load-file "~/.emacs.d/emacs/modes/column-marker.el")
(load-file "~/.emacs.d/emacs/modes/css.el")
(load-file "~/.emacs.d/emacs/modes/erc.el")
(load-file "~/.emacs.d/emacs/modes/eshell.el")
;(load-file "~/.emacs.d/emacs/modes/flymake.el")
;(load-file "~/.emacs.d/emacs/modes/identica.el")
(load-file "~/.emacs.d/emacs/modes/longlines.el")
(load-file "~/.emacs.d/emacs/modes/magit.el")
(load-file "~/.emacs.d/emacs/modes/mozrepl.el")
(load-file "~/.emacs.d/emacs/modes/yasnippet.el")
;(load-file "~/.emacs.d/emacs/modes/org.el")
(load-file "~/.emacs.d/emacs/modes/php.el")
(load-file "~/.emacs.d/emacs/modes/tramp.el")
(load-file "~/.emacs.d/emacs/modes/visual-regexp.el")
(load-file "~/.emacs.d/emacs/modes/wordcount.el")
(load-file "~/.emacs.d/emacs/modes/ido.el")
(load-file "~/.emacs.d/emacs/modes/smex.el")
(load-file "~/.emacs.d/emacs/modes/uniquify.el")
(load-file "~/.emacs.d/emacs/modes/ediff.el")
(load-file "~/.emacs.d/emacs/modes/ace-jump.el")
(load-file "~/.emacs.d/emacs/modes/web-mode.el")
(load-file "~/.emacs.d/emacs/modes/highlight-parentheses.el")
(load-file "~/.emacs.d/emacs/modes/highlight-sexps.el")
(load-file "~/.emacs.d/emacs/modes/paredit.el")
(load-file "~/.emacs.d/emacs/modes/multiple-cursors.el")
(require 'conf-tabbar)
(require 'conf-ideskel)
(require 'conf-popwin)
(require 'conf-jedi)
(require 'conf-buffer-management)
(require 'conf-file-management)
;(require 'conf-helm)
(require 'conf-skewer)
(require 'conf-projectile)
(require 'conf-ibuffer)
(require 'conf-dired)
(require 'buffer-move)
(require 'conf-python)
(require 'conf-auto-indent)
;(require 'conf-hideshow)
(require 'conf-paredit)
(require 'conf-perspective)
(require 'conf-isearch)
(require 'conf-hideshowvis)
(require 'conf-highlight-symbol)

;; Test
(require 'expand-region)
(require 'delsel)
(require 'jump-char)
;(require 'eproject)
(require 'smart-forward)
(require 'change-inner)
;(require 'multifiles)

;; Emacs IDE
;(add-to-list 'load-path "~/.emacs.d/site-lisp/")
;(require 'eide)
;(eide-start)
