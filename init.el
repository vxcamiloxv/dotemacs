;;; package --- Init Emacs
;;;
;;; Commentary:
;;; Init and load my configuration for Emacs
;;;
;;; Code:

;; Path
(load-file "~/.emacs.d/config/path.el")

;; Packages
(require 'setup-package)
(require 'setup-elget)

;;Theme
(require 'base-color)
(require 'conf-theme)

;; Load up the general config
(require 'general)
(require 'conf-init)
(require 'shortcuts)
(require 'hooks)
(require 'conf-fringe)

;;Flycheck
(require 'conf-flycheck)

;; Line Numeber
(require 'conf-linum)

;; Auto Pair
;(require 'conf-autopair)
;(autopair-global-mode 0)
(require 'conf-smartparens)

;; Install a custom mode line
(require 'conf-powerline)

;; Finalize things
;; (load-file "~/.emacs.d/emacs/finalize.el")

;; Colour theme and other gui related config
(require 'gui-config)

;;Color shell text
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Tree Directory
(require 'nav)
 ;;(nav-disable-overeager-window-splitting)

(autoload 'dirtree "dirtree" "Add directory to tree view")
(defun ep-dirtree ()
  (interactive)
    (dirtree-in-buffer eproject-root t))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Load Modes
(require 'conf-python)
;; (load-file "~/.emacs.d/emacs/modes/column-marker.el")
;; (load-file "~/.emacs.d/emacs/modes/css.el")
;; (load-file "~/.emacs.d/emacs/modes/erc.el")
;; (load-file "~/.emacs.d/emacs/modes/eshell.el")
;; ;(load-file "~/.emacs.d/emacs/modes/flymake.el")
;; ;(load-file "~/.emacs.d/emacs/modes/identica.el")
;; (load-file "~/.emacs.d/emacs/modes/longlines.el")
;; (load-file "~/.emacs.d/emacs/modes/magit.el")
;; (load-file "~/.emacs.d/emacs/modes/mozrepl.el")
;; (load-file "~/.emacs.d/emacs/modes/yasnippet.el")
;; ;(load-file "~/.emacs.d/emacs/modes/org.el")
;; ;(load-file "~/.emacs.d/emacs/modes/php.el")
;; (load-file "~/.emacs.d/emacs/modes/tramp.el")
;; (load-file "~/.emacs.d/emacs/modes/visual-regexp.el")
;; (load-file "~/.emacs.d/emacs/modes/wordcount.el")
;; (load-file "~/.emacs.d/emacs/modes/ido.el")
;; (load-file "~/.emacs.d/emacs/modes/smex.el")
;; (load-file "~/.emacs.d/emacs/modes/uniquify.el")
;; (load-file "~/.emacs.d/emacs/modes/ediff.el")
;; (load-file "~/.emacs.d/emacs/modes/ace-jump.el")
;; (load-file "~/.emacs.d/emacs/modes/web-mode.el")
;; (load-file "~/.emacs.d/emacs/modes/highlight-parentheses.el")
;; ;(load-file "~/.emacs.d/emacs/modes/highlight-sexps.el")
;; (load-file "~/.emacs.d/emacs/modes/multiple-cursors.el")
;; (require 'conf-tabbar)
;; (require 'conf-ideskel)
;; (require 'conf-popwin)
;; (require 'conf-jedi)
;; (require 'conf-buffer-management)
;; (require 'conf-file-management)
;; ;(require 'conf-helm)
;; (require 'conf-skewer)
;; (require 'conf-projectile)
;; (require 'conf-ibuffer)
;; (require 'conf-dired)
;; (require 'buffer-move)
;; (require 'conf-python)
;; (require 'conf-auto-indent)
;; (require 'conf-hideshow)
;; (require 'conf-paredit)
;; (require 'conf-perspective)
;; (require 'conf-isearch)
;; (require 'conf-hideshowvis)
;; (require 'conf-highlight-symbol)
;; ;(require 'conf-mode-line)
;; (require 'conf-highlight-indentation)
;; (require 'conf-json)

;; Test
;; (require 'expand-region)
;; (require 'delsel)
;; (require 'jump-char)
;; ;(require 'eproject)
;; (require 'smart-forward)
;; (require 'change-inner)
;; (require 'tabkey2)
;; ;(require 'multifiles)

;; fixme mode
(require 'fixme-mode)
(fixme-mode)
