;;; Code:

;; Path
(load-file "~/.emacs.d/conf/path.el")

;; Packages
(require 'setup-package)
(require 'setup-elget)

;;Theme
(require 'base-color)
(require 'conf-theme)

;; Load up the general config
(require 'conf-init)
(require 'general)
(require 'shortcuts)
(require 'conf-fringe)

;; Autocomplete
(require 'conf-autocomplete)

;;Flycheck
(require 'conf-flycheck)

;; Line Numeber
(require 'conf-linum)

;; Auto Pair
;;(require 'conf-autopair)
;;(autopair-global-mode 0)
;;(require 'conf-smartparens)
(electric-pair-mode 1)
(global-aggressive-indent-mode 1)
(dolist (source '(diary-mode css-mode less-css-mode))
  (add-to-list 'aggressive-indent-excluded-modes source t))

;; Load custom mode line
(require 'conf-powerline)

;; Colour theme and other gui related config
(require 'gui-config)

;;Color shell text
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Tree Directory
(require 'conf-nav)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))


;; Useful libs
(require 'expand-region)
(require 'jump-char)
(require 'smart-forward)
(require 'delsel)

;;(require 'conf-ideskel) I try to living without ideskel,
;; this project is abandoned and have multiples bugs with emacs 24 and others libs

;; Conf Modes
(require 'conf-mu4e)
(require 'conf-jabber)
(require 'conf-erc)
(require 'conf-gnusocial)
(load-file "~/.emacs.d/emacs/modes/column-marker.el")
(load-file "~/.emacs.d/emacs/modes/eshell.el")
(load-file "~/.emacs.d/emacs/modes/longlines.el")
(load-file "~/.emacs.d/emacs/modes/mozrepl.el")
(load-file "~/.emacs.d/emacs/modes/yasnippet.el")
;;(load-file "~/.emacs.d/emacs/modes/php.el")
(load-file "~/.emacs.d/emacs/modes/tramp.el")
(load-file "~/.emacs.d/emacs/modes/visual-regexp.el")
(load-file "~/.emacs.d/emacs/modes/wordcount.el")
(load-file "~/.emacs.d/emacs/modes/uniquify.el")
(load-file "~/.emacs.d/emacs/modes/ediff.el")
(load-file "~/.emacs.d/emacs/modes/highlight-parentheses.el")
;;(load-file "~/.emacs.d/emacs/modes/highlight-sexps.el")
(load-file "~/.emacs.d/emacs/modes/multiple-cursors.el")
(require 'conf-avy)
(require 'conf-magit)
(require 'conf-web-mode)
(require 'conf-javascript)
(require 'conf-web-beautify)
(require 'conf-emmet)
(require 'conf-skewer)
(require 'conf-css)
(require 'conf-ido)
(require 'conf-smex)
(require 'conf-pomodoro)
(require 'conf-org)
(require 'conf-present)
(require 'conf-markdown)
(require 'conf-rst)
(require 'conf-tabbar)
(require 'conf-popwin)
(require 'conf-jedi)
(require 'conf-hippie)
(require 'conf-buffer-management)
(require 'conf-file-management)
;;(require 'conf-helm)
(require 'conf-dev-utils)
(require 'conf-projectile)
(require 'conf-ibuffer)
(require 'conf-dired)
(require 'buffer-move)
(require 'conf-python)
(require 'conf-auto-indent)
(require 'conf-hideshow)
;;(require 'conf-paredit)
(require 'conf-isearch)
(require 'conf-hideshowvis)
(require 'conf-highlight-symbol)
(require 'conf-rainbow)
(require 'conf-highlight-indentation)
(require 'conf-guide-key)
(require 'conf-json)
;;(require 'conf-eclim)

;; Test
(require 'change-inner)

;;TODO
;; Warning (initialization): Your `load-path' seems to contain
;; your `.emacs.d' directory: ~/.emacs.d/
;; This is likely to cause problems...
;; Consider using a subdirectory instead, e.g.: /home/distopico/.emacs.d/lisp
