;; Path
(load-file "~/.emacs.d/conf/path.el")

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
(require 'conf-fringe)

;; Auto-complete
(require 'auto-complete)
(global-auto-complete-mode)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories  "~/.emacs.d/ac-dict")
(ac-config-default)
(ac-linum-workaround)
;(setq ac-delay 0.2)
;(setq ac-auto-show-menu 0.3)
(setq ac-use-fuzzy t)

(defvar distopico-ac-default
   '(ac-source-yasnippet
     ac-source-features
     ;ac-source-abbrev
     ac-source-dictionary
     ac-source-imenu
     ;ac-source-gtags
     ;ac-source-semantic
     ac-source-words-in-same-mode-buffers
     ac-source-words-in-buffer
     ac-source-files-in-current-dir
))
(setq-default ac-sources distopico-ac-default)


;; Django test

;(require 'python-django)
(require 'pony-mode)
;(add-to-list 'auto-mode-alist '("\\.dtpl$" . pony-tpl-mode))

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

;; Aditionals Modes
(load-file "~/.emacs.d/emacs/modes/column-marker.el")
(load-file "~/.emacs.d/emacs/modes/css.el")
(load-file "~/.emacs.d/emacs/modes/erc.el")
(load-file "~/.emacs.d/emacs/modes/eshell.el")
(load-file "~/.emacs.d/emacs/modes/longlines.el")
(load-file "~/.emacs.d/emacs/modes/magit.el")
(load-file "~/.emacs.d/emacs/modes/mozrepl.el")
(load-file "~/.emacs.d/emacs/modes/yasnippet.el")
;(load-file "~/.emacs.d/emacs/modes/org.el")
;(load-file "~/.emacs.d/emacs/modes/php.el")
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
;(load-file "~/.emacs.d/emacs/modes/highlight-sexps.el")
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
(require 'conf-hideshow)
(require 'conf-paredit)
(require 'conf-perspective)
(require 'conf-isearch)
(require 'conf-hideshowvis)
(require 'conf-highlight-symbol)
(require 'conf-rainbow)
;(require 'conf-mode-line)
(require 'conf-highlight-indentation)
(require 'conf-guide-key)
(require 'conf-json)

;; Test
(require 'expand-region)
(require 'delsel)
(require 'jump-char)
(require 'smart-forward)
(require 'change-inner)
(require 'tabkey2)

