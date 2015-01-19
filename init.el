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


;; Completion
;; Give a change to company-mode
(add-hook 'after-init-hook 'global-company-mode)
;;(add-to-lis 'company-backends '(company-dabbrev company-keywords company-abbrev company-capf company-yasnippet company-dabbrev-code company-files))
(setq company-dabbrev-other-buffers t
      company-complete-number t
      company-show-numbers t
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-idle-delay 0.2)

(defun add-pcomplete-to-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
(add-hook 'org-mode-hook #'add-pcomplete-to-capf)

;; (global-auto-complete-mode t)
;; (add-to-list 'ac-dictionary-directories  "~/.emacs.d/ac-dict")
;; (ac-config-default)
;; (ac-linum-workaround)
;; ;;(setq ac-delay 0.2)
;; ;;(setq ac-auto-show-menu 0.3)
;; (setq ac-use-fuzzy t)

;; (defvar distopico-ac-default
;;   '(;ac-source-yasnippet
;;     ac-source-features
;;     ac-source-abbrev
;;     ac-source-dictionary
;;     ac-source-imenu
;;     ;;ac-source-gtags
;;     ;;ac-source-semantic
;;     ac-source-words-in-same-mode-buffers
;;     ac-source-words-in-buffer
;;     ac-source-files-in-current-dir
;;     ))
;; (setq-default ac-sources distopico-ac-default)
;; (require 'org-ac)
;; (org-ac/config-default)

;; Django test

;;(require 'python-django)
(require 'pony-mode)
;;(add-to-list 'auto-mode-alist '("\\.dtpl$" . pony-tpl-mode))

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
(require 'neotree)
(setq neo-theme 'arrow
      neo-smart-open t
      neo-cwd-line-style 'button
      projectile-switch-project-action 'neotree-projectile-action)

(defun distopico:neotree-toggle ()
  "Fix split when emacs-nav is open"
  (interactive)
  (if (get-buffer "*nav*")
      (progn
        (kill-buffer "*nav*")
        (neotree-toggle))
    (neotree-toggle)
    )
  )
(defun distopico:nav-toggle ()
  "Close neotree and open nav"
  (interactive)
  (neotree-hide)
  (nav-toggle)
  )

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Aditionals Modes
(require 'conf-mu4e)
(load-file "~/.emacs.d/emacs/modes/column-marker.el")
(load-file "~/.emacs.d/emacs/modes/css.el")
(load-file "~/.emacs.d/emacs/modes/erc.el")
(load-file "~/.emacs.d/emacs/modes/eshell.el")
(load-file "~/.emacs.d/emacs/modes/longlines.el")
(load-file "~/.emacs.d/emacs/modes/magit.el")
(load-file "~/.emacs.d/emacs/modes/mozrepl.el")
(load-file "~/.emacs.d/emacs/modes/yasnippet.el")
;;(load-file "~/.emacs.d/emacs/modes/php.el")
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
;;(load-file "~/.emacs.d/emacs/modes/highlight-sexps.el")
(load-file "~/.emacs.d/emacs/modes/multiple-cursors.el")
(require 'conf-pomodoro)
(require 'conf-org)
(require 'conf-present)
(require 'conf-markdown)
(require 'conf-tabbar)
(require 'conf-ideskel)
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
;;(require 'conf-perspective)
(require 'conf-isearch)
(require 'conf-hideshowvis)
(require 'conf-highlight-symbol)
(require 'conf-rainbow)
;;(require 'conf-mode-line)
(require 'conf-highlight-indentation)
(require 'conf-guide-key)
(require 'conf-json)
(require 'conf-eclim)

;; Test
(require 'expand-region)
(require 'delsel)
(require 'jump-char)
(require 'smart-forward)
(require 'change-inner)
(require 'tabkey2)

;;TODO
;; Warning (initialization): Your `load-path' seems to contain
;; your `.emacs.d' directory: ~/.emacs.d/
;; This is likely to cause problems...
;; Consider using a subdirectory instead, e.g.: /home/distopico/.emacs.d/lisp
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(cua-enable-cua-keys nil)
 '(cua-keep-region-after-copy t)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(cursor-type (quote bar))
 '(custom-safe-themes t)
 '(display-time-mode t)
 '(ediff-custom-diff-program "diff")
 '(ediff-diff-program "diff")
 '(ediff-diff3-program "diff3")
 '(enable-local-variables :all)
 '(fci-rule-character-color "#202")
 '(fci-rule-color "#202")
 '(flycheck-highlighting-mode (quote lines))
 '(flycheck-indication-mode (quote right-fringe))
 '(global-auto-highlight-symbol-mode nil)
 '(ide-skel-tabbar-mwheel-mode nil t)
 '(indicate-buffer-boundaries (quote right))
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js3-enter-indents-newline t)
 '(js3-indent-level 2)
 '(jshint-configuration-path "~/.jshintrc")
 '(linum-delay nil)
 '(linum-eager t)
 '(menu-bar-mode nil)
 '(org-agenda-files
   (quote
    ("/home/distopico/Documents/org/life.org" "/home/distopico/Documents/org/annotated.org" "/home/distopico/Documents/org/calendar.org" "/home/distopico/Documents/org/contacts.org" "/home/distopico/Documents/org/myday.org" "/home/distopico/Documents/org/organizer.org" "/home/distopico/Documents/org/todo.org")))
 '(rainbow-delimiters-mode nil t)
 '(rainbow-identifiers-mode nil t)
 '(savehist-file (in-emacs-d ".cache/history"))
 '(savehist-mode t nil (savehist))
 '(show-paren-mode 1)
 '(size-indication-mode t)
 '(sml-modeline-mode 1)
 '(system-time-locale "C" t)
 '(tabbar-button-highlight ((t (:inherit tabbar-button))))
 '(tabbar-highlight ((t nil)))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-scroll-left-button (quote (("") "")))
 '(tabbar-scroll-right-button (quote (("") "")))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(windmove-wrap-around t)
 '(window-left-margin 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:foreground "forest green"))))
 '(completions-first-difference ((t (:bold t :weight bold :foreground "salmon"))))
 '(flycheck-error-face ((t (:inherit error :background "gray27" :foreground "IndianRed1" :underline (:color "red" :style wave)))))
 '(flycheck-warning-face ((t (:inherit warning :foreground "yellow1"))))
 '(hideshowvis-hidable-face ((t (:foreground "#2f4f4f"))))
 '(hs-face ((t (:background "#444" :box t))))
 '(hs-fringe-face ((t (:foreground "#afeeee" :box (:line-width 2 :color "grey75" :style released-button)))))
 '(js2-highlight-vars-face ((t (:background "royal blue" :foreground "white"))))
 '(powerline-active1 ((t (:foreground "#f9f9f9" :background "#123550" :box nil))))
 '(powerline-active2 ((t (:foreground "#f9f9f9" :background "#112230" :box nil))))
 '(web-mode-current-element-highlight-face ((t (:foreground "#FF8A4B"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "#FFE84B"))))
 '(web-mode-html-tag-face ((t (:foreground "#729fcf")))))
