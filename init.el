;;; Code:

;; Initialized package
(package-initialize)

(message "      ______                          ")
(message "     / ____/___ ___  ____ ___________ ")
(message "    / __/ / __ `__ \/ __ `/ ___/ ___/ ")
(message "   / /___/ / / / / / /_/ / /__(__  )  ")
(message "  /_____/_/ /_/ /_/\__,_/\___/____/   ")
(message "                                      ")

;; Core
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
;; Packages
(require 'setup-package)
(require 'setup-elget)

;; Path
(require 'setup-path)
(distopico:startup-load-path)

;; Utils libs
(require 'init-defuns)

;;Theme
(require 'setup-color)
(require 'setup-theme)

;; Load up the general config
(require 'setup-general)
(require 'setup-gui)
(require 'setup-keybindings)

;; Basic dependencies
(require 'delsel)
(require 'column-marker)
(require 'expand-region)
(require 'jump-char)
(require 'smart-forward)
(require 'change-inner)
(require 'buffer-move)

;; Color shell text
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Config Modes
;; ----------------

;; Social
(when (require 'mu4e nil 'noerror)
  (require 'conf-mu4e))
(require 'conf-elfeed)
(require 'conf-jabber)
;; (require 'conf-gnusocial)
(require 'conf-erc)

;; Basic Helpers
(require 'conf-smex)
(require 'conf-fringe)
(require 'conf-linum)
(require 'conf-guide-key)
(require 'conf-powerline)
(require 'conf-management-buffer)
(require 'conf-management-file)

;; Navigation
(require 'conf-ido)
(require 'conf-nav)
(require 'conf-tabbar)
(require 'conf-ibuffer)
(require 'conf-isearch)
(require 'conf-dired)
(require 'conf-popwin)
(require 'conf-projectile)

;; Write and organize
(require 'conf-pomodoro)
(require 'conf-org)
(require 'conf-present)
(require 'conf-markdown)
(require 'conf-rst)
(require 'conf-flyspell)

;; Utils
(require 'conf-eshell)
(require 'conf-text)
(require 'conf-tramp)
(require 'conf-ediff)
(require 'conf-avy)

;; Coding helpers
(require 'conf-prog)
(require 'conf-multiple-cursors)
(require 'conf-highlight-code)
(require 'conf-hideshow)
(require 'conf-rainbow)
(require 'conf-hippie)
(require 'conf-pair-indent)
(require 'conf-autocomplete)
(require 'conf-flycheck)
(require 'conf-editorconfig)

;; Development
(require 'conf-yasnippet)
(require 'conf-magit)
(require 'conf-css)
(require 'conf-nxml)
(require 'conf-web-mode)
(require 'conf-javascript)
(require 'conf-jsx)
(require 'conf-java)
(require 'conf-python)
(require 'conf-emmet)
(require 'conf-json)
(require 'conf-dev-utils)
;; Extra config
(require 'conf-extra nil 'noerror)

;; Final
;;(run-at-time "10 seg" nil #'run-hooks 'distopico:after-init-load-hook)
(run-hooks 'distopico:after-init-load-hook)
