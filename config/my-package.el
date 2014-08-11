;;; package --- My Packages
;;;
;;; Commentary:
;;; List all packages dependencies
;;;
;;; Code:

(defvar distopico-packages
   '(s
     cl
     dash
     org
     cl-lib
     es-lib
     magit
     iedit
     paredit
     icicles
     diminish
     move-text
     python-pep8
     python-pylint
     column-marker
     god-mode
     gist
     tabbar
     htmlize
     highlight-symbol
     visual-regexp
     flycheck
     flycheck-color-mode-line
     flx
     flx-ido
     jump-char
     smex
     jedi
     direx
     dired+
     dired-details+
     css-eldoc
     perspective
     yasnippet
     smartparens
     projectile
     es-windows
     ido-ubiquitous
     ido-vertical-mode
     ido-at-point
     simple-httpd
     smart-forward
     guide-key
     nodejs-repl
     restclient
     ido-ubiquitous
     mouse-slider-mode
     highlight-escape-sequences
     whitespace-cleanup-mode
     elisp-slime-nav
     git-commit-mode
     gitconfig-mode
     gitignore-mode
     clojure-mode
     cider
     cider-tracing
     dropdown-list
     window-numbering
     smart-tabs-mode
     fill-column-indicator
     browse-kill-ring
     highlight-indentation
     find-file-in-project
     php-extras
     ac-python
     ac-emmet
     ac-js2
     js2-mode
     js2-refactor
     json-mode
     iy-go-to-char
     tabkey2
     fic-mode
     )
       "A list of packages to ensure are installed at launch."
)

(provide 'my-package)

;;; my-package.el ends here
