;; Install extensions if they're missing
(defvar distopico-packages
   '(magit
     paredit
     move-text
     god-mode
     gist
     htmlize
     highlight-symbol
     visual-regexp
     flycheck
     flx
     flx-ido
     css-eldoc
     yasnippet
     smartparens
     ido-vertical-mode
     ido-at-point
     simple-httpd
     guide-key
     nodejs-repl
     restclient
     highlight-escape-sequences
     whitespace-cleanup-mode
     elisp-slime-nav
     git-commit-mode
     gitconfig-mode
     gitignore-mode
     clojure-mode
     groovy-mode
     cider
     cider-tracing
     dropdown-list
     )
       "A list of packages to ensure are installed at launch."
)

(provide 'my-package)
