;; Install extensions if they're missing
(defvar distopico-packages
   '(magit
     paredit
     icicles
     move-text
     god-mode
     gist
     htmlize
     highlight-symbol
     visual-regexp
     flycheck
     flx
     jedi
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
     window-numbering
     smart-tabs-mode
     fill-column-indicator
     browse-kill-ring
     highlight-indentation
     php-extras
     ac-emmet
     ac-js2
     js2-mode
     iy-go-to-char
     tabkey2
     fic-mode
     )
       "A list of packages to ensure are installed at launch."
)

(provide 'my-package)
