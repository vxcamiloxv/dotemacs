;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(magit
     paredit
     move-text
     god-mode
     gist
     htmlize
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
     cider-tracing)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(provide 'my-package)   
