;; Install extensions if they're missing
(defvar distopico-packages
  '(s
    cl
    dash
    cl-lib
    es-lib
    magit
    iedit
    paredit
    icicles
    diminish
    move-text
    column-marker
    god-mode
    gist
    tabbar
    htmlize
    highlight-symbol
    auto-highlight-symbol
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
    helm
    helm-projectile
    perspective
    smartparens
    aggressive-indent
    projectile
    es-windows
    ido-ubiquitous
    ido-vertical-mode
    ido-at-point
    simple-httpd
    smart-forward
    move-dup
    restclient
    rainbow-mode
    rainbow-identifiers
    rainbow-delimiters
    ido-ubiquitous
    mouse-slider-mode
    highlight-escape-sequences
    whitespace-cleanup-mode
    elisp-slime-nav
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    dropdown-list
    window-numbering
    smart-tabs-mode
    fill-column-indicator
    browse-kill-ring
    highlight-indentation
    find-file-in-project
    ;; Programing
    nodejs-repl
    php-extras
    php-auto-yasnippets
    pony-mode
    ac-python
    ;; Web
    ac-js2
    ac-emmet
    js2-mode
    js2-refactor
    json-mode
    css-eldoc
    less-css-mode
    markdown-mode
    web-beautify

    ;; Utils
    iy-go-to-char
    tabkey2
    fic-mode
    multi-term
    mu4e-maildirs-extension

    ;; Dubug
    skewer-mode
    skewer-less
    )
  "A list of packages to ensure are installed at launch."
  )

(provide 'my-package)
