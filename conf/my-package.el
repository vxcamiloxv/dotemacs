;; Install extensions if they're missing
(defvar distopico-packages
  '(
    ;; Basic
    s
    cl
    dash
    cl-lib
    es-lib
    log4e
    ;;auto-package-update

    ;; Ui
    iedit
    paredit
    icicles
    diminish
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
    helm-swoop
    helm-projectile
    helm-css-scss
    perspective
    smartparens
    aggressive-indent
    projectile
    es-windows
    ido-ubiquitous
    ido-vertical-mode
    ido-at-point
    simple-httpd
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
    emr
    nodejs-repl
    php-mode
    php-extras
    php-auto-yasnippets
    pony-mode
    ac-python

    ;; Web
    magit
    restclient
    ac-js2
    ac-emmet
    js2-mode
    js2-refactor
    json-mode
    css-eldoc
    less-css-mode
    markdown-mode
    web-beautify
    smart-forward
    emacs-eclim
    angular-snippets

    ;; Utils
    anzu
    company
    iy-go-to-char
    tabkey2
    quickrun
    fic-mode
    multi-term
    expand-region
    ace-jump-mode
    ace-jump-zap
    ace-isearch
    multiple-cursors
    auto-async-byte-compile

    ;; Org
    org-plus-contrib
    org-projectile
    org-present
    org-pomodoro
    toc-org
    org-ac

    ;; Activity
    mu4e-maildirs-extension

    ;; Dubug
    skewer-mode
    skewer-less
    )
  "A list of packages to ensure are installed at launch."
  )

(provide 'my-package)
