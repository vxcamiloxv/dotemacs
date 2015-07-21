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
    paradox
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
    direx
    dired+
    dired-details+
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
    dropdown-list
    window-numbering
    smart-tabs-mode
    fill-column-indicator
    browse-kill-ring
    highlight-indentation
    find-file-in-project

    ;; Helm
    helm
    helm-swoop
    helm-projectile
    helm-css-scss
    helm-emmet

    ;; Programing
    emr
    nodejs-repl
    php-mode
    php-extras
    php-auto-yasnippets
    pony-mode
    jedi
    anaconda-mode

    ;; Web
    restclient
    emmet-mode
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
    magit
    anzu
    iy-go-to-char
    quickrun
    fic-mode
    multi-term
    expand-region
    yasnippet
    avy
    avy-zap
    ace-window
    multiple-cursors
    auto-async-byte-compile

    ;; Auto Complete
    company
    company-web
    company-tern
    company-restclient
    company-jedi
    company-anaconda
    auto-complete
    ac-python
    ac-emmet
    ac-capf
    ac-js2

    ;; Org
    org-plus-contrib
    org-projectile
    org-present
    org-pomodoro
    toc-org

    ;; Activity
    mu4e-maildirs-extension

    ;; Dubug
    skewer-mode
    skewer-less
    )
  "A list of packages to ensure are installed at launch."
  )

(provide 'my-package)
