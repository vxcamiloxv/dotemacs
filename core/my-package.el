;;; Code:
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
    exec-path-from-shell

    ;; Helpers
    which-key
    change-inner
    goto-last-change
    region-bindings-mode
    misc-cmds
    buffer-move

    ;; Ido
    ido-sort-mtime
    ido-completing-read+
    ido-vertical-mode
    ido-select-window
    ido-complete-space-or-hyphen
    ido-occur
    ido-at-point
    crm-custom

    ;; Window
    zoom-window
    windresize
    ace-window
    es-windows

    ;; Dired
    direx
    dired+
    dired-details+
    image-dired+
    dired-hacks-utils
    dired-subtree
    dired-ranger
    dired-narrow
    dired-sort
    dired-imenu

    ;; Ui
    hlinum
    linum-off
    iedit
    paredit
    icicles
    column-marker
    god-mode
    gist
    tabbar
    htmlize
    highlight-symbol
    flycheck
    flycheck-color-mode-line
    jump-char
    smex
    perspective
    smartparens
    aggressive-indent
    projectile
    simple-httpd
    rainbow-mode
    rainbow-identifiers
    rainbow-delimiters
    mouse-slider-mode
    highlight-escape-sequences
    whitespace-cleanup-mode
    elisp-slime-nav
    dropdown-list
    smart-tabs-mode
    fill-column-indicator
    browse-kill-ring
    highlight-indentation
    find-file-in-project
    powerline

    ;; Helm
    helm
    helm-swoop
    helm-projectile
    helm-css-scss
    helm-emmet

    ;; Programing
    ag
    emr
    nodejs-repl
    php-mode
    php-extras
    php-auto-yasnippets
    pony-mode
    jedi
    realgud
    lua-mode
    move-dup
    anaconda-mode
    jdee
    java-snippets
    java-imports
    meghanada
    android-mode
    gradle-mode
    groovy-mode
    dumb-jump

    ;; Web
    web-mode
    restclient
    emmet-mode
    js2-mode
    js2-refactor
    rjsx-mode
    js-doc
    xref-js2
    json-mode
    css-eldoc
    less-css-mode
    jade-mode
    pug-mode ;; Tets what is better
    markdown-mode
    web-beautify
    smart-forward
    angular-snippets

    ;; Utils
    magit
    anzu
    editorconfig
    iy-go-to-char
    quickrun
    minimap
    fic-mode
    multi-term
    tramp-term
    expand-region
    yasnippet
    avy
    avy-zap
    multiple-cursors
    auto-async-byte-compile
    sync-recentf
    emojify
    flyspell-correct

    ;; Auto Complete
    company
    company-web
    company-tern
    company-restclient
    company-jedi
    company-anaconda
    company-statistics
    company-quickhelp
    ac-js2

    ;; Org
    org-plus-contrib
    org-projectile
    org-present
    org-pomodoro
    org-mime
    org-link-types
    toc-org
    ox-rst

    ;; Activity
    znc
    elfeed
    elfeed-org
    mu4e-maildirs-extension

    ;; Gtags
    etags-select
    ggtags

    ;; Dubug
    skewer-mode
    skewer-less
    )
  "A list of packages to ensure are installed at launch."
  )

(provide 'my-package)
