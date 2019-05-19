;;; Code:
;; Install extensions if they're missing
(defvar distopico-packages
  '(
    ;; Basic
    cl
    cl-lib
    dash
    es-lib
    exec-path-from-shell
    log4e
    paradox
    s

    ;; Ido
    crm-custom
    ido-at-point
    ido-complete-space-or-hyphen
    ido-completing-read+
    ido-occur
    ido-select-window
    ido-sort-mtime
    ido-vertical-mode

    ;; Window
    ace-window
    es-windows
    windresize
    zoom-window

    ;; Dired
    dired+
    dired-details+
    dired-hacks-utils
    dired-imenu
    dired-narrow
    dired-ranger
    dired-sort
    dired-subtree
    direx
    image-dired+

    ;; UI/UX
    browse-kill-ring
    column-marker
    fill-column-indicator
    flycheck-color-mode-line
    highlight-escape-sequences
    highlight-indentation
    highlight-symbol
    hlinum
    jump-char
    linum-off
    perspective
    powerline
    rainbow-delimiters
    rainbow-identifiers
    rainbow-mode
    smex
    tabbar
    whitespace-cleanup-mode

    ;; TODO: check those modes/libraries
    dropdown-list
    elisp-slime-nav
    gist
    god-mode
    htmlize
    icicles
    iedit
    misc-cmds
    mouse-slider-mode
    paredit
    simple-httpd
    smart-tabs-mode
    smartparens

    ;; Helm
    helm
    helm-css-scss
    helm-emmet
    helm-projectile
    helm-swoop

    ;; Projects
    projectile
    find-file-in-project

    ;; Auto Complete
    company-anaconda
    company-jedi
    company-lsp
    company-quickhelp
    company-restclient
    company-statistics
    company-tern
    company-web
    company

    ;; Development
    ag
    aggressive-indent
    dumb-jump
    editorconfig
    emr
    expand-region
    flycheck
    js-doc
    magit
    move-dup
    xref-js2

    ;; Debug
    nodejs-repl
    realgud
    skewer-less
    skewer-mode

    ;; Languages
    anaconda-mode
    android-mode
    gradle-mode
    groovy-mode
    java-imports
    java-snippets
    jdee
    jedi
    kotlin-mode
    lsp-mode
    lua-mode
    meghanada
    php-mode
    pony-mode

    ;; Web
    angular-snippets
    css-eldoc
    emmet-mode
    jade-mode
    js2-mode
    js2-refactor
    json-mode
    less-css-mode
    markdown-mode
    mmm-mode
    pug-mode ;; Tets what is better
    restclient
    rjsx-mode
    smart-forward
    web-beautify
    web-mode
    yaml-mode

    ;; Utilities
    auto-async-byte-compile
    avy
    avy-zap
    emojify
    fic-mode
    minimap
    multi-term
    multiple-cursors
    quickrun
    sync-recentf
    tramp-term
    yasnippet

    ;; Helpers
    anzu
    buffer-move
    change-inner
    goto-last-change
    iy-go-to-char
    region-bindings-mode
    which-key

    ;; Org / Productivity
    flyspell-correct
    org-link-types
    org-mime
    org-plus-contrib
    org-pomodoro
    org-present
    org-projectile
    ox-rst
    toc-org

    ;; Activity
    elfeed
    elfeed-org
    mu4e-maildirs-extension
    znc

    ;; Ctags/Gtags
    etags-select
    ggtags
    ctags-update
    )
  "A list of packages to ensure are installed at launch."
  )

(provide 'my-package)
