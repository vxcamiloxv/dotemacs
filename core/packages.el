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
    dired-sort
    dired-subtree
    image-dired+

    ;; UI/UX
    whitespace-cleanup-mode
    browse-kill-ring
    column-marker
    fill-column-indicator
    flycheck-color-mode-line
    highlight-indent-guides
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

    ;; TODO: check those modes/libraries
    dropdown-list
    iedit
    misc-cmds
    smartparens

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
    yasnippet
    aggressive-indent
    editorconfig
    emr
    elisp-slime-nav
    expand-region
    flycheck
    flycheck-gradle
    js-doc
    magit
    smartscan
    simple-httpd
    move-dup
    dumb-jump
    xref-js2
    ggtags
    counsel-etags

    ;; Debug
    nodejs-repl
    realgud
    skewer-less
    skewer-mode

    ;; Languages
    anaconda-mode
    android-mode
    android-env ;; I should merge android-mode and android-env in a single package
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
    pug-mode ;; Test what is better
    restclient
    rjsx-mode
    smart-forward
    web-beautify
    web-mode
    yaml-mode

    ;; Utilities
    htmlize
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

    ;; Helpers
    anzu
    buffer-move
    change-inner
    goto-last-change
    iy-go-to-char
    region-bindings-mode
    which-key

    ;; Projects
    projectile
    org-projectile
    find-file-in-project

    ;; Org / Productivity
    flyspell-correct
    org-link-types
    org-mime
    org-plus-contrib
    org-pomodoro
    org-present
    ox-rst
    ox-hugo
    toc-org

    ;; Activity
    elfeed
    elfeed-org
    mu4e-maildirs-extension
    znc)
  "A list of packages to ensure are installed at launch.")

(provide 'packages)