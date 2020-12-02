;;; Code:
;; Install extensions if they're missing
(defvar distopico-packages
  '(
    ;; Basic
    s
    cl
    cl-lib
    dash
    dash-functional ;; require by company-tern
    es-lib
    log4e
    paradox
    exec-path-from-shell

    ;; Ido
    crm-custom
    ido-at-point
    ido-complete-space-or-hyphen
    ido-completing-read+
    ido-occur
    ido-select-window
    ido-sort-mtime
    ido-vertical-mode

    ;; Window/Buffer
    ace-window
    es-windows
    windresize
    zoom-window
    buffer-move

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
    anzu
    highlight-escape-sequences
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
    diminish
    rainbow-delimiters
    rainbow-mode
    tabbar

    ;; TODO: check those modes/libraries
    iedit
    misc-cmds
    paredit
    smartparens

    ;; Auto Complete
    company-anaconda
    company-jedi
    company-quickhelp
    company-restclient
    company-statistics
    company-web
    company

    ;; Development
    ag
    eglot
    magit
    quickrun
    yasnippet
    editorconfig
    aggressive-indent
    emr
    js-doc
    js2-refactor
    flycheck
    flycheck-gradle
    flycheck-guile
    simple-httpd
    smartscan ;; Test if we should use it
    move-dup
    dumb-jump
    change-inner
    expand-region
    smart-forward
    xref-js2
    elisp-slime-nav
    ggtags
    counsel-etags
    tern ;; I should try eglot
    geiser

    ;; Debug
    nodejs-repl
    realgud
    skewer-less
    skewer-mode

    ;; Languages
    anaconda-mode
    android-mode
    android-env ;; I should merge android-mode and android-env in a single package
    meghanada
    jedi
    java-imports
    java-snippets ;; Move to sub-modules as the other snippets?
    javadoc-lookup
    gradle-mode
    groovy-mode
    kotlin-mode
    rust-mode
    lua-mode
    php-mode
    pony-mode

    ;; Web
    css-eldoc
    less-css-mode
    web-mode
    rjsx-mode
    js2-mode
    emmet-mode
    yaml-mode
    json-mode
    markdown-mode
    mmm-mode
    jade-mode
    pug-mode ;; Test what is better
    web-beautify
    restclient

    ;; Utilities
    avy
    avy-zap
    auto-async-byte-compile
    htmlize
    emojify
    fic-mode
    multi-term
    multiple-cursors
    sync-recentf
    tramp-term
    guix

    ;; Helpers
    goto-last-change
    region-bindings-mode
    which-key

    ;; Projects
    projectile
    org-projectile
    find-file-in-project

    ;; Org / Productivity
    flyspell-correct
    org-plus-contrib
    org-link-types
    org-pomodoro
    org-present
    org-mime
    toc-org
    ox-rst
    ox-hugo
    ox-gfm

    ;; Activity
    elfeed
    elfeed-org
    mu4e-maildirs-extension
    znc)
  "A list of packages to ensure are installed at launch.")

(provide 'packages)
