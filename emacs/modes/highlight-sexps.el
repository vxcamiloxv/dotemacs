;; highlight
(add-to-list 'load-path "~/.emacs.d/el-get/highlight-sexps")
(require 'highlight-sexps)

(setq hl-sexp-background-colors '("#121212" "#2b2925"))
(add-hook 'lisp-mode-hook 'highlight-sexps-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-sexps-mode)
