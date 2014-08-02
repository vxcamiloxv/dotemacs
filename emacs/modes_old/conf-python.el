;;(add-to-list 'load-path (in-modes-d "python.el"))
(require 'python)

(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (setq python-indent-offset 4)
    (setq tab-width 4)))

(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?l] 'pylookup-lookup)
             (local-set-key (kbd "C-c #") 'comment-or-uncomment-region)))

; run redgreen in current project
(require 'redgreen-at-project-root)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?r] 'redgreen-at-project-root)))

; python-auto-super
(require 'python-auto-super)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?s] 'python-auto-super)))

; python-auto-import
(require 'python-auto-import)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?i] 'python-auto-import)))

;(ac-ropemacs-initialize)
;(add-hook 'python-mode-hook
;          (lambda ()
;	    (add-to-list 'ac-sources 'ac-source-ropemacs)))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; Initialize Pymacs
;(load-file "~/.emacs.d/elpa/pymacs-0.25/pymacs.el")
;(require 'pymacs)

;(autoload 'pymacs-apply "pymacs")
;(autoload 'pymacs-call "pymacs")
;(autoload 'pymacs-eval "pymacs" nil t)
;(autoload 'pymacs-exec "pymacs" nil t)
;(autoload 'pymacs-load "pymacs" nil t)
;; Initialize Rope
;(pymacs-load "ropemacs" "rope-")
;(setq ropemacs-enable-autoimport t)

(provide 'conf-python)
