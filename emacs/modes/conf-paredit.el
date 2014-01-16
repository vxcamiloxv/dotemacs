;; My keybindings for paredit

(require 'paredit)
(require 'dash)
(require 's)

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(defun paredit-wrap-square-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-square))

(defun paredit-wrap-curly-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-curly))

(defun paredit-kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-kill-word)))

;(add-hook 'clojure-mode-hook (lambda () (paredit-mode 1)))
;(add-hook 'cider-repl-mode-hook (lambda () (paredit-mode 1)))
;(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))

    (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
(define-key paredit-mode-map (kbd "M-)") 'paredit-wrap-round-from-behind)
(define-key paredit-mode-map (kbd "M-s-8") 'paredit-wrap-square)
(define-key paredit-mode-map (kbd "M-s-9") 'paredit-wrap-square-from-behind)
(define-key paredit-mode-map (kbd "M-s-(") 'paredit-wrap-curly)
(define-key paredit-mode-map (kbd "M-s-)") 'paredit-wrap-curly-from-behind)

(define-key paredit-mode-map (kbd "C-w") 'paredit-kill-region-or-backward-word)

;; Change nasty paredit keybindings
(defvar my-nasty-paredit-keybindings-remappings
  '(("M-s"         "s-s"         paredit-splice-sexp)
    ("M-<up>"      "M-s-<up>"      paredit-splice-sexp-killing-backward)
    ("M-<down>"    "M-s-<down>"    paredit-splice-sexp-killing-forward)
    ("C-<right>"   "M-s-<right>"   paredit-forward-slurp-sexp)
    ("C-<left>"    "C-s-<left>"    paredit-forward-barf-sexp)
    ("C-M-<left>"  "M-s-<left>"  paredit-backward-slurp-sexp)
    ("C-M-<right>" "C-s-<right>" paredit-backward-barf-sexp)))

(define-key paredit-mode-map (kbd "s-r") 'paredit-raise-sexp)

(--each my-nasty-paredit-keybindings-remappings
  (let ((original (car it))
        (replacement (cadr it))
        (command (car (last it))))
    (define-key paredit-mode-map (read-kbd-macro original) nil)
    (define-key paredit-mode-map (read-kbd-macro replacement) command)))

;; don't hijack \ please
(define-key paredit-mode-map (kbd "\\") nil)

;; Enable `paredit-mode' in the minibuffer, during `eval-expression'.
(defun conditionally-enable-paredit-mode ()
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

;; making paredit work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-newline 'delete-selection t)

;; functions in smartparens that do not have an equivalent in paredit - take a look at them
(when nil
  '(sp-beginning-of-sexp
    sp-end-of-sexp
    sp-next-sexp
    sp-previous-sexp
    sp-kill-sexp
    sp-unwrap-sexp
    sp-backward-unwrap-sexp
    sp-select-next-thing-exchange
    sp-select-next-thing
    sp-forward-symbol
    sp-backward-symbol))

(provide 'conf-paredit)
