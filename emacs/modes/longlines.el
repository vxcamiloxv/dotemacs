;; ---------
;; LongLines
;; ---------

;; act (more) like a word processor
(load "~/.emacs.d/modes/longlines.el")
(add-to-list 'auto-mode-alist '("\\.ll\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . text-mode))
