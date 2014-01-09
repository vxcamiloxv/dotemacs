;; ---------
;; Word count
;; ---------

(load "~/.emacs.d/modes/word-counter.el")
(require 'word-counter)
(global-set-key (kbd "C-c w") 'word-count)
