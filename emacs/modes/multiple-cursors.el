(add-to-list 'load-path "~/.emacs.d/modes/multiple-cursors")
(require 'multiple-cursors)

;; Expand region (increases selected region by semantic units)
(require 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)

;; Experimental multiple-cursors
(global-set-key (kbd "C-c Q") 'mc/edit-lines)
(global-set-key (kbd "C-c A") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c Z") 'mc/edit-beginnings-of-lines)

;; Mark additional regions matching current region
(global-set-key (kbd "s-b") 'mc/mark-all-dwim)
(global-set-key (kbd "s-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "s->") 'mc/mark-next-like-this)
(global-set-key (kbd "s-z") 'mc/mark-all-like-this)
(global-set-key (kbd "s-c") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "s-v") 'mc/mark-all-in-region)

;; Symbol and word specific mark-more
(global-set-key (kbd "s-g") 'mc/mark-next-word-like-this)
(global-set-key (kbd "s-f") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "s-t") 'mc/mark-all-words-like-this)
(global-set-key (kbd "s-e") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "s-d") 'mc/mark-previous-symbol-like-this)
(global-set-key (kbd "s-r") 'mc/mark-all-symbols-like-this)

;; Extra multiple cursors stuff
(global-set-key (kbd "C-¿") 'mc/reverse-regions)
(global-set-key (kbd "M-¿") 'mc/sort-regions)
(global-set-key (kbd "s-¿") 'mc/insert-numbers)

(global-set-key (kbd "C-s-<mouse-1>") 'mc/add-cursor-on-click)

;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "S-SPC") 'set-rectangular-region-anchor)

;; Replace rectangle-text with inline-string-rectangle
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; Quickly jump in document with ace-jump-mode
(define-key global-map (kbd "C-ø") 'ace-jump-mode)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; M-i for back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)
