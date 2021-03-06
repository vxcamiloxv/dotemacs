;;; Code:

(require 'multiple-cursors)
(require 'region-bindings-mode)
(require 'expand-region)

;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c <next>") 'er/expand-region)
(global-set-key (kbd "C-c e <left>") 'er/expand-region)
(global-set-key (kbd "C-c e w") 'er/mark-word)
(global-set-key (kbd "C-c e c") 'er/mark-comment)
(global-set-key (kbd "C-c e s") 'er/mark-symbol)

;; Set th cache
(setq mc/list-file (in-emacs-d ".cache/mc-lists.el"))

;; Experimental multiple-cursors
(global-set-key (kbd "C-c r l") 'mc/edit-lines)
(global-set-key (kbd "C-c r e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c r b") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c r p") 'mc/mark-sgml-tag-pair)

;; Extra multiple cursors stuff
(global-set-key (kbd "C-s-<mouse-1>") 'mc/add-cursor-on-click)
;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "S-SPC") 'set-rectangular-region-anchor)

;; Mark additional regions matching current region
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c r d") 'mc/mark-all-dwim)
(global-set-key (kbd "C-c r a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c r m") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c r r") 'mc/mark-all-in-region)

;; Juggle around with the current cursors
(global-set-key (kbd "C-c r u") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-c r U") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c r j") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-c r J") 'mc/skip-to-previous-like-this)

;; Symbol and word specific mark-more
(global-set-key (kbd "s->") 'mc/mark-next-word-like-this)
(global-set-key (kbd "s-<") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "C-c r w") 'mc/mark-all-words-like-this)
(global-set-key (kbd "C-c r S") 'mc/sort-regions)

(global-set-key (kbd "C-s->") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "C-s-<") 'mc/mark-previous-symbol-like-this)
(global-set-key (kbd "C-c r s") 'mc/mark-all-symbols-like-this)

;; keys when region is active
(define-key region-bindings-mode-map (kbd "C-a") 'mark-all-like-this)
(define-key region-bindings-mode-map (kbd "<C-left>") 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map (kbd "<C-right>") 'mc/mark-next-like-this)
(define-key region-bindings-mode-map (kbd "<C-up>") 'mc/mark-more-like-this-extended)
(define-key region-bindings-mode-map (kbd "<C-down>") 'mc/mark-more-like-this-extended)
(define-key region-bindings-mode-map (kbd "<C-home>") 'mc/edit-beginnings-of-lines)
(define-key region-bindings-mode-map (kbd "<C-end>") 'mc/edit-ends-of-lines)
(define-key region-bindings-mode-map (kbd "<C-prior>") 'mc/edit-lines)
(define-key region-bindings-mode-map (kbd "<C-next>") 'mc/edit-lines)


(provide 'conf-multiple-cursors)
