(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/emacs/yas-snippets"   ;; personal snippets
        "~/.emacs.d/modes/yasnippet"  ; third party snippets
        "~/.emacs.d/snippets"
        "~/.emacs.d/snippets/django"
        "~/.emacs.d/snippets/text-mode"
        ))

;; Also putting hippie-expand here.  Maybe should be its own file..
(define-key global-map "\M-/" 'hippie-expand)

(setq hippie-expand-try-functions-list
      (cons 'yas/hippie-try-expand hippie-expand-try-functions-list))

; Fix orgmode issue with yas
(add-hook 'org-mode-hook
          (lambda ()
            (org-set-local 'yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))


; change dropdown behavior
;(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt yas/no-prompt))

(yas-global-mode 1)

