(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/emacs/yas-snippets"   ;; personal snippets
        "~/.emacs.d/modes/yasnippet/snippets"  ; third party snippets
        "~/.emacs.d/modes/yasnippet/yasmate/snippets" ;; the yasmate collection
        "~/.emacs.d/snippets"
        "~/.emacs.d/snippets/django"
        "~/.emacs.d/snippets/text-mode"
        ))

;; Also putting hippie-expand here.  Maybe should be its own file..
(define-key global-map "\M-#" 'hippie-expand)

(setq hippie-expand-try-functions-list
      (cons 'yas/hippie-try-expand hippie-expand-try-functions-list))

; Fix orgmode issue with yas
(add-hook 'org-mode-hook
          (lambda ()
            (org-set-local 'yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))


; change dropdown behavior
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt yas/no-prompt))

;;Popup Menu
(require 'popup)

;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas/prompt-functions '(yas/popup-isearch-prompt yas/no-prompt))

;;Ido
;; Completing point by some yasnippet key
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
  (let ((original-point (point)))
    (while (and
            (not (= (point) (point-min) ))
            (not
             (string-match "[[:space:]\n]" (char-to-string (char-before)))))
      (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))

(define-key yas-minor-mode-map (kbd "M-ñ")     'yas-ido-expand)

(yas-global-mode 1)
