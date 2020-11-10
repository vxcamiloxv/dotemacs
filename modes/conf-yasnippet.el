;;; Code:
(require 'yasnippet)
(require 'angular-snippets)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/my-snippets"                    ;; personal snippets
        "~/.emacs.d/snippets/yasnippet-snippets/snippets"    ;; third party snippets
        "~/.emacs.d/snippets/yasmate/snippets"               ;; the yasmate collection
        "~/.emacs.d/snippets/extend-snippets"                ;; Extend basic snippets
        )
      yas-prompt-functions
      '(yas-maybe-ido-prompt
        yas-completing-prompt
        yas-no-prompt)
      hippie-expand-try-functions-list
      (cons 'yas-hippie-try-expand hippie-expand-try-functions-list))

;; Functions
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key with ido."
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

;; Enable
(yas-global-mode 1)

(provide 'conf-yasnippet)
