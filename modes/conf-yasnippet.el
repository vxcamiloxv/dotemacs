;;; Code:
(require 'yasnippet)
(require 'angular-snippets)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/my-snippets"           ;; personal snippets
        "~/.emacs.d/snippets/yasnippet-snippets"    ;; third party snippets
        "~/.emacs.d/snippets/yasmate/snippets"      ;; the yasmate collection
        "~/.emacs.d/snippets/extend-snippets"       ;; Extend basic snippets
        "~/.emacs.d/snippets"                       ;; Basic folder
        )
      yas-prompt-functions
      '(yas-popup-isearch-prompt,
        yas-completing-prompt
        yas-ido-prompt
        yas-dropdown-prompt))

(yas-global-mode t)

(setq hippie-expand-try-functions-list
      (cons 'yas/hippie-try-expand hippie-expand-try-functions-list))

;; Functions
(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
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
     :isearch t)))

(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key with ido"
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

;; Fix org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local yas-expand-from-trigger-key [tab])
            (define-key yas-keymap [tab] 'yas-next-field-or-maybe-expand)))


(provide 'conf-yasnippet)
