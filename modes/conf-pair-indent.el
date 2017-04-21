;;; Code:

;; Native emacs pair
(electric-pair-mode t)

;; Custom keys
(global-set-key [home] 'distopico:smart-beginning-of-line)
(global-set-key (kbd "C-a") 'distopico:smart-beginning-of-line)

;; Functions
(defun distopico:set-newline-and-indent ()
  "Return and indent on `prog-mode' variants."
  (local-set-key [(return)] 'newline-and-indent))

(defun distopico:smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line'.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;; Hooks
(add-hook 'prog-mode-hook 'distopico:set-newline-and-indent)

(provide 'conf-pair-indent)
