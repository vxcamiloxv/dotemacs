;;; Code:
(require 'anzu)

(global-anzu-mode t)
(diminish 'anzu-mode)

;; Faces
(set-face-attribute 'anzu-mode-line nil
                    :foreground "cyan" :weight 'bold)

;; Hooks
(add-hook 'isearch-mode-end-hook 'distopico:goto-match-beginning)

;; Functions
(defun distopico:isearch-process-search-char()
  (interactive)
  (isearch-process-search-char ?\n))

(defun distopico:goto-match-beginning ()
  "Position of the Cursor after Searching"
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defvar isearch-initial-string nil)
(defun isearch-set-initial-string ()
  "I-search with initial contents."
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(provide 'conf-isearch)
