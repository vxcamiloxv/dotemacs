(require 'markdown-mode)
(require 'org-table)
;;; Code:

;; Functions
(defun distopico:md-convert-org-tbl ()
  "Convert org-tables to md format github"
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))))

(defun distopico:md-clean-tbl ()
  "Clean tables if have white space"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "|\n" nil t)
      (let (p1 p2)
        (if (region-active-p)
            (progn (setq p1 (region-beginning))
                   (setq p2 (region-end)))
          (progn (setq p1 (line-beginning-position))
                 (setq p2 (line-end-position))))
        (save-excursion
          (let (wCnt charCnt)
            (setq wCnt 0)
            (setq charCnt (- p2 p1))
            (goto-char p1)
            (while (and (< (point) p2) (re-search-forward "\\w+\\W*" p2 t))
              (setq wCnt (1+ wCnt)))
            (when (eq charCnt 0)
              (replace-match "|")
              (while (search-forward "|-\n" nil t)
                (let (p1 p2)
                  (if (region-active-p)
                      (progn (setq p1 (region-beginning))
                             (setq p2 (region-end)))
                    (progn (setq p1 (line-beginning-position))
                           (setq p2 (line-end-position))))
                  (save-excursion
                    (let (wCnt charCnt)
                      (setq wCnt 0)
                      (setq charCnt (- p2 p1))
                      (goto-char p1)
                      (while (and (< (point) p2) (re-search-forward "\\w+\\W*" p2 t))
                        (setq wCnt (1+ wCnt)))
                      (when (eq charCnt 0)
                        (replace-match "|-")
                        )))))
              )))))))

;; Hooks
(add-hook 'markdown-mode-hook 'orgtbl-mode)
(add-hook 'markdown-mode-hook
          (lambda()
            (add-hook 'after-save-hook 'distopico:md-convert-org-tbl  nil 'make-it-local)))

(provide 'conf-markdown)
