(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key [(M C i)] 'aj-toggle-fold)

(global-set-key (kbd "C-<f4>") 'highlight-indentation-current-column-mode)
(global-set-key (kbd "C-<f5>") 'highlight-indentation-mode)

(require 'highlight-indentation)

(set-face-background 'highlight-indentation-face "gray5")
(set-face-background 'highlight-indentation-current-column-face "#2f4f4f")

(provide 'conf-highlight-indentation)
