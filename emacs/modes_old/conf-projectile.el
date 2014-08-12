(provide 'conf-projectile)

(require 'projectile)
(require 'project-explorer)

(projectile-global-mode)
(setq projectile-indexing-method 'git)
(setq projectile-enable-caching t)
(setq projectile-remember-window-configs t)
;(setq projectile-keymap-prefix (kbd "C-c C-p"))


;;; Project Explorer

(require 'project-explorer)
(setq-default pe/width 20)

;; Touch tree to open
(define-key project-explorer-mode-map (kbd "<mouse-1>") 'pe/return)

;; Open project explorer with swipe from left margin
(global-set-key
 (kbd "<left-margin> <drag-mouse-1>")
 (lambda () (interactive)
   (-if-let (win (car (-keep 'get-buffer-window (pe/get-project-explorer-buffers))))
       (delete-window win)
     (project-explorer-open))))

;; Highlight current tree item
(add-hook 'project-explorer-mode-hook 'hl-line-mode)

;;Other tree project manager
(require 'direx)
(eval-after-load "python"
  '(define-key python-mode-map (kbd "C-<f6>") 'jedi-direx:pop-to-buffer))
