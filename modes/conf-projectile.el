;;; Code:
(require 'projectile)
(require 'project-explorer)

(projectile-global-mode)
(setq projectile-enable-caching t
      projectile-cache-file (in-emacs-d ".cache/projectile.cache")
      projectile-known-projects-file (in-emacs-d ".cache/projectile-bookmarks.eld")
      projectile-remember-window-configs t)

;; Project Explorer
(setq pe/width 20
      pe/cache-directory (in-emacs-d ".cache/project-explorer-cache/")
      pe/project-root-function 'projectile-project-root)

;; Touch tree to open
(define-key project-explorer-mode-map (kbd "<mouse-1>") 'pe/return)

;; Open project explorer with swipe from left margin
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'distopico:project-explorer)

;; Highlight current tree item
(add-hook 'project-explorer-mode-hook 'hl-line-mode)

;; Functions
(defun distopico:project-explorer () (interactive)
       (-if-let (win (car (-keep 'get-buffer-window (pe/get-project-explorer-buffers))))
           (delete-window win)
         (project-explorer-open)))

(provide 'conf-projectile)
