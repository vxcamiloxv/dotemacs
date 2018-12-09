;;; Code:
(require 'projectile)
(require 'project-explorer)

(projectile-mode)
(setq projectile-enable-caching t
      projectile-cache-file (in-emacs-d ".cache/projectile.cache")
      projectile-known-projects-file (in-emacs-d ".cache/projectile-bookmarks.eld")
      projectile-file-exists-remote-cache-expire (* 10 60)
      projectile-remember-window-configs t)

(add-to-list 'projectile-globally-ignored-directories "node_modules")

;; keymap prefixes
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
