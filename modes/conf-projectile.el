;;; Code:
(require 'projectile)

(setq projectile-enable-caching t
      projectile-cache-file (in-emacs-d ".cache/projectile.cache")
      projectile-known-projects-file (in-emacs-d ".cache/projectile-bookmarks.eld")
      projectile-file-exists-remote-cache-expire (* 10 60)
      projectile-remember-window-configs t)

(add-to-list 'projectile-globally-ignored-directories "node_modules")

;; keymap prefixes
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Enable
(projectile-mode)

(provide 'conf-projectile)
