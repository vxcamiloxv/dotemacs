;;; Code:
(require 'projectile)

(setq projectile-enable-caching t
      projectile-cache-file (in-emacs-d ".cache/projectile.cache")
      projectile-known-projects-file (in-emacs-d ".cache/projectile-bookmarks.eld")
      projectile-file-exists-remote-cache-expire (* 10 60))

;; To overwrite default mode-line lighter
(setq-default projectile-mode-line-prefix " "
              projectile--mode-line projectile-mode-line-prefix)

(add-to-list 'projectile-globally-ignored-directories "node_modules")

;; keymap prefixes
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Enable
(projectile-mode)

(provide 'conf-projectile)
;;; conf-projectile.el ends here
