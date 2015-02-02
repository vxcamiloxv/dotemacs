;;; Code:

(require 'nav)
(require 'neotree)

;; Basic
(setq neo-theme 'arrow
      neo-smart-open t
      neo-cwd-line-style 'button
      projectile-switch-project-action 'neotree-projectile-action)

;; Funtions
(defun distopico:neotree-toggle ()
  "Fix split when emacs-nav is open"
  (interactive)
  (if (get-buffer "*nav*")
      (progn
        (kill-buffer "*nav*")
        (neotree-toggle))
    (neotree-toggle)
    )
  )
(defun distopico:nav-toggle ()
  "Close neotree and open nav"
  (interactive)
  (neotree-hide)
  (nav-toggle)
  )

(provide 'conf-nav)
