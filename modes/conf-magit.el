;;; Code:
(require 'magit)

(setq transient-history-file (in-emacs-d ".cache/transientl/history.el")
      magit-completing-read-function 'magit-ido-completing-read)

(add-to-list 'git-commit-setup-hook 'git-commit-turn-on-flyspell);

;; By default the diff for the changes that are about to be committed
;;  are automatically shown when invoking the commit
;; Remove this prevent that
(remove-hook 'server-switch-hook 'magit-commit-diff)

;; (add-to-list 'with-editor-cancel-query-functions 'distopico:close-emacs-giteditor)
;; (add-to-list 'with-editor-finish-query-functions 'distopico:close-emacs-giteditor)

;; (defun distopico:close-emacs-giteditor(&optional force)
;;   (if (equal "emacs-giteditor" (frame-parameter nil 'name))
;;       (delete-frame)))

(defun distopico:magit-popup-mode-hook ()
  "Hook when a magit popup is open."
  (tabbar-local-mode))

(add-hook 'magit-popup-mode-hook #'distopico:magit-popup-mode-hook)

(provide 'conf-magit)
