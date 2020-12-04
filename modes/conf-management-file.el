;;; Code:
(require 'recentf)
(require 'saveplace)
(require 'sync-recentf)
(require 'find-file-in-project)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; recentf/save-place settings
(setq-default
 recentf-auto-cleanup 300
 recentf-max-saved-items 300
 recentf-exclude
 (append recentf-exclude
         '("\\.emacs.d/el-get/" "~$" "\\.autosaves/"
           "\\.emacs.d/elpa/" "/.emacs.bmk$"
           "\\.ido.last" "session\\.[a-f0-9]*$"
           "\\.recentf" "/.emacs.d/TAGS"
           "\\.cache/" "\\.mail/"))
 save-place t ; save position in files
 save-place-forget-unreadable-files nil ; no checks every loaded file is readable
 save-place-file (in-emacs-d ".cache/places"))

;; Recognize zsh files
(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))

;; recentf - save history of recently visited files
(recentf-mode 1)
(run-with-idle-timer (* 5 60) t 'recentf-save-list)

;; Functions
(defun distopico:rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(provide 'conf-management-file)
;;; conf-management-file.el ends here
