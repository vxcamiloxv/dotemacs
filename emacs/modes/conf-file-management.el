(provide 'conf-file-management)
; autosave settings
(setq auto-save-list-file-prefix nil)
(setq make-backup-files nil)

; recentf - save history of recently visited files
(require 'recentf)
(run-with-idle-timer (* 5 60) t 'recentf-save-list)
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 1000)

(setq recentf-exclude
(append recentf-exclude
    '(
    "/.emacs.d/el-get/" "~$" "/.autosaves/"
    "/.emacs.d/elpa/" "/.emacs.bmk$"
    "\\.ido.last$" "session\\.[a-f0-9]*$"
    "\\.recentf" "/.emacs.d/TAGS"
    )
))

; saveplace - save position in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (in-emacs-d ".places"))

; rename buffer and file
(defun my/rename-current-buffer-file ()
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

(global-set-key (kbd "C-x C-r") 'my/rename-current-buffer-file)

; find-files-in-project
(require 'find-file-in-project)

; Recognize zsh files
(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))
