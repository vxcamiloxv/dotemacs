;;; Code:
(require 'etags-select)
(require 'projectile)

(defcustom ctags-update-lighter " etagsU"
  "Lighter displayed in mode line when `distopico:etags-auto-update-mode' is enabled."
  :group 'distopico:etags-update
  :type 'string)

(defcustom distopico:etags-update-delay-seconds  (* 5 60) ; 5 mins
  "In `after-save-hook' `current-time' - last-time must bigger than this value \
then `distopico:etags-update' will be called."
  :type 'integer
  :group 'ctags-update)

(defvar distopico:etags-update-last-update-time
  (- (float-time (current-time)) distopico:etags-update-delay-seconds 1)
  "Make sure when user first call `distopico:etags-update' it can run immediately.")

;; Functions
(defun distopico:projectile-update-tags ()
  "Update the project's [e|g]tags."
  (interactive)
  (when (> (- (float-time (current-time))
              distopico:etags-update-last-update-time)
           distopico:etags-update-delay-seconds)
    (if (and (boundp 'ggtags-mode)
             (memq projectile-tags-backend '(auto ggtags)))
        (progn
          (let* ((ggtags-project-root (projectile-project-root))
                 (default-directory ggtags-project-root))
            (ggtags-ensure-project)
            (ggtags-update-tags t)))
      (let* ((project-root (projectile-project-root))
             (tags-exclude (projectile-tags-exclude-patterns))
             (default-directory project-root)
             (tags-file (expand-file-name projectile-tags-file-name))
             (command (format projectile-tags-command tags-file tags-exclude))
             shell-output exit-code)
        (when (and (file-exists-p tags-file) ;
                   (not (and (buffer-file-name)
                             (string-equal tags-file (buffer-file-name)))) ;
                   (setq distopico:etags-update-last-update-time (float-time (current-time)));;update time
                   (with-temp-buffer
                     (setq exit-code
                           (call-process-shell-command command nil (current-buffer))
                           shell-output (projectile-trim-string
                                         (buffer-substring (point-min) (point-max)))))
                   (unless (zerop exit-code)
                     (error shell-output))))))))

;;;###autoload
(define-minor-mode distopico:etags-auto-update-mode
  "Auto update TAGS using `exuberant-ctags' in parent directory."
  :lighter distopico:etags-update-lighter
  ;; :global t
  :init-value nil
  :group 'distopico:etags-update
  (if distopico:etags-auto-update-mode
      (progn
        (add-hook 'after-save-hook 'distopico:projectile-update-tags nil t))
    (remove-hook 'after-save-hook 'distopico:projectile-update-tags t)))

;;;###autoload
(defun distopico:turn-on-etags-auto-update-mode()
  "Turn on `etags-auto-update-mode'."
  (interactive)
  (distopico:etags-auto-update-mode 1))

;; Enable for all programming mode
(add-hook 'prog-mode-hook  'distopico:turn-on-etags-auto-update-mode)


(provide 'conf-gtags)
