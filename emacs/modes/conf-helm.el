(provide 'conf-helm)

(require 'helm)
;(require 'helm-config)
(helm-mode t)

(setq helm-boring-buffer-regexp-list '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*monky-cmd-process\\*" "\\*epc con" "\\*Compile-Log\\*" "\\*monky-process\\*" "\\*CEDET CScope\\*" "\\*Messages\\*" "\\*Flycheck error" "\\*Elpy" "\\*elpy-rpc" "\\*magit"))
(setq helm-boring-file-regexp-list '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$" "\\.pyc$"
				     "\\.elc$" "TAGS" "\#*\#" "\\.exe$" "\\.zip$" "\\.tar." "\\.rar$" "\\.7z$" "\\.jar$"
				     "\\.img$" "\\.iso$" "\\.xlsx$" "\\.epub$" "\\.docx$"))

(setq helm-ff-skip-boring-files t)
(setq helm-move-to-line-cycle-in-source t)
(setq helm-truncate-lines t)

(defun emacs-d-find-file ()
  (interactive)
  (helm-projectile-find-file-in-dir "~/.emacs.d"))

(global-set-key (kbd "C-c C-e") 'emacs-d-find-file)

(defun helm-projectile-find-file-in-dir (dir-name)
  (let ((default-directory dir-name))
    (helm-projectile)))

(defun make-quickdir (modifier dir-name)
  (lexical-let ((d dir-name))
    (let ((hook (lambda  () (interactive) (helm-projectile-find-file-in-dir d))))
      (global-set-key modifier hook))))



