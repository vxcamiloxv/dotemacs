(require 'org-archive)

(defun line-content-as-string ()
  "TODO"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(defun org-child-list ()
  "TODO"
  (interactive)
  (save-excursion
    (if (= (org-outline-level) 0)
	(outline-next-visible-heading 1)
;;        (org-forward-heading-same-level 1)
      (org-goto-first-child))
    (let ((child-list (list (line-content-as-string))))
      (while (org-goto-sibling)
        (setq child-list (cons (line-content-as-string) child-list)))
      child-list)))

(defun fa/org-struct-subtree (&optional start-value)
  "TODO"
  (interactive)
  (let ((archive-tree start-value))
    (save-excursion
      (while (org-up-heading-safe)
        (let ((heading
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
          (if (eq archive-tree nil)
              (setq archive-tree (list heading))
            (setq archive-tree (cons heading archive-tree))))))
    archive-tree))


(defun org-archive-subtree-hierarchical ()
  "TODO"
  (interactive)
  (org-copy-subtree)
  (let ((org-tree (fa/org-struct-subtree))
        (this-buffer (current-buffer))
        (file (abbreviate-file-name
		   (or (buffer-file-name (buffer-base-buffer))
		       (error "No file associated to buffer")))))
    (save-excursion
      (setq location (org-get-local-archive-location)
            afile (org-extract-archive-file location)
            heading (org-extract-archive-heading location)
            infile-p (equal file (abbreviate-file-name (or afile ""))))
      (unless afile
        (error "Invalid `org-archive-location'"))

      (if (> (length afile) 0)
          (setq newfile-p (not (file-exists-p afile))
                visiting (find-buffer-visiting afile)
                buffer (or visiting (find-file-noselect afile)))
        (setq buffer (current-buffer)))
      (unless buffer
        (error "Cannot access file \"%s\"" afile))
      (set-buffer buffer)
      (switch-to-buffer buffer)
      (org-mode)
      (show-all)

      (goto-char (point-min))
      (while (not (equal org-tree nil))
        (let ((child-list (org-child-list)))
          (if (member (car org-tree) child-list)
              (progn
                (search-forward (car org-tree) nil t)
                (setq org-tree (cdr org-tree)))
            (progn
              (newline)
              (org-insert-struct org-tree)
              (setq org-tree nil)
              ))))
      (newline)
      (org-yank)
      ;; Save and kill the buffer, if it is not the same buffer.
      (when (not (eq this-buffer buffer))
        (save-buffer))
      (message "Subtree archived %s"
               (concat "in file: " (abbreviate-file-name afile))))))

(defun org-insert-struct (struct)
  "TODO"
  (interactive)
  (when struct
    (insert (car struct))
    (newline)
    (org-insert-struct (cdr struct))))