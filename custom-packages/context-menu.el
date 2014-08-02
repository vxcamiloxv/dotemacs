;; The method of displaying buffers and frames has been borrowed from `menu-bar.el`

(global-set-key [mouse-3] 'lawlist-popup-context-menu)

(defvar lawlist-context-menu-map
  (let ((map (make-sparse-keymap "Context Menu")))
  map) "Keymap for the LAWLIST context menu.")

(defun lawlist-popup-context-menu  (event &optional prefix)
  "Popup a context menu."
  (interactive "@e \nP")
    (define-key lawlist-context-menu-map [buffers-menu]
      `(menu-item nil ,(lawlist-buffer-frame-menu t)))
    (popup-menu lawlist-context-menu-map event prefix))

(setq buffers-menu-max-size 20) ;; default is 10

(defun lawlist-buffer-frame-menu (&optional force)
  (let (
      (buffers (buffer-list))
      (frames (frame-list))
      lawlist-buffers-menu)
    (if (and (integerp buffers-menu-max-size)
        (> buffers-menu-max-size 1))
      (if (> (length buffers) buffers-menu-max-size)
        (setcdr (nthcdr buffers-menu-max-size buffers) nil)))
    (setq lawlist-buffers-menu
      (let (alist)
        (dolist (buf buffers)
          (let ((name (buffer-name buf)))
            ;; To show the hidden buffers, comment out:
            ;; (unless (eq ?\s (aref name 0))
            ;; along with the appropriate closing quote.
            (unless (eq ?\s (aref name 0))
              (push (menu-bar-update-buffers-1 (cons buf
                (if (and (integerp buffers-menu-buffer-name-length)
                    (> (length name) buffers-menu-buffer-name-length))
                  (concat
                    (substring name 0 (/ buffers-menu-buffer-name-length 2))
                     "..."
                   (substring name (- (/ buffers-menu-buffer-name-length 2))))
                  name) ))
                alist))))
      (let ((buffers-vec (make-vector (length alist) nil)) (i (length alist)))
        (dolist (pair alist)
          (setq i (1- i))
          (aset buffers-vec i
            (nconc (list (car pair)
              (cons nil nil))
              `(lambda () (interactive)
                (funcall menu-bar-select-buffer-function ,(cdr pair))))))
        (setq lawlist-buffers-menu
          (nconc lawlist-buffers-menu `(
          
      (clipboard-kill-ring-save menu-item "Copy"
        ,(cons "Copy" 'clipboard-kill-ring-save))

      (clipboard-kill-region menu-item "Cut"
        ,(cons "Cut" 'clipboard-kill-region))

      (clipboard-yank menu-item "Paste"
        ,(cons "Paste" 'clipboard-yank))

      (undo-tree-visualize menu-item "Undo tree"
        ,(cons "Undo tree" 'undo-tree-visualize))
        
            (lawlist-buffers menu-item "Buffers" ,(cons 'keymap (list "Select Buffers" buffers-vec)))))))))
    (when (cdr frames)
      (let* (
        (frames-vec (make-vector (length frames) nil))
        (frames-menu (cons 'keymap (list "Select Frame" frames-vec)))
        (i 0) )
        (dolist (frame frames)
          (aset frames-vec i
            (nconc
              (list (frame-parameter frame 'name) (cons nil nil))
              `(lambda () (interactive)
                (menu-bar-select-frame ,frame))))
          (setq i (1+ i)))
        (setq lawlist-buffers-menu
          (nconc lawlist-buffers-menu `(
            (frames-separator "--")
            (frames menu-item "Frames" ,frames-menu))))))
    (setq lawlist-context-menu-entries `(
                            
      (first-separator "--")            
      (major-mode-menu menu-item (symbol-name major-mode)
        ,(mouse-menu-major-mode-map))
      (second-separator "--")
      (yas-mode-menu menu-item (concat "YAS " (symbol-name major-mode))
        ,(gethash major-mode yas--menu-table))
      (third-separator "--")
      (dired-insert-file menu-item "Insert File"
        ,(cons "Insert File" 'dired-insert-file))
      (copy-buffer-file-name-as-kill menu-item "Copy Filename / Path"
        ,(cons "Copy Filename / Path" 'copy-buffer-file-name-as-kill))
      (rename-file-and-buffer menu-item "Rename File"
        ,(cons "Rename File" 'rename-file-and-buffer))
      (fourth-separator "--")
      (first-folder-heading menu-item "Multiple Cursors"
        ,(cons 'keymap (list
          (list
            'mc/cycle-backward
            'menu-item
            "mc/cycle-backward"
            'mc/cycle-backward
            :help " ")
          (list
            'mc/cycle-forward
            'menu-item
            "mc/cycle-forward"
            'mc/cycle-forward
            :help " ")
          (list
            'mc/mark-all-like-this
            'menu-item
            "mc/mark-all-like-this"
            'mc/mark-all-like-this
            :help " ")
          (list
            'mark-previous-like-this-cycle-forward
            'menu-item
            "mark-previous-like-this-cycle-forward"
            'mark-previous-like-this-cycle-forward
            :help " ")
          (list
            'mark-next-like-this-cycle-forward
            'menu-item
            "mark-next-like-this-cycle-forward"
            'mark-next-like-this-cycle-forward
            :help " ")
          (list
            'mc/edit-beginnings-of-lines
            'menu-item
            "mc/edit-beginnings-of-lines"
            'mc/edit-beginnings-of-lines
            :help " ")
          (list
            'mc/edit-ends-of-lines
            'menu-item
            "mc/edit-ends-of-lines"
            'mc/edit-ends-of-lines
            :help " ") )))
      (third-separator "--")
      (second-folder-heading menu-item "B.B.D.B."
        ,(cons 'keymap (list
          (list
            'bbdb-info
            'menu-item
            "info see bbdb.texinfo"
            'bbdb-info
            :help " ")
          (list
            'bbdb-help
            'menu-item
            "help"
            'bbdb-help
            :help " ")
          (list
            'bbdb-bury-buffer
            'menu-item
            "bury-buffer"
            'bbdb-bury-buffer
            :help " ")
          (list
            'bbdb-notes
            'menu-item
            "search notes"
            'bbdb-notes
            :help " ")
          (list
            'bbdb-net
            'menu-item
            "search net"
            'bbdb-net
            :help " ")
          (list
            'bbdb-company
            'menu-item
            "search company name"
            'bbdb-company
            :help " ")
          (list
            'bbdb-name
            'menu-item
            "search name"
            'bbdb-name
            :help " ")
          (list
            'bbdb-refile-record
            'menu-item
            "merge records"
            'bbdb-refile-record
            :help " ")
          (list
            'bbdb-save-db
            'menu-item
            "save-db"
            'bbdb-save-db
            :help " ")
          (list
            'bbdb-send-mail
            'menu-item
            "compose email"
            'bbdb-send-mail
            :help " ")
          (list
            'bbdb-omit-record
            'menu-item
            "hide search result"
            'bbdb-omit-record
            :help " ")
          (list
            'bbdb-toggle-records-display-layout
            'menu-item
            "expand / collapse"
            'bbdb-toggle-records-display-layout
            :help " ")
          (list
            'bbdb-next-record
            'menu-item
            "next-record"
            'bbdb-next-record
            :help " ")
          (list
            'bbdb-prev-record
            'menu-item
            "prev-record"
            'bbdb-prev-record
            :help " ")
          (list
            'bbdb-transpose-fields "transpose-like-fields"
            'menu-item
            "transpose-like-fields"
            'bbdb-transpose-fields
            :help " ")
          (list
            'bbdb-delete-current-field-or-record
            'menu-item
            "delete field/record"
            'bbdb-delete-current-field-or-record
            :help " ")
          (list
            'bbdb-record-edit-notes
            'menu-item
            "edit-notes"
            'bbdb-record-edit-notes
            :help " ")
          (list
            'bbdb-insert-new-field
            'menu-item
            "insert-new-field"
            'bbdb-insert-new-field
            :help " ")
          (list
            'bbdb-edit-current-field
            'menu-item
            "edit-current-field"
            'bbdb-edit-current-field
            :help " ")
          (list
            'bbdb-create
            'menu-item
            "create"
            'bbdb-create
            :help " ")
          (list
            'bbdb
            'menu-item
            "search"
            'bbdb
            :help " "))))))
    (setq lawlist-buffers-menu (nconc lawlist-buffers-menu lawlist-context-menu-entries))
    (setcdr lawlist-context-menu-map (cons "Context Menu" lawlist-buffers-menu)) ))

    
(provide 'context-menu)

