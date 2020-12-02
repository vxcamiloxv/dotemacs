;;; Code:
(require 'elfeed)
(require 'elfeed-show)
(require 'elfeed-org)

;; Control vars
(defcustom distopico:elfeed-update-interval 300
  "Feeds update interval."
  :group 'elfeed
  :type 'integer)

(defcustom distopico:elfeed-update-at-time "9:30"
  "Feeds update in specific hour."
  :group 'elfeed
  :type 'string)

(defcustom distopico:elfeed-update-modeline-interval 60
  "Feeds update interval of modeline."
  :group 'elfeed
  :type 'integer)

(defcustom distopico:elfeed-entry-pane-position 'right
  "Position of the popup entry pane."
  :group 'elfeed
  :type '(choice (const left) (const right) (const top) (const bottom)))

(defcustom distopico:elfeed-entry-pane-size 0.50
  "Size (width or height, depending on position) of the popup entry pane."
  :group 'elfeed
  :type 'number)

(defvar distopico:elfeed-update-timer nil
  "Interval timer object.")

(defvar distopico:elfeed-mode-line-format nil
  "Format to display in the mode line.")

(defvar distopico:elfeed-old-entries-read (elfeed-make-tagger :before "2 weeks ago" :remove 'unread)
  "Mark as unread old entries.")

;; Basic
(setq rmh-elfeed-org-files (list "~/Documents/org/feeds.org"))

;; Tweaks
(setq elfeed-show-entry-switch #'distopico:elfeed-switch-pane
      elfeed-show-entry-delete #'distopico:elfeed-delete-pane)

;; Prevent 'Queue timeout exceeded'
(setf url-queue-timeout 30)

;; Custom key map
(define-key elfeed-search-mode-map (kbd "C-q") 'distopico:elfeed-close)
(define-key elfeed-search-mode-map (kbd "M-q") 'kill-this-buffer)
(define-key elfeed-search-mode-map "q" 'distopico:elfeed-close)
(define-key elfeed-search-mode-map "f" 'distopico:elfeed-group-filter-unread)
(define-key elfeed-search-mode-map "F" 'distopico:elfeed-group-filter-all)
(define-key elfeed-search-mode-map "i" 'distopico:elfeed-org-update)
(define-key elfeed-search-mode-map "W" 'distopico:elfeed-week)
(define-key elfeed-search-mode-map "B" 'distopico:elfeed-bookmarks)
(define-key elfeed-search-mode-map (kbd "C-b") 'distopico:elfeed-search-toggle-bookmark)

(define-key elfeed-show-mode-map "q" 'distopico:elfeed-delete-pane)
(define-key elfeed-show-mode-map "B" 'distopico:elfeed-show-toggle-bookmark)
(define-key elfeed-show-mode-map (kbd "M-<up>") 'elfeed-show-prev)
(define-key elfeed-show-mode-map (kbd "M-<down>") 'elfeed-show-next)

;; Functions
(defun distopico:elfeed-open ()
  "Custom function to open elfeed and delete others windows."
  (interactive)
  (let ((elfeed-search-buffer "*elfeed-search*"))
    (open-buffer-delete-others elfeed-search-buffer :elfeed-fullscreen 'elfeed t)
    (with-current-buffer (get-buffer elfeed-search-buffer)
      (unless (eq major-mode 'elfeed-search-mode)
        (elfeed-search-mode))
      (distopico:elfeed-unread-update))))

(defun distopico:elfeed-close ()
  "Restore the previous window configuration and burry buffer."
  (interactive)
  (bury-buffer-restore-prev :elfeed-fullscreen)
  (distopico:elfeed-unread-update))

(defun distopico:elfeed-week ()
  "List unread entries of last week."
  (interactive)
  (elfeed-search-set-filter "@1-week-ago +unread"))

(defun distopico:elfeed-bookmarks ()
  "Return entrues with tag `bookmark'."
  (interactive)
  (elfeed-search-set-filter "+bookmark"))

(defun distopico:elfeed-search-toggle-bookmark (tag)
  "Toggle TAG bookmark on all selected entries."
  (interactive (list (intern "bookmark")))
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             when (elfeed-tagged-p tag entry)
             do (elfeed-untag entry tag)
             else do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun distopico:elfeed-show-toggle-bookmark (&rest tag)
  "Toggle TAG bookmark to the displayed entry."
  (interactive (list (intern "bookmark")))
  (let ((entry elfeed-show-entry))
    (if (memq 'bookmark (elfeed-entry-tags entry))
        (apply #'elfeed-untag entry tag)
      (apply #'elfeed-tag entry tag))
    (with-current-buffer (elfeed-search-buffer)
      (elfeed-search-update-entry entry))
    (elfeed-show-refresh)))

(defun distopico:elfeed-switch-pane (buff)
  "Display BUFF in a popup window."
  (popwin:popup-buffer
   buff
   :position distopico:elfeed-entry-pane-position
   :width distopico:elfeed-entry-pane-size
   :height distopico:elfeed-entry-pane-size
   :stick t
   :dedicated t))

(defun distopico:elfeed-delete-pane (&optional nodelete)
  "Delete the *elfeed-entry* split pane, optional `NODELETE'."
  (interactive)
  (let* ((buff (get-buffer "*elfeed-entry*"))
         (window (get-buffer-window buff)))
    (kill-buffer buff)
    (if(not nodelete)
        (delete-window window))))

(defun distopico:elfeed-total-unread ()
  "Return total of `unread' entries."
  (let ((counts 0))
    (with-elfeed-db-visit (e _)
      (let ((tags (elfeed-entry-tags e)))
        (when (memq 'unread tags)
          (dolist (tag tags)
            (unless (eq tag 'unread)
              (cl-incf counts))))))
    counts))

(defun distopico:elfeed-tag-entries (&optional unread)
  "Get tag of entries, optional return `UNREAD' also."
  (let ((counts (make-hash-table)))
    (with-elfeed-db-visit (e _)
      (let ((tags (elfeed-entry-tags e)))
        (if unread
            (progn
              (when (memq 'unread tags)
                (dolist (tag tags)
                  (unless (eq tag 'unread)
                    (cl-incf (gethash tag counts 0))))))
          (progn
            (dolist (tag tags)
              (cl-incf (gethash tag counts 0)))))))
    (cl-loop for tag hash-keys of counts using (hash-values count)
             collect (cons tag count))))

(defun distopico:elfeed-unread-update ()
  "Print tooltip help and icon for unread feeds."
  (interactive)
  (setq distopico:elfeed-mode-line-format
        (let ((unread
               (number-to-string (distopico:elfeed-total-unread))))
          (let ((unread-string
                 (if (string= "0" unread) ""
                   (if (window-system) "--"
                     (format "[⇑ %s]" unread)))))
            (propertize
             unread-string
             'display (if (boundp 'img:tron-feed)
                          img:tron-feed
                        "⇑")
             'local-map  (make-mode-line-mouse-map 'mouse-1 #'distopico:elfeed-open)
             'help-echo (format "elfeed :: %s unread" unread)
             ))))
  (force-mode-line-update)
  (sit-for 0))

(defun distopico:elfeed-group-filter (&optional unread)
  "Filter by group, optional show `UNREAD'."
  (let ((default-filter
          (concat
           "@6-months-ago"
           (if unread
               " +unread"
             "")))
        (default-value "*NONE*")
        tags-alist)
    (setq tags-alist
          (append
           (list (cons default-value default-filter))
           (mapcar
            #'(lambda (x)
                (let ((tag-name (symbol-name (car x)))
                      (num-str (number-to-string (cdr x))))
                  (cons (concat tag-name " (" num-str ")")
                        (concat default-filter " +" tag-name))))
            (distopico:elfeed-tag-entries unread))))
    (unwind-protect
        (let ((elfeed-search-filter-active :live)
              (choose (completing-read
                       (concat "Filter: " default-filter " +")
                       (mapcar #'car tags-alist)
                       nil nil nil t default-value)))
          (setq elfeed-search-filter
                (cdr (assoc choose tags-alist))))
      (elfeed-search-update :force))))

(defun distopico:elfeed-group-filter-unread ()
  "Show only unread feeds."
  (interactive)
  (distopico:elfeed-group-filter t))

(defun distopico:elfeed-group-filter-all ()
  "Show all feeds."
  (interactive)
  (distopico:elfeed-group-filter))

(defun distopico:elfeed-org-update ()
  "Force to update feed in `org-mode' to elfeed list."
  (interactive)
  (rmh-elfeed-org-process rmh-elfeed-org-files rmh-elfeed-org-tree-id)
  (elfeed-update))

(defun distopico:elfeed-new-entry-hook (entry)
  "Run hook after get new feed `ENTRY'."
  ;; TODO: notify new entry
  (distopico:elfeed-unread-update)
  (distopico:elfeed-old-entries-read entry))

(defun distopico:elfeed-init-load-hook ()
  "Run hook after load init.el file."
  (distopico:elfeed-run)
  (distopico:elfeed-mode-line t)
  ;; Update elfeed only when idle emacs and in specific hour time
  (run-at-time distopico:elfeed-update-at-time nil #'elfeed-update)
  (run-with-idle-timer distopico:elfeed-update-interval t #'elfeed-update))

(defun distopico:elfeed-show-mode-hook ()
  "Hook when enter in show mode."
  (tabbar-local-mode 1)
  (buffer-face-set 'message-read-face)
  (visual-line-mode))

;;;###autoload
(defun distopico:elfeed-run ()
  "Run elfeed without enter in buffer."
  (interactive)
  (elfeed-org)
  (elfeed)
  (elfeed-update)
  ;; (distopico:elfeed-org-update)
  ;; (let ((elfeed-search-buffer (get-buffer-create "*elfeed-search*")))
  ;;  (with-current-buffer elfeed-search-buffer
  ;;  (unless (eq major-mode 'elfeed-search-mode)
  ;;  (elfeed-search-mode))
  ;; (elfeed-search-update)))
  )

;; Custom modes
(define-minor-mode distopico:elfeed-mode-line
  "Minor mode Toggle unread feeds display in mode line"
  :global t :group 'hardware
  (setq distopico:elfeed-mode-line-format "")
  (and distopico:elfeed-update-timer (cancel-timer distopico:elfeed-update-timer))

  (if (not distopico:elfeed-mode-line-format)
      (message "Disabled elfeed mode line..")
    (setq distopico:elfeed-update-timer
          (run-at-time nil distopico:elfeed-update-modeline-interval #'distopico:elfeed-unread-update))
    (distopico:elfeed-unread-update)))

;; Hooks
(add-hook 'elfeed-show-mode-hook 'distopico:elfeed-show-mode-hook)
(add-hook 'elfeed-new-entry-hook 'distopico:elfeed-new-entry-hook)
(add-hook 'distopico:after-init-load-hook 'distopico:elfeed-init-load-hook)

(provide 'conf-elfeed)
;;; conf-elfeed.el ends here
