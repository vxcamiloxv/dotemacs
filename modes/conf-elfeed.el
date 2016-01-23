
;;; Code:

(require 'elfeed)
(require 'elfeed-org)

;; Control vars
(defcustom distopico:elfeed-update-interval 60
  "Feeds update interval"
  :group 'elfeed
  :type 'integer)

(defcustom distopico:elfeed-entry-pane-position 'bottom
  "Position of the popup entry pane."
  :group 'elfeed
  :type '(choice (const left) (const right) (const top) (const bottom)))

(defcustom distopico:elfeed-entry-pane-size 0.50
  "Size (width or height, depending on position) of the popup entry pane."
  :group 'elfeed
  :type 'number)

(defvar distopico:elffed-update-timer nil
  "Interval timer object.")

(defvar distopico:elfeed-mode-line-format nil
  "Format to display in the mode line.")

;; Basic
(elfeed-org)
(setq rmh-elfeed-org-files (list "~/Documents/org/feeds.org"))

;; Tweaks
(setq elfeed-show-entry-switch #'distopico:elfeed-switch-pane
      elfeed-show-entry-delete #'distopico:elfeed-delete-pane)

;; Change titles
(defadvice elfeed-search-update (before nullprogram activate)
  (let ((feed (elfeed-db-get-feed "https://feeds.feedburner.com/DebianHackers")))
    (setf (elfeed-feed-title feed) "DebianHackers")))

;; Custom key map
(define-key elfeed-search-mode-map (kbd "C-q") 'distopico:elfeed-close)
(define-key elfeed-search-mode-map (kbd "M-q") 'kill-this-buffer)
(define-key elfeed-search-mode-map (kbd "q") 'distopico:elfeed-close)
(define-key elfeed-search-mode-map "f" 'distopico:elfeed-group-filter)
(define-key elfeed-search-mode-map "w"
  (lambda ()
    (interactive)
    (elfeed-search-set-filter "@1-week-ago +unread")))

;; Functions
(defun distopico:elfeed-open ()
  (interactive)
  (open-buffer-delete-others "*elfeed-search*" :elfeed-fullscreen 'elfeed))

(defun distopico:elfeed-close ()
  "Restores the previous window configuration and burry buffer"
  (interactive)
  (bury-buffer-restore-prev :elfeed-fullscreen)
  (distopico:elfeed-delete-pane t))

(defun distopico:elfeed-switch-pane (buff)
  "Display BUFF in a popup window."
  (popwin:popup-buffer buff
                       :position distopico:elfeed-entry-pane-position
                       :width distopico:elfeed-entry-pane-size
                       :height distopico:elfeed-entry-pane-size
                       :stick t
                       :dedicated t))

(defun distopico:elfeed-delete-pane (&optional nodelete)
  "Delete the *elfeed-entry* split pane."
  (interactive)
  (let* ((buff (get-buffer "*elfeed-entry*"))
         (window (get-buffer-window buff)))
    (kill-buffer buff)
    (if(not nodelete)
        (delete-window window))))

(defun distopico:elfeed-total-unread ()
  (let ((counts 0))
    (with-elfeed-db-visit (e _)
      (let ((tags (elfeed-entry-tags e)))
        (when (memq 'unread tags)
          (dolist (tag tags)
            (unless (eq tag 'unread)
              (cl-incf counts))))))
    counts))

(defun distopico:elfeed-tag-unread ()
  (let ((counts (make-hash-table)))
    (with-elfeed-db-visit (e _)
      (let ((tags (elfeed-entry-tags e)))
        (when (memq 'unread tags)
          (dolist (tag tags)
            (unless (eq tag 'unread)
              (cl-incf (gethash tag counts 0)))))))
    (cl-loop for tag hash-keys of counts using (hash-values count)
             collect (cons tag count))))

(defun distopico:elfeed-unread-update ()
  "Print tooltip help and icon for unread feeds"
  (interactive)
  (setq distopico:elfeed-mode-line-format
        (let ((unread
               (number-to-string (distopico:elfeed-total-unread))))
          (let ((unread-string (if (string= "0" unread) "" (format "[⇑ %s]" unread) )))
            (if (window-system)
                (setq unread-string "--"))
            (propertize
             unread-string
             'display img:tron-feed
             'local-map  (make-mode-line-mouse-map 'mouse-1 #'distopico:elfeed-open)
             'help-echo (format "elfeed :: %s unread" unread)
             ))))
  (force-mode-line-update)
  (sit-for 0))

(defun distopico:elfeed-group-filter ()
  (interactive)
  (message "%S" (distopico:elfeed-total-unread))
  (let ((default-filter "@6-months-ago +unread")
        tags-alist)
    (setq tags-alist
          (append
           (list (cons "*NONE*" default-filter))
           (mapcar
            #'(lambda (x)
                (let ((tag-name (symbol-name (car x)))
                      (num-str (number-to-string (cdr x))))
                  (cons (concat tag-name " (" num-str ")")
                        (concat default-filter " +" tag-name))))
            (distopico:elfeed-tag-unread))))
    (unwind-protect
        (let ((elfeed-search-filter-active :live)
              (choose (completing-read
                       (concat "Filter: " default-filter " +")
                       (mapcar #'car tags-alist))))
          (setq elfeed-search-filter
                (cdr (assoc choose tags-alist))))
      (elfeed-search-update :force))))

(defun distopico:elfeed-init-load-hook ()
  "Run hook after load init.el file"
  (distopico:elfeed-mode-line t))

;; Custom modes
(define-minor-mode distopico:elfeed-mode-line
  "Minor mode Toggle unread feeds display in mode line"
  :global t :group 'hardware
  (setq distopico:elfeed-mode-line-format "")
  (and distopico:elffed-update-timer (cancel-timer distopico:elffed-update-timer))

  (if (not distopico:elfeed-mode-line-format)
      (message "Disabled elfeed mode line..")
    (setq distopico:elffed-update-timer
          (run-at-time nil distopico:elfeed-update-interval 'distopico:elfeed-unread-update))
    (distopico:elfeed-unread-update)))

;; Hooks
(add-hook 'distopico:after-init-load-hook 'distopico:elfeed-init-load-hook)


(provide 'conf-elfeed)
