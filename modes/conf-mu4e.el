;;; Code:
(require 'mu4e)
(require 'mu4e-contrib)

;; Custom vars
(defcustom distopico:mu4e-inbox-update-modeline-interval 300
  "Inbox update interval."
  :type 'integer
  :group 'hardware)

(defcustom distopico:message-attachment-reminder
  "Are you sure you want to send this message without any attachment? "
  "The default question asked when trying to send a message \
containing `distopico:message-attachment-intent-re' without an
actual attachment.")

(defcustom distopico:message-attachment-intent-re
  (regexp-opt '("I attach"
                "I have attached"
                "I've attached"
                "I have included"
                "I've included"
                "see the attached"
                "see the attachment"
                "attached file"
                "archivo incluido"
                "adjunto archivo"
                "ver adjunto"
                "adjunto va"
                "archivo adjunto"))
  "A regex which - if found in the message, and if there is no \
attachment - should launch the no-attachment warning.")

(defvar distopico:mu4e-new-mail nil
  "Boolean to represent if there is new mail.")
(defvar distopico:mu4e-mode-line-format nil
  "String to display in the mode line.")
(defvar distopico:mu4e-update-timer nil
  "Interval timer object.")
(defvar distopico:mu4e-mail-address-list)
(defvar distopico:mu4e-account-alist)

;; General mu4e config
(setq mu4e-maildir "~/.mail"
      ;;mu4e-get-mail-command "~/.emacs.d/scripts/offlineimap_notify.py"
      mu4e-get-mail-command "flock -n /tmp/offlineimap.lock offlineimap"
      mu4e-confirm-quit nil
      mu4e-compose-keep-self-cc nil
      mu4e-view-prefer-html t
      mu4e-view-use-gnus t
      ;;mu4e-html2text-command "html2text"
      mu4e-compose-complete-addresse t
      mu4e-compose-dont-reply-to-self t
      mu4e-hide-index-messages t
      mu4e-headers-auto-update t
      mu4e-use-fancy-chars t
      mu4e-split-view 'horizontal
      mu4e-update-interval 300
      mu4e-headers-visible-lines 20
      mu4e-html2text-command 'mu4e-shr2text
      mu4e-headers-leave-behavior 'ask
      mu4e~main-buffer-name "*mu4e-main*"
      mu4e-headers-fields '((:human-date    .   12)
                            (:flags         .   10)
                            (:mailing-list  .   10)
                            (:from          .   22)
                            (:subject       .   nil)))

;; Compose mail with gnus.
(setq read-mail-command 'gnus
      mail-user-agent 'gnus-user-agent);;gnus-user-agent mu4e-user-agent

;; Bookmarks and shortcuts
(setq mu4e-maildir-shortcuts
      '( ("/1-Distopico/INBOX"        . ?d)
         ("/2-vXcamiloXv/INBOX"       . ?c)
         ("/3-AccionVisual/INBOX"     . ?a)
         ("/4-RadioLiberacion/INBOX"  . ?r)
         ("/5-TienditaVegan/INBOX"    . ?t)
         ))

(setq mu4e-bookmarks
      '(("maildir:/\/*\/INBOX/" "All Inbox" ?i)
        ("maildir:/1-Distopico/INBOX" "[Distopico] All" ?D)
        ("flag:unread AND maildir:/1-Distopico/INBOX" "[Distopico] Unread Inbox" ?d)
        ("maildir:/2-vXcamiloXv/INBOX" "[vXcamiloXv] All" ?C)
        ("flag:unread AND maildir:/2-vXcamiloXv/INBOX" "[vXcamiloXv] Unread Inbox" ?c)
        ("flag:unread AND NOT flag:trashed AND NOT maildir:/\/*\/Spam*/ AND NOT maildir:/\/*\/Trash*/" "Unread All Inbox" ?u)
        ("flag:unread" "Unread All" ?U)
        ("date:today..now AND NOT maildir:/\/*\/Spam*/" "Today's messages" ?t)
        ("date:7d..now AND NOT maildir:/\/*\/Spam*/" "Last 7 days" ?w)
        ("mime:image/* AND NOT maildir:/\/*\/Spam*/" "Messages with images" ?p)
        ("flag:unread AND maildir:/\/*\/Spam*/" "Unread spam" ?s)))

;; Enable account structure
(require 'mu4e-maildirs-extension)
(mu4e-maildirs-extension)
(setq mu4e-maildirs-extension-use-bookmarks t)

;; Custom marks
(setq mu4e-headers-draft-mark             '("D" . "⚒ ")
      mu4e-headers-flagged-mark           '("F" . "✚ ")
      mu4e-headers-new-mark               '("N" . "✱ ")
      mu4e-headers-passed-mark            '("P" . "❯ ")
      mu4e-headers-replied-mark           '("R" . "❮ ")
      mu4e-headers-seen-mark              '("S" . "✔ ")
      mu4e-headers-trashed-mark           '("T" . "⏚ ")
      mu4e-headers-attach-mark            '("a" . "✉ ")
      mu4e-headers-encrypted-mark         '("x" . "⚴ ")
      mu4e-headers-signed-mark            '("s" . "☡ ")
      mu4e-headers-unread-mark            '("u" . "⚐ ")
      ;; Prefix
      mu4e-headers-empty-parent-prefix    '("-" . "○")
      mu4e-headers-first-child-prefix     '("\\" . "┗━❯")
      mu4e-headers-has-child-prefix       '("+" . "┗◉")
      mu4e-headers-duplicate-prefix       '("=" . "⚌")
      mu4e-headers-default-prefix         '("|" . "┃"))

;; Fix hl unread messages with mu4e 0.9.15
(setq mu4e-maildirs-extension-propertize-func 'distopico:mu4e-maildirs-extension-propertize-func)

;; Config Mesagges
(setq message-kill-buffer-on-exit t
      message-signature-insert-empty-line t
      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format "On %Y-%m-%d, %f wrote:\n" ;;message-citation-line-format "%N @ %Y-%m-%d %H:%M %Z:\n" "On %Y-%m-%d, %f wrote:" "On %Y-%m-%d %a at %H:%M %Z, %f wrote:\n"
      )

;; Enable or disabled images
(setq mu4e-view-show-images nil
      mu4e-view-image-max-width 200
      mu4e-view-image-max-height 200)
(setq mu4e-msg2pdf "/usr/bin/msg2pdf")

(when (fboundp 'org-mime-htmlize)
  (add-hook 'message-mode-hook (lambda () (local-set-key "\C-c\M-o" 'org-mime-htmlize))))

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;;Org config
(require 'org-mu4e)
(defalias 'org-mail 'org-mu4e-compose-org-mode)
(setq org-mu4e-convert-to-html t
      org-mu4e-link-query-in-headers-mode t
      mu4e-org-contacts-file  "~/Documents/org/contacts.org")

;; Actions
(add-to-list 'mu4e-view-actions
             '("View in browser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions
             '("add contact org" . distopico:mu4e-action-add-org-contact) t)

(add-to-list 'mu4e-headers-actions
             '("add contact org" . distopico:mu4e-action-add-org-contact) t)

;; Default dir attachment
(setq mu4e-attachment-dir  "~/Downloads")

;; Send async
(require 'smtpmail-async)
(setq message-send-mail-function 'smtpmail-send-it) ;;allow: async-smtpmail-send-it or smtpmail-send-it

;; Not start in queuing mode
(setq smtpmail-queue-mail nil
      smtpmail-queue-dir   (concat mu4e-maildir "/Queue/cur")
      smtpmail-smtp-user t)

;; Config by mu4e Account
(ignore-errors
  (load (expand-file-name ".conf-private.gpg" "~/") t))

;; Default Account
(setq mu4e-sent-folder "/1-Distopico/Sent"
      mu4e-drafts-folder "/1-Distopico/Drafts"
      mu4e-trash-folder "/1-Distopico/Trash")

;; mu4e email list
(setq mu4e-user-mail-address-list distopico:mu4e-mail-address-list)

;; Sign/Encrypt
(setq mml2015-encrypt-to-self t
      mml2015-sign-with-sender t)
;; (setq mm-sign-option 'guided
;;       mm-encrypt-option 'guided
;;       epg-user-id "")

;; Custom keymap
(define-key mu4e-main-mode-map "r" 'distopico:mu4e-maildirs-force-update)
(define-key mu4e-main-mode-map (kbd "C-q") 'distopico:mu4e-close)
(define-key mu4e-headers-mode-map (kbd "C-q") 'distopico:mu4e-kill-close)
(define-key mu4e-headers-mode-map (kbd "C-x k") 'distopico:mu4e-kill-close)
(define-key mu4e-headers-mode-map (kbd "C-c o c") 'org-mu4e-store-and-capture)
(define-key mu4e-view-mode-map (kbd "C-q") 'distopico:mu4e-kill-close)
(define-key mu4e-view-mode-map (kbd "C-x k") 'distopico:mu4e-kill-close)
(define-key mu4e-view-mode-map (kbd "C-c o c") 'org-mu4e-store-and-capture)

;;------------------
;; Useful functions
;;------------------

;; hl unread messages
(defun distopico:mu4e-maildirs-extension-propertize-func (m)
  "Propertize the maildir text using M plist."
  (let ((unread (or (plist-get m :unread) 0))
        (total (or (plist-get m :total) 0)))
    (setq msg-face (cond
                    ((> unread 0) 'mu4e-maildirs-extension-maildir-hl-face)
                    (t            'mu4e-maildirs-extension-maildir-face)))
    (format "\t%s%s %s (%s/%s)"
            (propertize (plist-get m :indent) 'face msg-face)
            (propertize (plist-get m :prefix) 'face msg-face)
            (propertize (plist-get m :name) 'face msg-face)
            (propertize (number-to-string unread) 'face msg-face)
            (propertize (number-to-string total) 'face msg-face))))

;; Set Account
(defun distopico:mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                distopico:mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) distopico:mu4e-account-alist)
                             nil t nil nil (caar distopico:mu4e-account-alist))))
         (account-vars (cdr (assoc account distopico:mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

;; Starting the whole mu4e in its own frame
(defun mu4e-in-new-frame ()
  "Start mu4e in new frame."
  (interactive)
  (select-frame (make-frame))
  (mu4e))

(defun distopico:mu4e-open ()
  "Open mu4e and remove other windows, save the state."
  (interactive)
  (open-buffer-delete-others mu4e~main-buffer-name :mu4e-fullscreen 'mu4e))

(defun distopico:mu4e-close ()
  "Restore the previous window configuration and burry buffer."
  (interactive)
  (bury-buffer-restore-prev :mu4e-fullscreen))

(defun distopico:mu4e-kill-close ()
  "Kill buffer and reload maildirs if headers."
  (interactive)
  (if (equal (buffer-name) "*mu4e-headers*")
      (progn
        (unless (eq major-mode 'mu4e-headers-mode)
          (mu4e-error "Must be in mu4e-headers-mode (%S)" major-mode))
        (mu4e-mark-handle-when-leaving)
        (let ((curbuf (current-buffer)) (curwin (selected-window))
              (headers-visible))
          (walk-windows
           (lambda (win)
             (with-selected-window win
               ;; if we the view window connected to this one, kill it
               (when (and (not (one-window-p win)) (eq mu4e~headers-view-win win))
                 (delete-window win)
                 (setq mu4e~headers-view-win nil)))
             ;; and kill any _other_ (non-selected) window that shows the current
             ;; buffer
             (when (and
                    (eq curbuf (window-buffer win)) ;; does win show curbuf?
                    (not (eq curwin win))	        ;; it's not the curwin?
                    (not (one-window-p)))           ;; and not the last one?
               (delete-window win))))
          (kill-buffer (current-buffer))
          (mu4e~main-view))
        (distopico:mu4e-maildirs-force-update))
    (if (equal (buffer-name) "*mu4e-view*")
        (progn
          (kill-buffer (current-buffer))
          (delete-other-windows))
      (kill-buffer (current-buffer)))))

;; Toggle related
(defun distopico:mu4e-toggle-headers-include-related ()
  "Toggle `mu4e-headers-include-related' and refresh."
  (interactive)
  (setq mu4e-headers-include-related
        (not mu4e-headers-include-related))
  (mu4e-headers-rerun-search))

(define-key 'mu4e-headers-mode-map "o"
  'distopico:mu4e-toggle-headers-include-related)

;; Custom add contact
(defun distopico:mu4e-action-add-org-contact (msg)
  "Add an org-contact entry based on the From: address of the \
current message `MSG' (in headers or view), You need to set
`mu4e-org-contacts-file' to the full path to the file where you
store your org-contacts."
  (unless (require 'org-capture nil 'noerror)
    (mu4e-error "The org-capture is not available"))
  (unless mu4e-org-contacts-file
    (mu4e-error "`mu4e-org-contacts-file' is not defined"))
  (let* ((sender (car-safe (mu4e-message-field msg :from)))
         (name (car-safe sender)) (email (cdr-safe sender))
         (blurb
          (format
           (concat
            "** %s%%? \n"
            ":PROPERTIES:\n"
            ":EMAIL: %s\n"
            ":NICK: \n"
            ":END:\n\n")
           (or name email "")
           (or email "")))
         (key "mu4e-add-org-contact-key")
         (head "General")
         (org-capture-templates
          (append org-capture-templates
                  (list (list key "contacts" 'entry
                              (list 'file+headline mu4e-org-contacts-file head) blurb)))))
    (message "%S" org-capture-templates)
    (when (fboundp 'org-capture)
      (org-capture nil key))))

(defun distopico:mu4e-unread-mail-query ()
  "The query to look for unread messages in all account INBOXes."
  (mapconcat
   (lambda (acct)
     (let ((name (car acct)))
       (format "%s"
               (mapconcat (lambda (fmt)
                            (format fmt name))
                          '("flag:unread AND maildir:'/%s/INBOX'")
                          " "))))
   distopico:mu4e-account-alist
   " OR "))

(defun distopico:mu4e-new-mail-p ()
  "Predicate for if there is new mail or not in Boolean."
  (not (eq 0 (string-to-number
              (distopico:mu4e-get-unread-command)))))

(defun distopico:mu4e-get-unread-command ()
  "Get unread messages fro mu4e binary."
  (replace-regexp-in-string
   "[\t\n\r]" ""
   (shell-command-to-string
    (concat "echo -n $( " mu4e-mu-binary " find "
            (distopico:mu4e-unread-mail-query)
            " 2>/dev/null | wc -l )"))))

(defun distopico:mu4e-inbox-update ()
  "Print tooltip help and icon for unread messages."
  (interactive)
  (setq distopico:mu4e-mode-line-format
        (let ((unread (distopico:mu4e-get-unread-command)))
          (let ((unread-string
                 (if (string= "0" unread) ""
                   (if (window-system) "--"
                     (format "[✉ %s]" unread)))))

            (propertize
             unread-string
             'display (if (boundp 'img:tron-email)
                          img:tron-email
                        "✉")
             'local-map  (make-mode-line-mouse-map 'mouse-1 #'distopico:mu4e-open)
             'help-echo (format "mu4e :: %s unread messages" unread)))))
  (force-mode-line-update)
  (sit-for 0))

(defun distopico:mu4e-maildirs-force-update()
  "Clear cache and insert maildirs summary and reload."
  (interactive)
  (mu4e-message "Updating index & cache...")
  (mu4e-update-index)
  (mu4e-maildirs-extension-force-update '(16))
  ;;(mu4e-maildirs-extension-index-updated-handler)
  (distopico:mu4e-inbox-update))

(defun distopico:message-attachment-present-p ()
  "Return t if an attachment is found in the current message."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (search-forward "<#part" nil t) t))))

(defun distopico:message-warn-if-no-attachments ()
  "Ask the user if s?he wants to send the message even though \
there are no attachments.
from: http://mbork.pl/2016-02-06_An_attachment_reminder_in_mu4e"
  (when (and (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (re-search-forward distopico:message-attachment-intent-re nil t)))
             (not (distopico:message-attachment-present-p)))
    (unless (y-or-n-p distopico:message-attachment-reminder)
      (keyboard-quit))))

(defun distopico:mu4e-index-updated-hook ()
  "Hooen when come some new message."
  (message "new message")
  ;;(start-process "mu4e-update" nil (in-emacs-d "/scripts/notify_mail.sh") (number-to-string mu4e-update-interval))
  (call-process "/bin/bash" nil 0 nil (in-emacs-d "/scripts/notify_mail.sh") (number-to-string mu4e-update-interval))
  ;; (shell-command-to-string (concat (in-emacs-d "/scripts/notify_mail.sh") " " (number-to-string mu4e-update-interval)))
  (distopico:mu4e-inbox-update))

(defun distopico:mu4e-view-mode-hook ()
  "Enable/disable some mode in `mu4e-view-mode'."
  (buffer-face-set 'mu4e-view-body-face)
  (tabbar-local-mode 1)
  (visual-line-mode))

(defun distopico:mu4e-compose-mode-hook ()
  "Enable/disable some mode in `mu4e-compose-mode'."
  (distopico:mu4e-view-mode-hook)
  (mml-secure-message-sign-pgpmime))

(defun distopico:mu4e-init-load-hook ()
  "Run mu4e in startup."
  (mu4e t)
  (distopico:mu4e-mode-line t))

;; Custom modes
(define-minor-mode distopico:mu4e-mode-line
  "Minor mode Toggle inbox status display in mode line"
  :global t :group 'hardware
  (setq distopico:mu4e-mode-line-format "")
  (and distopico:mu4e-update-timer (cancel-timer distopico:mu4e-update-timer))

  (if (not distopico:mu4e-mode-line-format)
      (message "Disabled mu4e mode line..")
    (setq distopico:mu4e-update-timer
          (run-at-time nil distopico:mu4e-inbox-update-modeline-interval #'distopico:mu4e-inbox-update))
    (distopico:mu4e-inbox-update)))

;;-------------------
;; Hooks
(add-hook 'mu4e-compose-pre-hook #'distopico:mu4e-set-account)
(add-hook 'mu4e-index-updated-hook #'distopico:mu4e-index-updated-hook)
(add-hook 'mu4e-view-mode-hook #'distopico:mu4e-view-mode-hook)
(add-hook 'mu4e-compose-mode-hook #'distopico:mu4e-compose-mode-hook)
(add-hook 'message-send-hook #'distopico:message-warn-if-no-attachments)
(add-hook 'distopico:after-init-load-hook #'distopico:mu4e-init-load-hook)

(provide 'conf-mu4e)
