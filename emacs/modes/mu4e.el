(require 'mu4e)
(require 'org-mu4e)

;; Default MailDir
(setq mu4e-maildir "~/.mail")


;; General mu4e config
(setq mu4e-get-mail-command "~/.emacs.d/scripts/offlineimap_notify.py"
      mu4e-update-interval 300
      mu4e-view-prefer-html t
      mu4e-html2text-command "html2text"
      mu4e-compose-dont-reply-to-self t
      mu4e-compose-keep-self-cc nil
      mu4e-confirm-quit nil
      mu4e-hide-index-messages t
      mu4e-use-fancy-chars t
      mu4e~main-buffer-name "*mu4e-main*")

;; Compose mail with gnus.
(setq read-mail-command 'gnus
      mail-user-agent 'gnus-user-agent);;gnus-user-agent mu4e-user-agent

;; Bookmarks and shortcuts
(setq mu4e-maildir-shortcuts
      '( ("/Distopico/INBOX"    . ?D)
         ("/Camilo/INBOX"       . ?C)
         ;; ("/Sent"               . ?s)
         ;; ("/Trash"              . ?t)
         ;; ("/Drafts"             . ?d)
         ;; ("/Queue"              . ?q)
         ;; ("/Spam"               . ?S)
         ))

(setq mu4e-bookmarks
      '(
        ("maildir:/Distopico/INBOX"                               "[Distopico] All"           ?D)
        ("flag:unread AND maildir:/Distopico/INBOX"               "[Distopico] Unread Inbox"  ?d)
        ("maildir:/Camilo/INBOX"                                  "[vXcamiloXv] All"          ?C)
        ("flag:unread AND maildir:/Camilo/INBOX"                  "[vXcamiloXv] Unread Inbox" ?c)
        ("flag:unread AND NOT flag:trashed AND NOT maildir:/Spam" "Unread messages"           ?u)
        ("date:today..now AND NOT maildir:/Spam"                  "Today's messages"          ?t)
        ("date:7d..now AND NOT maildir:/Spam"                     "Last 7 days"               ?w)
        ("mime:image/* AND NOT maildir:/Spam"                     "Messages with images"      ?p)
        ("flag:unread AND NOT flag:trashed AND maildir:/Spam"     "Unread spam"               ?s)
        ))

;; Enable account structure
(require 'mu4e-maildirs-extension)
(mu4e-maildirs-extension)

;; Config Mesagges
(setq message-kill-buffer-on-exit t
      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format "On %Y-%m-%d, %f wrote:\n" ;;message-citation-line-format "%N @ %Y-%m-%d %H:%M %Z:\n" "On %Y-%m-%d, %f wrote:" "On %Y-%m-%d %a at %H:%M %Z, %f wrote:\n"
      )

;; view some emails in web browser
(setq mu4e-msg2pdf "/usr/bin/msg2pdf")
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; Enable images
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; Default dir attachment
(setq mu4e-attachment-dir  "~/Downloads")

;; Send async
(require 'smtpmail-async)
(setq send-mail-function 'async-smtpmail-send-it
      message-send-mail-function 'async-smtpmail-send-it)
;;(setq message-send-mail-function 'smtpmail-send-it)

;; Default Account
(setq user-mail-address "distopico@riseup.net"
      user-full-name  "Distopico Vegan"
      mu4e-sent-folder "/Distopico/Sent"
      mu4e-drafts-folder "/Distopico/Drafts"
      starttls-use-gnutls t
      smtpmail-smtp-user "distopico@riseup.net"
      smtpmail-default-smtp-server "mail.riseup.net"
      smtpmail-local-domain "riseup.net"
      smtpmail-starttls-credentials '(("mail.riseup.net" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.mail/.authinfo.gpg")
      smtpmail-smtp-server "mail.riseup.net"
      smtpmail-smtp-service 25)

;; (setq
;;  user-mail-address "distopico@riseup.net"
;;  user-full-name  "Distopico Vegan"
;;  mu4e-compose-signature
;;  (concat
;;   "Distopico Vegan\n"
;;   "-----------------\n"))

;; Config by Account
(defvar mu4e-multi-account-alist
  '(("Distopico"
     (user-mail-address "distopico@riseup.net")
     (user-full-name  "Distopico Vegan")
     (mu4e-drafts-folder "/Distopico/Drafts")
     (mu4e-sent-folder "/Distopico/Sent")
     (smtpmail-default-smtp-server "mail.riseup.net")
     (smtpmail-local-domain "riseup.net")
     (smtpmail-smtp-user "distopico@riseup.net")
     (smtpmail-starttls-credentials '(("mail.riseup.net" 587 nil nil)))
     (smtpmail-auth-credentials (expand-file-name "~/.mail/.authinfo.gpg"))
     (smtpmail-smtp-server "mail.riseup.net")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 25))
    ("Camilo"
     (user-mail-address "vxcamiloxv0@openmailbox.org")
     (user-full-name  "Camilo QS")
     (mu4e-sent-folder "/Camilo/Sent")
     (mu4e-drafts-folder "/Camilo/Drafts")
     (smtpmail-default-smtp-server "smtp.openmailbox.org")
     (smtpmail-local-domain "openmailbox.org")
     (smtpmail-smtp-user "vxcamiloxv@openmailbox.org")
     (smtpmail-starttls-credentials '(("smtp.openmailbox.org" 587 nil nil)))
     (smtpmail-auth-credentials (expand-file-name "~/.mail/.authinfo.gpg"))
     (smtpmail-smtp-server "smtp.openmailbox.org")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))))

;; Set Account
(defun mu4e-milti-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                mu4e-multi-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) mu4e-multi-account-alist)
                             nil t nil nil (caar mu4e-multi-account-alist))))
         (account-vars (cdr (assoc account mu4e-multi-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

;; Hooks
(add-hook 'mu4e-compose-pre-hook 'mu4e-milti-set-account)
;; (add-hook 'mu4e-index-updated-hook
;;           (defun new-mail-notify ()
;;             (shell-command (concat "~/.emacs.d/scripts/notify_mail.sh "
;;                                    (number-to-string mu4e-update-interval)))))

;; Custom marks
(setq
 ;;mu4e-headers-seen-mark '("S" . "☑")
 mu4e-headers-new-mark '("N" .  "✉")
 ;; mu4e-headers-replied-mark '("R" . "↵")
 ;; mu4e-headers-passed-mark '("P" . "⇉")
 ;; mu4e-headers-encrypted-mark '("x" . "⚷")
 ;; mu4e-headers-signed-mark '("s" . "✍")
 ;; mu4e-headers-empty-parent-prefix '("-" . "◆")
 ;; mu4e-headers-first-child-prefix '("\\" . "▶")
 )

;; Starting the whole mu4e in its own frame
(defun mu4e-in-new-frame ()
  "Start mu4e in new frame."
  (interactive)
  (select-frame (make-frame))
  (mu4e))

;; Run mu4e in startup
(mu4e)
