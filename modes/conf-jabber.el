;;; Code:

(require 'tls)
(require 'starttls)
(require 'jabber)
(require 'jabber-otr)
(require 'autosmiley)

;; Costom vars
(defcustom distopico:jabber-default-account "distopico@riseup.net"
  "Jabber default account."
  :type 'string
  :group 'jabber)

(defcustom distopico:jabber-default-nickname "distopico"
  "Jabber default Nickname."
  :type 'string
  :group 'jabber)

(defcustom distopico:jabber-muc-exclude-regexp
  (concat "^\\bsubject\\b\:")
  "This regexp matches unwanted noise on jabber track muc."
  :type 'regexp
  :group 'jabber)

(defcustom distopico:jabber-muc-list nil
  "Jabber muc list."
  :type 'alist
  :group 'jabber)

;; Control vars
(defvar distopico:jabber-mode-line-format nil
  "String to display in the mode line.")
(defvar distopico:jid-history '())
(defvar distopico:jabber-account-alist nil)
(defvar distopico:jabber-invalid-certificate-servers nil)

;; Basic
(setq jabber-history-enabled t
      jabber-auto-reconnect t
      jabber-mode-line-mode t
      jabber-vcard-avatars-retrieve t
      jabber-avatar-max-width 40
      jabber-avatar-max-height 40
      jabber-backlog-number 40
      jabber-backlog-days 30
      ;; nil
      jabber-show-resources nil
      jabber-use-global-history nil
      jabber-show-offline-contacts nil
      jabber-message-alert-same-buffer nil
      ;; String
      jabber-default-status "M-x mode!!"
      jabber-roster-buffer: "*jabber-roster*"
      jabber-groupchat-buffer-format "*jabber-room: [ %n ]*"
      jabber-chat-buffer-format "*jabber-chat: [ %n ]*"
      jabber-chat-time-format "%H:%M"
      jabber-chat-delayed-time-format "%H:%M"
      jabber-notifications-icon notifications-application-icon
      jabber-notifications-timeout -1
      ;; jabber-muc-default-nicknames "DistopicoVegan"
      jabber-roster-line-format " %4c %s | %a %-23n \n %8u %S" ;; " %c %-25n %u %-8s  %S"
      jabber-history-dir (in-emacs-d ".cache/jabber-history")
      jabber-avatar-cache-directory (in-emacs-d ".cache/jabber-avatar-cache")
      jabber-otr-directory (in-emacs-d "config/jabber-otr")
      ;; Other
      jabber-alert-presence-hooks nil
      jabber-alert-message-hooks '(jabber-message-echo jabber-message-notifications jabber-message-scroll)
      jabber-alert-muc-hooks '(jabber-muc-echo-personal jabber-muc-notifications-personal jabber-muc-scroll)
      jabber-post-connect-hooks '(jabber-send-current-presence
                                  jabber-muc-autojoin
                                  jabber-whitespace-ping-start
                                  jabber-keepalive-start
                                  jabber-vcard-avatars-find-current)
      ;; Custom
      distopico:jabber-muc-list '("veganismo@salas.suchat.org"
                                  "vegan@conference.jabber.org"
                                  "elbinario@salas.xmpp.elbinario.net"
                                  "pump.io@conference.movim.eu"))

;; Jabber Accounts
(ignore-errors
  (load (expand-file-name ".conf-private.gpg" "~/") t))

(setq jabber-account-list distopico:jabber-account-alist
      jabber-invalid-certificate-servers distopico:jabber-invalid-certificate-servers)

;; Custom headers/model-line
(setq jabber-chat-header-line-format
      '(" ❱ " (:eval (jabber-jid-displayname jabber-chatting-with))
        " " (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
                     (propertize
                      (or (cdr (assoc (get buddy 'show) jabber-presence-strings))
                          (get buddy 'show)) 'face
                      (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
                          'jabber-roster-user-online))))
        " • " (:eval (jabber-fix-status (get (jabber-jid-symbol jabber-chatting-with) 'status)))
        (:eval (when (not (eq jabber-events-message ""))
                 " " jabber-events-messag))
        (:eval (when (not (eq jabber-chatstates-message ""))
                 " " jabber-chatstates-message)))
      jabber-muc-header-line-format
      '(" ❱ " (:eval (jabber-jid-displayname jabber-group)) " • " jabber-muc-topic " ]"))


;; Custom keys
(define-key jabber-roster-mode-map (kbd "C-q") 'distopico:jabber-close)
(define-key jabber-chat-mode-map (kbd "C-c C-j") 'distopico:jabber-chat-bury)
(define-key jabber-chat-mode-map (kbd "M-u") 'jabber-muc-names)
(define-key jabber-common-keymap (kbd "C-<tab>") 'distopico:jabber-chat-with)
(define-key jabber-common-keymap (kbd "C-x c") 'distopico:jabber-buffer-ido)

;; Message alert hooks
(define-jabber-alert echo "Show a message in the echo area (it not focus/active)"
  (lambda (text &optional title)
    (unless (minibuffer-prompt)
      (message "%s" (or title text)))))

;; Functions
(defun distopico:jabber-display-roster ()
  "Open rosetr jabber in fullscreen and delete other windows."
  (interactive)
  (open-buffer-delete-others jabber-roster-buffer :jabber-fullscreen 'jabber-switch-to-roster-buffer))

(defun distopico:jabber-close ()
  "Restore the previous window configuration and burry jabber buffer."
  (interactive)
  (bury-buffer-restore-prev :jabber-fullscreen))

(defun distopico:jabber-chat-bury ()
  "Bury frame or delete frame if exit more than one."
  (interactive)
  (if (eq 'jabber-chat-mode major-mode)
      (if (< (length (frame-list)) 3)
          (progn
            (bury-buffer)
            (jabber-switch-to-roster-buffer))
        (delete-frame))))

(defun distopico:jabber-buffer-ido ()
  "Switch to Jabber buffer using IDO to choose one."
  (interactive)
  (ido-for-mode "Jabber:" 'jabber-chat-mode))

(defun distopico:jabber-chat-with (jid &optional other-window)
  "Ido-based jabber-chat-with variant looking `JID' and optional open in `OTHER-WINDOW'."
  (interactive (list (distopico:jabber-read-jid-completing "Chat with: ")
                     current-prefix-arg))
  (let* ((jc (distopico:jabber-jid-connection jid))
         (buffer (jabber-chat-create-buffer jc jid)))
    (if other-window
        (switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))))

(defun distopico:jabber-auto-join (jc)
  "Only need to connect one `JC'.
I use multi-account and default auto-join not work
for me because connect all accounts."
  (let* ((state-data (fsm-get-state-data jc))
         (jid (plist-get state-data :original-jid)))
    (if (equal jid distopico:jabber-default-account)
        (progn
          (dolist (room distopico:jabber-muc-list)
            (jabber-groupchat-join jc room distopico:jabber-default-nickname))))))

(defun distopico:jabber-muc-looks-ignore-p (message)
  "Return non-nil if jabber MESSAGE must be ignored."
  (if message
      (string-match distopico:jabber-muc-exclude-regexp message)
  t))

(defun distopico:jabber-muc-looks-like-personal-p (message &optional group)
  "Return non-nil if jabber MESSAGE if I mentioned.
Optional argument GROUP to look."
  (if message
      (string-match (concat "\\b"
                            (regexp-quote (jabber-my-nick group))
                            "\\b")
                    message)
    nil))

(defun distopico:jabber-read-jid-completing (prompt)
  "Get JIDs candidates for ido completing, `PROMPT' to show in mini-buffer."
  (let* ((hist-items (remove-duplicates distopico:jid-history :test #'equal))
         (choices
          (mapcar #'symbol-name (jabber-concat-rosters))))
    (setf choices (append hist-items
                          (sort (set-difference choices hist-items :test #'equal)
                                #'string<)))
    (ido-completing-read prompt choices
                         nil nil nil 'distopico:jid-history)))

(defun distopico:jabber-jid-connection (jid)
  "Check if the `JID' has connection."
  (or (find-if
       #'(lambda (jc)
           (cl-find jid (plist-get (fsm-get-state-data jc) :roster)
                    :key #'symbol-name
                    :test #'equal))
       jabber-connections)
      (error "cannot determine connection for %s" jid)))

(defun distopico:jabber-chat-mode-hook ()
  "Hooks for `jabber-chat-mode'."
  (autosmiley-mode)
  ;;(tabbar-local-mode -1)
  )

(defun distopico:jabber-init-load-hook ()
  "Hooks on init jabber for connect all accounts."
  (when jabber-account-list
    (jabber-connect-all)))

;; Advice functions
(defun distopico:jabber-muc-process-presence (jc presence)
  "Remove all muc notices based on the `PRESENCE', `JC' from original function.
use this if you don't like all those notices about people joining/leaving."
  (let* ((from (jabber-xml-get-attribute presence 'from))
         (group (jabber-jid-user from))
         (buffer (get-buffer (jabber-muc-get-buffer group))))
    (if buffer
        (with-current-buffer buffer
          (ewoc-filter jabber-chat-ewoc (lambda (elt) (not (eq (car elt) :muc-notice))))))))

(defun distopico:jabber-activity-add-muc (orig-fun &rest args)
  "Advice `ORIG-FUN' `jabber-activity-add-muc' for only personal mentions.
Add a JID to mode line when `jabber-activity-show-p' needs `NICK' name from
the `GROUP' chat, also require `TEXT' to check if the message has name,
Optional `BUFFER' and `PROPOSED-ALERT'"
  ;; nick group buffer text proposed-alert
  (let ((group (nth 1 args))
        (text (nth 3 args)))
    (when (funcall jabber-activity-show-p group)
      (unless (distopico:jabber-muc-looks-ignore-p text)
        ;; No need activity if no call nick or is a ignored message
        (add-to-list 'jabber-activity-jids group)
        (when (distopico:jabber-muc-looks-like-personal-p text group)
          (add-to-list 'jabber-activity-personal-jids group))
        (jabber-activity-mode-line-update)))))

(defun distopico:jabber-activity-mode-line-update (orig-fun)
  "Advice `ORIG-FUN' `jabber-activity-mode-line-update' to avoid duplicate.
Update the string shown in the mode line using `jabber-activity-make-string'
on JIDs where `jabber-activity-show-p'.
Optional not-nil GROUP mean that message come from MUC.
Optional TEXT used with one-to-one or MUC chats and may be used to identify
personal MUC message.
Optional PRESENCE mean personal presence request or alert."
  (setq jabber-activity-mode-string
  	(if jabber-activity-jids
	    (concat (mapconcat
	     (lambda (x)
	       (let ((item (cdr x))
                 (jid (car (cdr x)))
                 (name (cdr (cdr x))))
		 (jabber-propertize
		  name
		  'face (if (member jid jabber-activity-personal-jids)
			    'jabber-activity-personal-face
			  'jabber-activity-face)
		  'local-map (when (fboundp 'make-mode-line-mouse-map)
			       (make-mode-line-mouse-map
				'mouse-1 `(lambda ()
					    (interactive "@")
					    (jabber-activity-switch-to
					     ,jid))))
		  'help-echo (concat "Jump to "
				     (jabber-jid-displayname jid)
				     "'s buffer"))))
           (let (result)
             (dolist (elt (mapcar #'jabber-activity-lookup-name
                                  jabber-activity-jids)
                          result)
               (let ((sofar (assoc (jabber-jid-displayname (car elt)) result)))
                 (unless sofar
                   (push (cons (jabber-jid-displayname (car elt)) elt) result)))))
	     ",") " ")
	  ""))
  (setq jabber-activity-count-string
	(number-to-string (length jabber-activity-jids)))
  (force-mode-line-update 'all)
  (run-hooks 'jabber-activity-update-hook))

(advice-add 'jabber-muc-process-presence :after #'distopico:jabber-muc-process-presence)
(advice-add 'jabber-activity-add-muc :around #'distopico:jabber-activity-add-muc)
(advice-add 'jabber-activity-mode-line-update :around #'distopico:jabber-activity-mode-line-update)

;; Hooks
(add-hook 'jabber-post-connect-hooks #'distopico:jabber-auto-join 'append)
(add-hook 'jabber-chat-mode-hook #'distopico:jabber-chat-mode-hook 'append)
(add-hook 'distopico:after-init-load-hook #'distopico:jabber-init-load-hook)


(provide 'conf-jabber)
