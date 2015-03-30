;;; Code:

(require 'tls)
(require 'starttls)
(require 'jabber)

;; Costom vars
(defcustom distopico:jabber-default-account "distopico@riseup.net"
  "jabber default account"
  :type 'string
  :group 'jabber)
(defcustom distopico:jabber-default-nickname "DistopicoVegan"
  "jabber default Nickname"
  :type 'string
  :group 'jabber)
(defcustom distopico:jabber-muc-list nil
  "jabber muc list"
  :type 'alist
  :group 'jabber)

;; Control vars
(defvar distopico:jabber-account-alist)
(defvar distopico:jabber-invalid-certificate-servers)

;; Basic
(setq jabber-history-enabled t
      jabber-auto-reconnect t
      jabber-mode-line-mode t
      ;; nil
      jabber-show-resources nil
      jabber-use-global-history nil
      jabber-show-offline-contacts nil
      ;; String
      jabber-avatar-max-width 40
      jabber-avatar-max-height 40
      jabber-default-show ""
      jabber-default-status "M-x mode!!"
      jabber-groupchat-buffer-format "*-jabber-room: %b-*"
      jabber-chat-buffer-format "*-jabber: %n-*"
      ;; jabber-muc-default-nicknames "DistopicoVegan"
      jabber-roster-line-format " %4c %s | %a %-23n \n %8u %S" ;; " %c %-25n %u %-8s  %S"
      jabber-history-dir (in-emacs-d ".cache/jabber-history")
      jabber-avatar-cache-directory (in-emacs-d ".cache/jabber-avatar-cache")
      ;; Other
      jabber-alert-presence-hooks nil
      jabber-alert-message-hooks '(jabber-message-echo jabber-message-scroll)
      jabber-alert-muc-hooks '(jabber-muc-scroll)
      jabber-post-connect-hooks '(jabber-send-current-presence
                                  jabber-muc-autojoin
                                  jabber-whitespace-ping-start
                                  jabber-keepalive-start
                                  jabber-vcard-avatars-find-current)
      ;; Custom
      distopico:jabber-default-nickname "DistopicoVegan"
      distopico:jabber-muc-list '("veganismo@salas.suchat.org"
                                  "vegan@conference.jabber.org"
                                  "radiolibrevigo@conference.amaya.tk"
                                  "elbinario@salas.elbinario.net"))

;; Jabber Accounts
(ignore-errors
  (load (expand-file-name ".conf-private.gpg" "~/") t))

(setq jabber-account-list distopico:jabber-account-alist
      jabber-invalid-certificate-servers distopico:jabber-invalid-certificate-servers)


(setq jabber-chat-header-line-format
      '(;;" " (:eval (jabber-jid-displayname jabber-chatting-with))
        "[ " (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
                      (propertize
                       (or (cdr (assoc (get buddy 'show) jabber-presence-strings))
                           (get buddy 'show)) 'face
                           (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
                               'jabber-roster-user-online))))
        " ~ " (:eval (get (jabber-jid-symbol jabber-chatting-with) 'status))
        (:eval (when (not (eq jabber-events-message ""))
                 "\t" jabber-events-messag))
        " ] •"
        (:eval (tabbar-local-mode -1))))


;; Custom keys
(define-key jabber-roster-mode-map (kbd "C-q") 'distopico:jabber-close)
(define-key jabber-chat-mode-map (kbd "C-q") 'distopico:jabber-chat-burry)
(define-key jabber-chat-mode-map (kbd "M-q") 'kill-this-buffer)
(define-key jabber-chat-mode-map (kbd "M-u") 'jabber-muc-names)


;; Functions
(defun distopico:jabber-display-roster ()
  "Open rosetr jabber in fullscreen and delete other windows"
  (interactive)
  (open-buffer-delete-others "*-jabber-roster-*" :jabber-fullscreen 'jabber-switch-to-roster-buffer))

(defun distopico:jabber-close ()
  "Restores the previous window configuration and burry jabber buffer"
  (interactive)
  (bury-buffer-restore-prev :jabber-fullscreen))

(defun distopico:jabber-chat-burry ()
  "Burry frame or delete frame if exit more than one."
  (interactive)
  (if (eq 'jabber-chat-mode major-mode)
      (if (< (length (frame-list)) 3)
          (bury-buffer)
        (delete-frame))))


(defun distopico:jabber-auto-join (jc)
  "I use multi-account and default auto-join not work for me because connect all accounts.
And only need to connect one"
  (let* ((state-data (fsm-get-state-data jc))
         (jid (plist-get state-data :original-jid)))
    (if (equal jid distopico:jabber-default-account)
        (progn
          (dolist (room distopico:jabber-muc-list)
            (jabber-groupchat-join jc room distopico:jabber-default-nickname))))))

(add-hook 'jabber-post-connect-hooks 'distopico:jabber-auto-join 'append)

;; Hooks
(add-hook 'jabber-chat-mode-hook
          (lambda ()
            ;;(visual-line-mode t)
            ))


;; Run
(jabber-connect-all)

(provide 'conf-jabber)
