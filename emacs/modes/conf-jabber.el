;;; Code:

(require 'tls)
(require 'starttls)
(require 'jabber)

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
      jabber-default-show ""
      jabber-default-status "M-x mode!!"
      ;;jabber-muc-default-nicknames "DistopicoVegan"
      jabber-roster-line-format " %4c %s | %a %-23n \n %8u %S" ;; " %c %-25n %u %-8s  %S"
      jabber-history-dir (in-emacs-d ".cache/jabber-history")
      jabber-avatar-cache-directory (in-emacs-d ".cache/jabber-avatar-cache")
      jabber-muc-autojoin '("vegan@conference.jabber.org")
      ;; Other
      jabber-alert-presence-hooks nil
      jabber-alert-message-hooks '(jabber-message-echo jabber-message-scroll))

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


;; Functions
(defun distopico:jabber-display-roster ()
  "Open rosetr jabber in fullscreen and delete other windows"
  (interactive)
  (window-configuration-to-register :jabber-fullscreen)
  (jabber-switch-to-roster-buffer)
  (delete-other-windows))

(defun distopico:jabber-close ()
  "Restores the previous window configuration and burry buffer"
  (interactive)
  (bury-buffer)
  (jump-to-register :jabber-fullscreen))

(defun distopico:jabber-chat-burry ()
  "Burry frame or delete frame if exit more than one."
  (interactive)
  (if (eq 'jabber-chat-mode major-mode)
      (if (< (length (frame-list)) 3)
          (bury-buffer)
        (delete-frame))))

(defun distopico:jabber-resize-avatar (&optional jc)
  "Resize jabber avatar in file directory."
  (interactive)
  (shell-command (concat
                  (format "for file in %s/*; " jabber-avatar-cache-directory)
                  "do size='$(identify -format %h $file)'; if [[ '$size' > '20' ]]; then convert $file -resize 20 $file; fi done 2>/dev/null"
                  ) nil nil)
  (message ""))

(defun distopico:jabber-avatar-compute-size (avatar)
  "Compute and set the width and height fields of AVATAR.
Return AVATAR."
  ;; image-size only works when there is a window system.
  ;; But display-graphic-p doesn't exist on XEmacs...
  (let ((size (and (fboundp 'display-graphic-p)
                   (display-graphic-p)
                   (let ((image (jabber-avatar-image avatar)))
                     (and image
                          (image-size image t))))))
    (when size
      (setf (avatar-width avatar) (/ (car size) 2))
      (setf (avatar-height avatar) (/ (cdr size) 2)))
    avatar))

;; Rewrite
(advice-add 'jabber-avatar-compute-size :override #'distopico:jabber-avatar-compute-size)

;; Hooks
(add-hook 'jabber-chat-mode-hook
          (lambda ()
            ;;(visual-line-mode t)
            ))

(add-hook 'jabber-post-connect-hooks 'distopico:jabber-resize-avatar 'append)

;; Run
(jabber-connect-all)

(provide 'conf-jabber)
