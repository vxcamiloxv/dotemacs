;;; Code:

(require 'znc)
(require 'erc)
(require 'erc-log)
;; (require 'erc-notify)
(require 'erc-services)
(require 'alert)
;; (require 'erc-goodies)

(defcustom distopico:erc-raw-connection nil
  "Erc connect without ZNC."
  :type 'boolean
  :group 'erc)

(defcustom distopico:erc-servers-list '("irc.freenode.net:6667" "irc.indymedia.org:6697" "irc.w3.org")
  "Erc defaults servers to connect."
  :type 'list
  :group 'erc)

(defcustom distopico:erc-alert-priority-target (concat "\\`[^&].*@freenode\\'")
  "Priority regexp target/network for finger alert."
  :type 'regexp
  :group 'erc)

(defcustom distopico:erc-alert-noise-regexp
  (concat "\\(Logging in:\\|Signing off\\|You're now away"
          "\\|Welcome back\\|Setting automatically away\\)")
  "This regexp matches unwanted noise."
  :type 'regexp
  :group 'erc)

(defcustom distopico:erc-alert-priority-regexp
  (concat "^\\(<\\([^>]*\\)>\\)" "\\|^\\*\\*\\*\\s-\\w")
  "This regexp matches priority messages."
  :type 'regexp
  :group 'erc)

(defcustom distopico:znc-server '("znc.distopico.info" 6697 t)
  "ZNC/irc defaults networks to connect."
  :type 'list
  :group 'erc)

(defcustom distopico:znc-username "cerdolibre"
  "ZNC username to connect."
  :type 'string
  :group 'erc)

(defcustom distopico:znc-networks '("freenode" "indymedia") ;;  TODO: fix duplicate name  with w3#social
  "ZNC/irc defaults networks to connect."
  :type 'list
  :group 'erc)

(defvar distopico:erc-reconnect-timer nil
  "Interval timer on reconnect.")

(defvar distopico:erc-alert-last-message nil
  "Last alert message send.")

;; Basic
(setq erc-header-line-uses-tabbar-p t
      erc-server-auto-reconnect t
      erc-server-reconnect-attempts t
      erc-track-exclude-server-buffer t
      erc-track-shorten-start 4
      erc-track-shorten-cutoff 5
      erc-track-shorten-aggressively t
      erc-minibuffer-ignored t ;; if we ignored something
      erc-track-showcount t
      ;; nil
      erc-prompt-for-nickserv-password nil
      ;; String
      erc-input-line-position -1
      erc-auto-query 'buffer
      erc-join-buffer 'bury
      erc-track-position-in-mode-line 'after-modes
      erc-current-nick-highlight-type 'nick-or-keyword
      erc-track-priority-faces-only 'all
      erc-echo-notice-hook '(erc-echo-notice-in-minibuffer)
      ;; Erc modules
      erc-modules '(pcomplete
                    netsplit fill button match readonly
                    track completion networks ring autojoin
                    noncommands irccontrols move-to-prompt
                    stamp menu list services truncate log
                    scrolltobottom);; notifications)
      ;; erc-track-faces-priority-list '(erc-current-nick-face
      ;;                                 erc-direct-msg-face
      ;;                                 erc-keyword-face)
      erc-track-faces-priority-list '(erc-error-face
                                      erc-current-nick-face
                                      erc-keyword-face
                                      erc-nick-msg-face
                                      erc-direct-msg-face
                                      erc-dangerous-host-face
                                      ;; erc-notice-face
                                      erc-prompt-face)
      ;; erc-user-full-name user-full-name
      erc-user-full-name "DistopicoVegan"
      erc-nick '("distopico" "DistopicoVegan")
      erc-keywords '("\\distopico[-a-z]*\\b")
      erc-mode-line-format "%a %t %o"
      erc-hide-list '("MODE")
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477")
      erc-log-channels-directory (in-emacs-d ".cache/erc/logs/")
      erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#emacs" "#gnu" "#emacs-es" "##vegan"
         "#social" "#libre.fm" "#parabola" "#trisquel-es" "#trisquel"
         "#mediagoblin" "#org-mode" "#pump.io" "#indieweb" "#social")
        (".*\\.w3.org", "#social")
        (".*\\.indymedia.org" "#riseup")))


;; Custom keys
(define-key erc-mode-map (kbd "C-c C-b") 'distopico:erc-ido-switch-buffer)
(define-key erc-mode-map (kbd "C-c C-r") 'distopico:erc-reset-track-mode)
(define-key erc-mode-map (kbd "C-c M-a") 'erc-track-switch-buffer)
(define-key erc-mode-map (kbd "C-q") 'distopico:erc-close)

;; Functions
(defun distopico:erc-ignore-unimportant (msg)
  "Ignore some IRC `MSG', less noise in `erc-mode'."
  (if (or (string-match "*** localhost has changed mode for &bitlbee to" msg)
          (string-match "Account already online" msg)
          (string-match "Unknown error while loading configuration" msg))
      (setq erc-insert-this nil)))

(defun distopico:erc-connect ()
  "Connect on all defaults servers."
  (interactive)
  
  (if distopico:erc-raw-connection
      ;; Connect only with ERC
      (dolist (server distopico:erc-servers-list)
        (let ((data (s-split "\\:" server))
              (buffer (get-buffer server)))
          (unless (erc-server-process-alive buffer)
            (let ((host (nth 0 data))
                  (port (nth 1 data)))
              (when buffer
                (kill-buffer buffer))
              (if (equal port "6667")
                  (erc :server host :port port :password nil)
                (erc-tls :server host :port port :password nil))))))
    ;; Connect to all networks with ZNC
    (distopico:erc-znc-setup)
    (condition-case nil
        (znc-all)
      (error (message "ZNC initialization failure")))))

(defun distopico:erc-reconnect ()
  "Reconnect all active servers."
  (interactive)
  (dolist (server-buffer (erc-buffer-list #'erc-server-buffer-p))
    (when (and (not (erc-server-process-alive server-buffer))
               (erc-server-reconnect-p "close")
               (not erc-server-quitting))
      (distopico:erc-server-reconnect server-buffer))))

(defun distopico:erc-close ()
  "Restore the previous window configuration and bury buffer."
  (interactive)
  (bury-buffer-restore-prev :erc-fullscreen))

(defun distopico:erc-server-reconnect (buffer)
  "Check if `BUFFER' still live and try to reconnect."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((active-buffers (distopico:erc-server-buffers-connect-p
                             erc-session-server
                             erc-session-port
                             (erc-current-nick)))
            (server-buffers (distopico:erc-server-buffers-p
                             erc-session-server
                             erc-session-port
                             (erc-current-nick))))
        ;; Veirfy if exist active server buffer
        (if active-buffers
            (unless (equal buffer (nth 0 active-buffers))
              (kill-buffer buffer))
          (progn
            (dolist (server-buffer server-buffers)
              ;; Kill similar server buffer, prevent duplicates
              (unless (equal buffer server-buffer)
                (kill-buffer buffer)))
            ;; Ensure if not has active connection yet
            (when (and (not erc-server-connected)
                       (not erc-server-reconnecting))
              (erc-server-reconnect))))))))

(defun distopico:erc-server-buffers-connect-p (server port nick)
  "Get buffers of `SERVER' connected with same `PORT' and `NICK' user."
  (erc-buffer-list
   (lambda ()
     (and (erc-server-buffer-p)
          (erc-server-process-alive)
          (string= erc-session-server server)
          (erc-port-equal erc-session-port port)
          (erc-current-nick-p nick)))))

(defun distopico:erc-reset-track-mode ()
  "Clears out annoying `erc-track-mode' stuff for when we don't care."
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))

(defun distopico:erc-server-buffers-p (server port nick)
  "Get all buffers of `SERVER' with same `PORT' and `NICK' user."
  (erc-buffer-list
   (lambda ()
     (and (erc-server-buffer-p)
          (string= erc-session-server server)
          (erc-port-equal erc-session-port port)
          (erc-current-nick-p nick)))))

(defun distopico:erc-start-ask-or-switch ()
  "Connect to ERC, or switch to last active buffer."
  (interactive)
  (if (> (length (distopico:erc-buffer-list)) 0)
      (progn
        (window-configuration-to-register :erc-fullscreen)
        (if erc-modified-channels-alist
            (erc-track-switch-buffer 1)
          (distopico:erc-ido-switch-buffer))
        (delete-other-windows))
    (when (y-or-n-p "Start ERC? ")
      (distopico:erc-connect))))

(defun distopico:erc-ido-switch-buffer ()
  "Switch to ERC buffer using IDO to choose which one, or start ERC if not already started."
  (interactive)
  (eval-when-compile
    (require 'ido))
  (let ((final-list (distopico:erc-buffer-list)))
    (if final-list
        (switch-to-buffer (ido-completing-read "ERC Buffer: " final-list))
      (call-interactively 'erc))))

(defun distopico:erc-switch-to-irc ()
  "Switch to an IRC channel buffer, or run `erc-select'.
When called repeatedly, cycle through the buffers `DEPRECATED'."
  (interactive)
  (let ((buffers (distopico:erc-global-get-channel-buffer-list)))
    (when (eq (current-buffer) (car buffers))
      (bury-buffer)
      (setq buffers (cdr buffers)))
    (if buffers
        (switch-to-buffer (car buffers))
      (call-interactively 'erc-select))))

(defun distopico:erc-buffer-list ()
  "Return all ERC buffers by major mode."
  (let (final-list (list ))
    (dolist (buf (buffer-list) final-list)
      (if (equal 'erc-mode (with-current-buffer buf major-mode))
          (setq final-list (append (list (buffer-name buf)) final-list))))))

(defun distopico:erc-global-get-channel-buffer-list ()
  "Return a list of the ERC-channel-buffers."
  (erc-buffer-filter '(lambda() (if (string-match "^[^#].*:\\([0-9]*\\|ircd\\)$" (buffer-name (current-buffer))) nil t)) nil))

(defun distopico:get-erc-nickserv-passwords (host port user)
  "Read irc nickserv password by `HOST', `PORT' and `USER'."
  (let ((found
         (and (fboundp 'auth-source-search)
              (nth 0 (auth-source-search
                      :user user
                      :host host
                      :port (if (numberp port) (number-to-string port) port)
                      :max 1
                      :require '(:secret))))))
    (if found
        (let ((secret (plist-get found :secret)))
          (copy-sequence
           (if (functionp secret)
               (funcall secret)
             secret))))))

(defun distopico:erc-mode-line ()
  "Return user in current erc channel."
  (let ((ops 0)
        (voices 0)
        (members 0))
    (if erc-channel-users
        (maphash (lambda (key value)
                   (when (erc-channel-user-op-p key)
                     (setq ops (1+ ops)))
                   (when (erc-channel-user-voice-p key)
                     (setq voices (1+ voices)))
                   (setq members (1+ members)))
                 erc-channel-users))
    (format "• O: %S | V: %S | M: %S " ops voices members)))

(defun distopico:erc-alert-important-p (info)
  "Return non-nil by `INFO' is a important notification to shown."
  (let ((erc-message (plist-get info :data)))
    (and erc-message
         (not (or (string-match "^\\** *Users on #" erc-message)
                  (string-match distopico:erc-alert-noise-regexp
                                erc-message))))))

(defun distopico:erc-matched-or-insert-hook (&optional match-type nickuserhost message)
  "Send notification filter by custom rules see: `distopico:erc-define-alerts'.
If has `MATCH-TYPE', `NICKUSERHOST' and `MESSAGE' from `erc-text-matched-hook'
send notification when `distopico:erc-alert-important-p' is non-nil"
  (if (and (or (null match-type)
               (not (eq match-type 'fool)))
           (or (null distopico:erc-alert-last-message)
               (not (equal distopico:erc-alert-last-message message)))
           (string-match distopico:erc-alert-priority-regexp (or message "")))
      (let (alert-message)
        ;; Setup alert message
        (if (not message)
            (setq alert-message (buffer-string))
          (setq distopico:erc-alert-last-message message)
          (setq alert-message
                (concat "<"
                        (nth 0 (erc-parse-user nickuserhost))
                        "> " message)))
        ;; Send alert but the rules filter what will shown
        (alert alert-message
               :severity 'high
               :title (concat "ERC: " (buffer-name))
               :data message))))

(defun distopico:erc-define-alerts ()
  "Define rules for `alert' mode for `erc-mode'."
  ;; Unless the user has recently typed in the ERC buffer, highlight the fringe
  (alert-add-rule
   :status '(buried visible idle)
   :severity '(moderate high urgent)
   :mode 'erc-mode
   :predicate
   #'(lambda (info)
       (and (not (eq (current-buffer) (plist-get info :buffer)))
            (or (string-match (concat "\\" (erc-current-nick) "[-a-z]*\\b")
                              (plist-get info :message))
                (string-match distopico:erc-alert-priority-target
                              (erc-format-target-and/or-network)))))
   :persistent
   #'(lambda (info)
       ;; If the buffer is buried, or the user has been idle for
       ;; `alert-reveal-idle-time' seconds, make this alert
       ;; persistent.  Normally, alerts become persistent after
       ;; `alert-persist-idle-time' seconds.
       (memq (plist-get info :status) '(buried idle)))
   :style 'fringe
   :continue t)

  ;; Important notifications
  (alert-add-rule
   :status 'buried
   :mode 'erc-mode
   :predicate #'distopico:erc-alert-important-p
   :append t)
  ;; Ignore if not match with above rules
  (alert-add-rule :mode 'erc-mode :style 'ignore :append t))


(defun distopico:erc-znc-setup ()
  "Setup configuration for ZNC server."
  (when distopico:znc-server
    (let ((networks-list (list ))
          (username distopico:znc-username)
          (server-conf distopico:znc-server)
          (host (nth 0 distopico:znc-server))
          (port (nth 1 distopico:znc-server))
          (pass))
      ;; Get password
      (setq pass (distopico:get-erc-nickserv-passwords host port username))
      ;; Networks lists
      (dolist (network distopico:znc-networks networks-list)
        (when (stringp network)
          (setq network (intern network)))
        (add-to-list 'networks-list (list network (format "%s/%s" username network) pass)))
      ;; Set configuration
      (setq znc-servers (list (append server-conf (list networks-list)))))))

(defun distopico:erc-cancel-reconnect ()
  "Cancel re-connection attempt interval."
  (when distopico:erc-reconnect-timer
    (cancel-timer distopico:erc-reconnect-timer)
    (setq distopico:erc-reconnect-timer nil)))

(defun distopico:erc-after-connect-hook (SERVER NICK)
  "When connect to irc `SERVER' send identify `NICK'."
  (let ((server (or erc-session-server SERVER))
        (port erc-session-port))
    (erc-nickserv-identify (distopico:get-erc-nickserv-passwords server port NICK))))

(defun distopico:erc-before-connect-hook (SERVER PORT NICK)
  "Before connect to irc `SERVER' with `PORT' with identify `NICK'."
  (distopico:erc-cancel-reconnect))

(defun distopico:erc-disconnected-hook (NICK HOSTNAME REASON)
  "Re-establish the connection by user `NICK' even if the server closed it in `HOSTNAME' by `REASON'."
  (distopico:erc-cancel-reconnect)
  (setq distopico:erc-reconnect-timer
        (run-at-time nil (* 10 erc-server-reconnect-timeout) #'distopico:erc-reconect)))

(defun distopico:erc-init-load-hook ()
  "Hook when Emacs load."
  (distopico:erc-define-alerts)
  (distopico:erc-connect))

;; Hooks
;;(add-hook 'erc-before-connect #'distopico:erc-before-connect-hook)
;; (add-hook 'erc-insert-pre-hook #'erc-ignore-unimportant)
;; (add-hook 'erc-disconnected-hook #'distopico:erc-disconnected-hook)
(add-hook 'erc-insert-post-hook #'erc-truncate-buffer)
(add-hook 'erc-text-matched-hook 'distopico:erc-matched-or-insert-hook)
(add-hook 'erc-insert-modify-hook 'distopico:erc-matched-or-insert-hook)
(add-hook 'erc-after-connect #'distopico:erc-after-connect-hook)
(add-hook 'distopico:after-init-load-hook #'distopico:erc-init-load-hook)

(provide 'conf-erc)
