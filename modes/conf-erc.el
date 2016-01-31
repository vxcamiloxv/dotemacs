
;;; Code:

(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-services)


;; Features
(erc-log-mode t)
(erc-track-mode t)
(erc-scrolltobottom-mode t)
(erc-autojoin-mode t)
(erc-services-mode t)
(erc-truncate-mode +1)

;; Basic
(setq erc-header-line-uses-tabbar-p t
      erc-server-auto-reconnect t
      erc-track-exclude-server-buffer t
      ;; nil
      erc-prompt-for-nickserv-password nil
      ;; String
      ;; erc-max-buffer-size 100000
      erc-input-line-position -2
      erc-auto-query 'buffer
      erc-join-buffer 'bury
      erc-user-full-name user-full-name
      erc-track-position-in-mode-line 'after-modes
      erc-current-nick-highlight-type 'nick-or-keyword
      erc-track-priority-faces-only 'all
      erc-modules '(autojoin
                    button completion fill irccontrols services
                    truncate keep-place list log match menu move-to-prompt
                    netsplit networks noncommands readonly ring
                    stamp track scrolltobottom notifications)

      erc-track-faces-priority-list '(erc-current-nick-face
                                      erc-direct-msg-face
                                      erc-keyword-face)
      ;; erc-track-faces-priority-list '(erc-error-face
      ;;                                 erc-current-nick-face
      ;;                                 erc-keyword-face
      ;;                                 erc-nick-msg-face
      ;;                                 erc-direct-msg-face
      ;;                                 erc-dangerous-host-face
      ;;                                 erc-notice-face
      ;;                                 erc-prompt-face)

      erc-nick "DistopicoVegan"
      erc-keywords '("\\DistopicoVegan[-a-z]*\\b")
      erc-mode-line-format "%a %t %o"
      erc-hide-list '("MODE")
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477")
      erc-log-channels-directory (in-emacs-d ".cache/erc/logs/")
      erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#emacs" "#gnu" "#emacs-es" "##vegan"
         "#social" "#isso" "#libre.fm" "#parabola" "#trisquel-es" "#trisquel"
         "#lorea" "#mediagoblin" "#org-mode" "#pump.io" "#radioliberacion")
        (".*\\.indymedia.org" "#riseup")))

;; Custom keys
(define-key erc-mode-map (kbd "C-c C-b") 'distopico:erc-ido-switch-buffer)
(define-key erc-mode-map (kbd "C-q") 'distopico:erc-close)
(define-key erc-mode-map (kbd "M-q") 'kill-this-buffer)

;; Functions
(defun erc-ignore-unimportant (msg)
  "less noise in erc-mode"
  (if (or (string-match "*** localhost has changed mode for &bitlbee to" msg)
          (string-match "Account already online" msg)
          (string-match "Unknown error while loading configuration" msg))
      (setq erc-insert-this nil)))

(defun distopico:get-erc-nickserv-passwords (host user)
  "Read irc nickserv password."
  (let ((found
         (and (fboundp 'auth-source-search)
              (nth 0 (auth-source-search
                      :user user
                      :host host
                      :port "irc"
                      :max 1
                      :require '(:secret))))))
    (if found
        (let ((secret (plist-get found :secret)))
          (copy-sequence
           (if (functionp secret)
               (funcall secret)
             secret))))))

(defun distopico:erc-connect ()
  (interactive)
  (distopico:erc-list-server))

(defun distopico:erc-list-server ()
  "List of defaults irc server to connect."
  (when (not (get-buffer "irc.freenode.net:6667"))
    (erc :server "irc.freenode.net" :port 6667 :password "")
    (erc :server "irc.radiognu.org" :port 6667 :password "")
    (erc-tls :server "irc.indymedia.org" :port 6697 :password "")
    ))

(defun distopico:erc-start-ask-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667")
      (progn
        (window-configuration-to-register :erc-fullscreen)
        (erc-track-switch-buffer 1)
        (delete-other-windows))
    (when (y-or-n-p "Start ERC? ")
      (distopico:erc-list-server))))

(defun distopico:erc-close ()
  "Restores the previous window configuration and burry buffer"
  (interactive)
  (bury-buffer)
  (jump-to-register :erc-fullscreen))

(defun distopico:erc-ido-switch-buffer ()
  "Switch to ERC buffer using IDO to choose which one, or start ERC if not already started."
  (interactive)
  (eval-when-compile
    (require 'ido))
  (let (final-list (list ))
    (dolist (buf (buffer-list) final-list)
      (if (equal 'erc-mode (with-current-buffer buf major-mode))
     	  (setq final-list (append (list (buffer-name buf)) final-list))))
    (if final-list
     	(switch-to-buffer (ido-completing-read "ERC Buffer: " final-list))
      (call-interactively 'erc))))


(defun distopico:erc-global-get-channel-buffer-list ()
  "Return a list of the ERC-channel-buffers"
  (erc-buffer-filter '(lambda() (if (string-match "^[^#].*:\\([0-9]*\\|ircd\\)$" (buffer-name (current-buffer))) nil t)) nil))

(defun distopico:erc-switch-to-irc ()
  "Switch to an IRC channel buffer, or run `erc-select'.
        When called repeatedly, cycle through the buffers."
  (interactive)
  (let ((buffers (distopico:erc-global-get-channel-buffer-list)))
    (when (eq (current-buffer) (car buffers))
      (bury-buffer)
      (setq buffers (cdr buffers)))
    (if buffers
        (switch-to-buffer (car buffers))
      (call-interactively 'erc-select))))

(defun distopico:erc-mode-line ()
  "Return user in current erc channel"
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

(defun distopico:erc-after-connect-hook (SERVER NICK)
  "Send identify when connect to irc chanel"
  (cond
   ((string-match "freenode\\.net" SERVER)
    (erc-message "PRIVMSG" (concat "NickServ identify " (distopico:get-erc-nickserv-passwords "irc.freenode.net" NICK)) ))))

(defun distopico:erc-init-load-hook ()
  (distopico:erc-connect))

;; Hooks
(add-hook 'erc-after-connect 'distopico:erc-after-connect-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(add-hook 'erc-insert-pre-hook 'erc-ignore-unimportant)
(add-hook 'distopico:after-init-load-hook 'distopico:erc-init-load-hook)

(provide 'conf-erc)