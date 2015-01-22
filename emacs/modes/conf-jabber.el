;;; Code:
(require 'jabber)

;; Basic
(setq jabber-history-enabled t
      jabber-auto-reconnect t
      jabber-mode-line-mode t
      ;; nil
      jabber-show-resources nil
      jabber-use-global-history nil
      jabber-show-offline-contacts nil
      ;; String
      jabber-default-show "chat"
      jabber-roster-line-format " %c %-25n %u %-8s  %S" ;; %14s | %-23n(%j) %S
      jabber-history-dir (in-emacs-d ".cache/jabber-history")
      jabber-avatar-cache-directory (in-custom-d ".cache/jabber-avatar-cache")
      ;; Other
      jabber-alert-presence-hooks nil
      jabber-alert-message-hooks '(jabber-message-echo jabber-message-scroll))

;; Accounts
(setq jabber-account-list
      '(("distopico@riseup.net"
         ;;(:password . "userpassword")
         (:network-server . "proxy.riseup.net")
         (:machine-alias  . "distopico")
         (:port . 5222)
         (:connection-type . starttls))
        ("vxcamiloxv@openmailbox.org"
         (:network-server  . "openmailbox.org")
         (:machine-alias   . "vxcamiloxv")
         (:port . 5222)
         (:connection-type . starttls))
        ))

(setq jabber-invalid-certificate-servers '("talk.gmail.com" "unal.edu.co"))
;; (setq starttls-use-gnutls t
;;       starttls-gnutls-program "gnutls-cli"
;;       starttls-extra-arguments '("--starttls" "--insecure"))

;; Hooks
(add-hook 'jabber-chat-mode-hook
          (lambda ()
            (visual-line-mode t)
            (tabbar-local-mode -1)))

(provide 'conf-jabber)
