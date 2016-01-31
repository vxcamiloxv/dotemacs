;;; Code:
;; ---------
;; Tramp
;; ---------
(require 'tramp)
(require 'tramp-term)

(setq tramp-default-method "ssh"
      tramp-backup-directory-alist backup-directory-alist)

(add-to-list 'tramp-default-method-alist '("localhost" "" "sudo"))
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))

(provide 'conf-tramp)
