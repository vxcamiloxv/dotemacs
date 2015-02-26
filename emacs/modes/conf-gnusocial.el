;;; Code:
(require 'gnu-social-mode)

;; User config
(setq gnu-social-username "distopico"
      gnu-social-server "quitter.se"
      gnu-social-port 443
      gnu-social-enable-highlighting t
      gnu-social-enable-striping t
      gnu-social-stripe-bg-color "#001214")

;; Custom keys
(define-key gnu-social-mode-map (kbd "C-q") 'distopico:gnusocial-close)
(define-key gnu-social-mode-map (kbd "M-q") 'kill-this-buffer)
(define-key gnu-social-mode-map (kbd "S") 'gnu-social-update-status-interactive)
(define-key gnu-social-mode-map (kbd "D") 'gnu-social-direct-message-interactive)

;; Functions
(defun distopico:gnusocial-open ()
  "Open gnusocial in fullscreen and delete other windows"
  (interactive)
  (window-configuration-to-register :gnusocial-fullscreen)
  (gnu-social)
  (delete-other-windows))

(defun distopico:gnusocial-close ()
  "Restores the previous window configuration and burry gnusocial buffer"
  (interactive)
  (bury-buffer)
  (jump-to-register :gnusocial-fullscreen))

;; Faces
;; (custom-set-faces
;;  '(gnu-social-stripe-face ((t (:background "cyan4" :foreground "gray2")))))

;; Run!!
(gnu-social)

(provide 'conf-gnusocial)
