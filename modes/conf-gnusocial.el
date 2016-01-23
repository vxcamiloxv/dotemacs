;;; Code:
(require 'gnu-social-mode)

;; User config
(setq gnu-social-username "distopico"
      gnu-social-server "quitter.se"
      gnu-social-port 443
      ;;gnu-social-display-max-dents 20
      gnu-social-new-dents-count 2
      gnu-social-enable-highlighting t
      gnu-social-enable-striping t
      ;; gnu-social-image-stack t
      gnu-social-scroll-mode t
      gnu-social-icon-mode t
      gnu-social-soft-wrap-status t
      gnu-social-status-format "\t %i %s: %h%t \n\t      - %@ | from %f%L%r \n"
      gnu-social-stripe-bg-color "#001214"
      gnu-social-reply-bg-color "#002125"
      gnu-social-highlight-bg-color "cyan")

;; Custom keys
(define-key gnu-social-mode-map (kbd "C-q") 'distopico:gnusocial-close)
(define-key gnu-social-mode-map (kbd "M-q") 'kill-this-buffer)
(define-key gnu-social-mode-map (kbd "S") 'gnu-social-update-status-interactive)
(define-key gnu-social-mode-map (kbd "D") 'gnu-social-direct-message-interactive)

;; Functions
(defun distopico:gnusocial-open (&optional open-same-window)
  "Open gnusocial in fullscreen and delete other windows"
  (interactive)
  (open-buffer-delete-others "*gnu-social*" :gnusocial-fullscreen 'gnu-social))

(defun distopico:gnusocial-close ()
  "Restores the previous window configuration and burry gnusocial buffer"
  (interactive)
  (bury-buffer-restore-prev :gnusocial-fullscreen))

(defun distopico:gnusocial-new-dents-hook ()
  ;;(gnu-social-icon-mode t)
  (gnu-social-scroll-mode t))

(defun distopico:gnusocial-init-load-hook ()
  (gnu-social)
  (when (eq major-mode 'gnu-social-mode)
    (switch-to-buffer "*scratch*")))

(add-hook 'gnu-social-new-dents-hook 'distopico:gnusocial-new-dents-hook)
(add-hook 'distopico:after-init-load-hook 'distopico:gnusocial-init-load-hook)

(provide 'conf-gnusocial)
