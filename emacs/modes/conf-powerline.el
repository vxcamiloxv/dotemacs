;;; Code:

(require 'powerline)
(require 'conf-mu4e)

;;(call-interactively 'display-time-mode)
;; Basic
(setq  powerline-default-separator 'wave)

;; Faces
(custom-set-faces
 '(powerline-active1 ((t (:foreground "#f9f9f9" :background "#123550" :box nil))))
 '(powerline-active2 ((t (:foreground "#f9f9f9" :background "#112230" :box nil))))
 )

;; Custom theme
(defun powerline-distopico-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     ;;(powerline-buffer-size nil 'l)
                                     ;;(powerline-raw mode-line-mule-info nil 'l)
                                     ;;(powerline-buffer-id nil 'l)
                                     (powerline-raw '(:eval
                                                      (propertize
                                                       "%b " 'help-echo
                                                       (if (eq major-mode 'erc-mode)
                                                           (powerline-raw mode-line-buffer-identification face2 'l)
                                                         (buffer-file-name)))) nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     ;;(powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-raw '(:eval (format " Proj[%s]"
                                                                    (projectile-project-name))) face2)
                                     (powerline-vc face2 'r)
                                     ))
                          (rhs (list (when (eq major-mode 'jabber-chat-mode)
                                       (powerline-raw jabber-chat-header-line-format face2 'r))
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face2 'l))
                                     (when (eq major-mode 'erc-mode)
                                       (powerline-raw '(:eval
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
                                                          (format "• O: %S | V: %S | M: %S " ops voices members))) face2 'l))
                                     (when (eq major-mode 'gnu-social-mode)
                                       (powerline-raw gnu-social-modeline-active face2 'l))
                                     (powerline-raw distopico:mu4e-mode-line face2 'r)
                                     (powerline-raw "•" face2 'r)
                                     (powerline-raw global-mode-string face2 'r)
                                     ;;(powerline-raw pomodoro-display-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%2l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw '(:eval
                                                      (propertize "%2c" 'face
                                                                  (if (>= (current-column) 90)
                                                                      'font-lock-warning-face 'nil))
                                                      ) face1 'l)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud mode-line face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

;; Run
(powerline-distopico-theme)


(provide 'conf-powerline)
