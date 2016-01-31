;;; Code:

(require 'powerline)

;; Basic
(setq  powerline-default-separator 'wave)

;; ;; Faces
(custom-set-faces
 '(powerline-active1 ((t (:foreground "#f9f9f9" :background "#123550" :box nil))))
 '(powerline-active2 ((t (:foreground "#f9f9f9" :background "#112230" :box nil)))))


(defun distopico:cycle-powerline-separators (&optional reverse)
  "Set Powerline separators in turn.  If REVERSE is not nil, go backwards."
  (interactive)
  (let* ((fn (if reverse 'reverse 'identity))
         (separators (funcall fn '("arrow" "arrow-fade" "slant"
                                   "chamfer" "wave" "brace" "roundstub" "zigzag"
                                   "butt" "rounded" "contour" "curve")))
         (found nil))
    (while (not found)
      (progn (setq separators (append (cdr separators) (list (car separators))))
             (when (string= (car separators) powerline-default-separator)
               (progn (setq powerline-default-separator (cadr separators))
                      (setq found t)
                      (redraw-display)))))))

(defun distopico:powerline-theme ()
  "Setup custom theme mode-line."
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
                                     ;; (powerline-buffer-size nil 'l)
                                     ;; (powerline-raw mode-line-mule-info nil 'l)
                                     ;; (powerline-buffer-id nil 'l)
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
                                     (powerline-vc face2 'r)))
                          (rhs (list (when (eq major-mode 'jabber-chat-mode)
                                       (powerline-raw distopico:jabber-mode-line-format face2 'r))
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face2 'l))
                                     (when (eq major-mode 'erc-mode)
                                       (powerline-raw '(:eval (distopico:erc-mode-line)) face2 'l))
                                     (when (eq major-mode 'gnu-social-mode)
                                       (powerline-raw '(:eval
                                                        (format "%s " (gnu-social-mode-line-buffer-identification))
                                                        ) face2 'l))
                                     (powerline-raw distopico:mu4e-mode-line-format face2 'r)
                                     (powerline-raw distopico:elfeed-mode-line-format face2 'r)
                                     (powerline-raw "•" face2 'r)
                                     (powerline-raw global-mode-string face2 'r)
                                     (powerline-raw pomodoro-display-string face2 'r)
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
                                     (powerline-raw "%6p" mode-line 'r)
                                     (when powerline-display-hud
                                       (powerline-hud mode-line face1)))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))


;; Run
(distopico:powerline-theme)

(provide 'conf-powerline)