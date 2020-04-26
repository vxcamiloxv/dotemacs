;;; Code:

(require 'powerline)

(defvar which-func-current)
(defvar which-func-keymap)

;; Basic
(setq powerline-default-separator 'wave)

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

(defun distopico:vc-modeline ()
  "Version control information."
  (when vc-mode
    (s-trim
     (concat vc-mode " "
             (when (buffer-file-name)
               (pcase (vc-state (buffer-file-name))
                 (`up-to-date "")
                 (`edited "*")
                 (`added "➕")
                 (`unregistered "?")
                 (`removed "➖")
                 (`needs-merge "⇆")
                 (`needs-update "↥")
                 (`ignored "∅")
                 (_ " Unk"))) " "))))

(defun distopico:which-function-modeline ()
  (when (and (boundp 'which-func-mode) which-func-mode)
    (let* ((current (format-mode-line which-func-current)))
      (when (string-match "{\\(.*\\)}" current)
        (setq current (match-string 1 current)))
      (propertize (concat "► " current)
                  'local-map which-func-keymap
                  'face 'which-func
                  'mouse-face 'mode-line-highlight
                  'help-echo "mouse-1: go to beginning\n\ mouse-2: toggle rest visibility\n\ mouse-3: go to end"))))

(defun distopico:setup-powerline-theme ()
  "Setup custom theme mode-line."
  (interactive)
  (setq-default mode-line-format (distopico:powerline-theme)))

(defun distopico:powerline-theme ()
  "Custom theme mode-line."
  '("%e"
    (:eval
     (let* ((active (powerline-selected-window-active))
            (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
            (mode-line (if active 'mode-line 'mode-line-inactive))
            (face0 (if active 'powerline-active0 'powerline-inactive0))
            (face1 (if active 'powerline-active1 'powerline-inactive1))
            (face2 (if active 'powerline-active2 'powerline-inactive2))
            (separator-left (intern (format "powerline-%s-%s"
                                            powerline-default-separator
                                            (car powerline-default-separator-dir))))
            (separator-right (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (cdr powerline-default-separator-dir))))
            (lhs (list (powerline-raw "%*" face0 'l)
                       ;; (powerline-buffer-size nil 'l)
                       ;; (powerline-raw mode-line-mule-info nil 'l)
                       ;; (powerline-buffer-id nil 'l)
                       (when powerline-display-mule-info
                         (powerline-raw mode-line-mule-info face0 'l))
                       (powerline-raw '(:eval
                                        (propertize
                                         "%b " 'help-echo
                                         (if (eq major-mode 'erc-mode)
                                             (powerline-raw mode-line-buffer-identification face0 'l)
                                           (buffer-file-name))))
                                      face0 'l)
                       (powerline-raw "" face0)
                       (funcall separator-left mode-line face1)
                       (powerline-major-mode face1 'l)
                       (powerline-process face1)
                       ;; (powerline-minor-modes face1 'l)
                       (powerline-narrow face1 'l)
                       (powerline-raw " " face1)
                       (funcall separator-left face1 face2)
                       (powerline-raw '(:eval
                                        (format " Proj[%s]" (projectile-project-name)))
                                      face2 'r)
                       (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
                         (powerline-raw mc/mode-line face2 'r))
                       (powerline-raw (distopico:vc-modeline) face2 'r)
                       (powerline-raw (distopico:which-function-modeline) face2 'l)))
            (rhs (list (when (eq major-mode 'jabber-chat-mode)
                         (powerline-raw distopico:jabber-mode-line-format face2 'r))
                       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                         (powerline-raw erc-modified-channels-object face1 'l))
                       (when (eq major-mode 'erc-mode)
                         (powerline-raw '(:eval (distopico:erc-mode-line)) face2 'l))
                       (when (eq major-mode 'gnu-social-mode)
                         (powerline-raw '(:eval
                                          (format "%s " (gnu-social-mode-line-buffer-identification)))
                                        face2 'l))
                       (when (and (boundp 'distopico:mu4e-mode-line-format) distopico:mu4e-mode-line-format)
                         (powerline-raw distopico:mu4e-mode-line-format face2 'r))
                       (when (and (boundp 'distopico:elfeed-mode-line-format) distopico:elfeed-mode-line-format)
                         (powerline-raw distopico:elfeed-mode-line-format face2 'r))
                       (powerline-raw "•" face2 'r)
                       (powerline-raw global-mode-string face2 'r)
                       (funcall separator-right face2 face1)
                       (unless window-system
                         (powerline-raw (char-to-string #xe0a1) face1 'l))
                       (powerline-raw "%2l" face1 'l)
                       (powerline-raw "•" face1 'l)
                       (powerline-raw '(:eval
                                        (propertize "%3c" 'face
                                                    (if (>= (current-column) 90)
                                                        'font-lock-warning-face 'nil)))
                                      face1 'r)
                       ;;(powerline-raw (distopico:selection-info-modeline) face1 'l)
                       (funcall separator-right face1 face0)
                       (powerline-raw " " face0)
                       (powerline-raw "%4p" face0 'r)
                       (when powerline-display-hud
                         (powerline-hud face1 face2))
                       (powerline-fill face0 0)
                       )))
       (concat (powerline-render lhs)
               (powerline-fill face2 (powerline-width rhs))
               (powerline-render rhs))))))


;; Run
(distopico:setup-powerline-theme)

(provide 'conf-powerline)
