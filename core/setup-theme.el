;;; Code:
(require 's)
(require 'dash)

(defcustom distopico:gui-theme 'tron
  "Default gui theme."
  :group 'faces)

(defcustom distopico:cli-theme nil
  "Default cli theme."
  :group 'faces)

;; Config
(setq custom-safe-themes t)

;; Load themes from `themes' path
(distopico:to-list-path-subdir (in-emacs-d "themes") 'custom-theme-load-path)
;; Enabled custom theme
(when distopico:gui-theme
  (load-theme distopico:gui-theme t nil))
(when distopico:cli-theme
  (load-theme distopico:cli-theme t t))

;; Functions
(defun distopico:load-available-themes ()
  "Load all themes available."
  (interactive)
  (dolist (name (custom-available-themes))
    (load-theme name t t)))

(defun distopico:toggle-theme ()
  "Toggle between cli or gui theme."
  (interactive)
  (let ((is-gui-theme (find distopico:gui-theme custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    ;; load-theme
    (if is-gui-theme
        (distopico:enable-theme distopico:cli-theme)
      (distopico:enable-theme distopico:gui-theme))))

(defun distopico:enable-theme (theme)
  "Check if THEME is loaded and enable."
  (when (not (equal theme nil))
    (when (stringp theme)
      (setq theme (intern theme)))
    (dolist (name (custom-available-themes))
      (when (eq name theme)
        (unless (find name custom-enabled-themes)
          (load-theme name t t))
        (enable-theme name)))))

(defun distopico:set-theme-window-system (frame)
  "Set theme by variable `window-system' in FRAME."
  (select-frame frame)
  (if (window-system frame)
      (progn
        (disable-theme distopico:cli-theme)
        (distopico:enable-theme distopico:gui-theme))
    (progn
      (disable-theme distopico:gui-theme)
      (distopico:enable-theme distopico:cli-theme))))

;; Hooks
(add-hook 'after-make-frame-functions 'distopico:set-theme-window-system)

(provide 'setup-theme)
