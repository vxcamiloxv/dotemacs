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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error-face ((t (:inherit error :background "gray27" :foreground "IndianRed1" :underline (:color "red" :style wave)))))
 '(flycheck-warning-face ((t (:inherit warning :foreground "yellow1"))))
 '(completions-common-part ((t (:foreground "forest green"))))
 '(completions-first-difference ((t (:bold t :weight bold :foreground "salmon")))))

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

(defun distopico:toggle-all-theme ()
  "Toggle between enabled themes."
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

;; (defun distopico:font-name-replace-size (font-name new-size)
;;   "Function from https://github.com/aaronbieber/dotfiles."
;;   (let ((parts (split-string font-name "-")))
;;     (setcar (nthcdr 7 parts) (format "%d" new-size))
;;     (mapconcat 'identity parts "-")))

;; (defun distopico:set-frame-font-size (size)
;;   "Function from https://github.com/aaronbieber/dotfiles"
;;   (set-frame-font (distopico:font-name-replace-size (face-font 'default) size) t t))

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
