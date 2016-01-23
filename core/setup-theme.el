;;; Code:
(require 's)
(require 'dash)

(defcustom distopico:gui-theme 'tron
  "default gui theme"
  :group 'faces)

(defcustom distopico:cli-theme 'nil
  "default cli theme"
  :group 'faces)

;; Load theme in `themes' path
(distopico:to-list-path-subdir (in-emacs-d "themes") 'custom-theme-load-path)

;; Custom themes added to load-path
;; (when (= emacs-major-version 24)
;;  (add-to-list 'custom-theme-load-path (concat emacs-dir "themes/"))
;; Enabled custom theme
(when distopico:gui-theme
  (load-theme distopico:gui-theme t))
(when distopico:cli-theme
  (load-theme distopico:cli-theme t))

;; Functions
(defun distopico:toggle-cli-theme ()
  (interactive)
  (let ((is-gui (find distopico:gui-theme custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    ;; load-theme
    (if is-gui
        (progn
          (if (not (eq distopico:cli-theme 'nil))
              (enable-theme distopico:cli-theme)))
      (progn
        (if (not (eq distopico:gui-theme 'nil))
            (enable-theme distopico:gui-theme))))))

(defun distopico:font-name-replace-size (font-name new-size)
  "Function from https://github.com/aaronbieber/dotfiles"
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun distopico:set-frame-font-size (size)
  "Function from https://github.com/aaronbieber/dotfiles"
  (set-frame-font (distopico:font-name-replace-size (face-font 'default) size) t t))

(defun distopico:set-theme-window-system (frame)
  (select-frame frame)
  (if (window-system frame)
      (progn
        (disable-theme distopico:cli-theme)
        (enable-theme distopico:gui-theme))
    (progn
      (disable-theme distopico:gui-theme)
      (disable-theme distopico:cli-theme))))

;; Hooks
(add-hook 'after-make-frame-functions 'distopico:set-theme-window-system)

(provide 'setup-theme)
