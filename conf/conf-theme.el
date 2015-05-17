;; Load path them
(require 's)
(require 'dash)

(-each
 (-map
  (lambda (item)
    (format "~/.emacs.d/themes/" item))
  (-filter
   (lambda (item) (s-contains? "theme" item))
   (directory-files "~/.emacs.d/themes/")))
 (lambda (item)
   (add-to-list 'custom-theme-load-path item)))

;; Load colors and theme things
;; (load-file "~/.emacs.d/emacs/colors.el")
;; (load-file "~/.emacs.d/themes/dark-emacs-theme.el")
;; (require 'dark-emacs-theme)
;; (package-initialize)
;; (load-theme 'zenburn t)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
;; (load-theme 'solarized-dark t)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized-emacs")
;; (load-theme 'solarized-dark t)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-tron-theme")
;(load-theme 'tron t)

;; last t is for NO-ENABLE
(load-theme 'tron t)
;(load-theme 'tron-legacy t t)

(defvar default-gui-theme 'tron
  "default gui theme")

(defvar default-cli-theme 'nil
  "default cli theme")

(defun toggle-cli-theme ()
(interactive)
(let ((is-gui (find default-gui-theme custom-enabled-themes)))
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  ;; load-theme
  (if is-gui
      (progn
        (if (not (eq default-cli-theme 'nil))
            (enable-theme default-cli-theme)))
    (progn
      (if (not (eq default-gui-theme 'nil))
          (enable-theme default-gui-theme)))
    )))

(defun custom-theme-cli (frame)
  (select-frame frame)
  (if (window-system frame)
      (progn
        (disable-theme default-cli-theme)
        (enable-theme default-gui-theme)
        )
    (progn
      (disable-theme default-gui-theme)
      (disable-theme default-cli-theme)
      )))

(add-hook 'after-make-frame-functions 'custom-theme-cli)

;; ;; For when started with emacs or emacs -nw rather than emacs --daemon
;; (if window-system
;;   	(disable-theme default-cli-theme)
;; 	(enable-theme default-gui-theme))

(provide 'conf-theme)