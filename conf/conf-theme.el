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

(defcustom default-gui-theme 'tron
  "default gui theme")

(defcustom default-cli-theme 'tron-legacy
  "default cli theme")

(defun toggle-cli-theme ()
(interactive)
(let ((is-gui (find default-gui-theme custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme (if is-gui default-cli-theme default-gui-theme))))

(global-set-key (kbd "M-p") 'toggle-cli-theme)  
                               
(defun pick-color-theme (frame)
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

(add-hook 'after-make-frame-functions 'pick-color-theme)
      
;; For when started with emacs or emacs -nw rather than emacs --daemon
(if window-system
  	(disable-theme default-cli-theme)
	(enable-theme default-gui-theme))
    

(provide 'conf-theme)
