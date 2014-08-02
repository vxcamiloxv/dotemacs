;; --------
;; Mozrepl
;; --------

(load-file "~/.emacs.d/modes/moz.el")

(require 'js2-mode)
(require 'moz)
(require 'json)
 
(defun moz-update (&rest ignored)
  "Update the remote mozrepl instance"
  (interactive)
  (comint-send-string (inferior-moz-process)
    (concat "content.document.body.innerHTML="
             (json-encode (buffer-string)) ";")))
 
(defun moz-enable-auto-update ()
  "Automatically the remote mozrepl when this buffer changes"
  (interactive)
  (add-hook 'after-change-functions 'moz-update t t))
 
(defun moz-disable-auto-update ()
  "Disable automatic mozrepl updates"
  (interactive)
  (remove-hook 'after-change-functions 'moz-update t))

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'java-custom-setup)
(defun javascript-custom-setup () (moz-minor-mode 1))
