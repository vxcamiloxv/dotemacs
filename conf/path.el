;; General Paths

(defvar dotfiles-dir "~/.emacs.d/"
  "The root Emacs Lisp source folder")

(add-to-list 'load-path "~/.emacs.d")

(let ((default-directory dotfiles-dir))
  (setq load-path
      (append
        (let ((load-path (copy-sequence load-path))) ;; Shadow
          (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; Custom themes added to load-path
;; (when (= emacs-major-version 24)
;;  (add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes/")))

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
  
(provide 'path)
