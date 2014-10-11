;; General Paths
(defvar dotfiles-dir "~/.emacs.d"
	  "The root Emacs Lisp source folder")

(if (< emacs-major-version 24)

(progn
	(let ((base dotfiles-dir))
	  (add-to-list 'load-path base)
	  (dolist (f (directory-files base))
	    (let ((name (concat base "/" f)))
	      (when (and (file-directory-p name)
	                 (not (equal f ".."))
	                 (not (equal f ".")))
	        (add-to-list 'load-path name)))))
	)

(progn
	(require 'cl)
	(require 'cl-lib)

	(add-to-list 'load-path dotfiles-dir)

	(let ((default-directory dotfiles-dir))
	  (setq load-path
	      (append
	        (let ((load-path (copy-sequence load-path))) ;; Shadow
	          (append
	            (copy-sequence (normal-top-level-add-to-load-path '(".")))
	            (normal-top-level-add-subdirs-to-load-path)))
	         load-path)))

	)
)

;; Add cutoms exec-path
(defvar my-exec-paths 
    '(
       "~/.emacs.d/node_modules/.bin"
       "~/.emacs.d/.python-environments/default/bin"
     ) 
    "A list of custom bin paths"
)
 
;; (setenv "PATH" (mapconcat 'identity my-exec-paths  ":") )
(setenv "PATH" (concat (getenv "PATH") ":" (mapconcat 'identity my-exec-paths ":") ))

(setq exec-path (append exec-path my-exec-paths))


;; Custom themes added to load-path
;; (when (= emacs-major-version 24)
;;  (add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes/")))

(provide 'path)
