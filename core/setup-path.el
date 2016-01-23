;;; Code:

(defvar core-dir (concat user-emacs-directory "core")
  "All essencial settings")
(defvar mode-dir (concat user-emacs-directory "modes")
  "My config for emacs modes")
(defvar site-elisp-dir  (concat user-emacs-directory "site-elisp")
  "Extenal emacs libs/modes")
(defvar utils-dir (concat user-emacs-directory "utils")
  "Useful functions to live in emacs")
(defvar theme-dir (concat user-emacs-directory "themes")
  "My favorites templates")
(defvar distopico:load-path `(,core-dir ,mode-dir ,site-elisp-dir ,utils-dir ,theme-dir)
  "Directories relative to `user-emacs-directory', to be include in `load-path'")
(defvar distopico:exec-paths
  '("~/.emacs.d/node_modules/.bin"
    "~/.emacs.d/.python-environments/default/bin")
  "A list of custom bin paths")

;; General Paths
(defun in-emacs-d (path)
  (expand-file-name (concat user-emacs-directory path)))
(defun in-modes-d (path)
  (expand-file-name (concat mode-dir path)))
(defun in-site-elisp-d (path)
  (expand-file-name (concat site-elisp-dir path)))

;; Functions
(defun distopico:from-modes-d (path &optional symbol)
  (add-to-list 'load-path (in-modes-d path))
  (if symbol
      (require symbol)
    (load-library path)))

(defun distopico:from-site-elisp-d (path &optional symbol)
  (add-to-list 'load-path (in-site-elisp-d path))
  (if symbol
      (require symbol)
    (load-library path)))

(defun distopico:autoload-and-run (symbol file interactive callback)
  (autoload symbol file nil interactive)
  (eval-after-load (symbol-name symbol) callback))


(defun distopico:path-join (&rest parts)
  (concat
   (mapconcat 'file-name-as-directory (butlast parts) "")
   (car (last parts))))

(defun distopico:get-absolute-path (filename)
  (if (file-name-absolute-p filename)
      (expand-file-name filename)
    (distopico:path-join user-emacs-directory filename)))

(defun distopico:get-path-subdirs (path-dir)
  "Returns a list of subdirectories"
  (mapcar (lambda (x) (distopico:path-join path-dir x))
          (cl-remove-if
           (lambda (x)
             (let ((abs-path (distopico:path-join path-dir x)))
               (or (string= x ".")
                   (string= x "..")
                   (not (file-accessible-directory-p abs-path)))))
           (directory-files path-dir))))

(defun distopico:load-path-subdir(path-dir &optional path-exclude)
  (unless (file-exists-p path-dir)
    (make-directory path-dir))

  (if (< emacs-major-version 24)
      (progn
        (let ((base path-dir))
          (add-to-list 'load-path base)
          (dolist (f (directory-files base))
            (let ((name (concat base "/" f)))
              (when (and (file-directory-p name)
                         (not (equal f ".."))
                         (not (equal f ".")))
                (add-to-list 'load-path name))))))
    (progn
      (require 'cl)
      (require 'cl-lib)
      (let ((default-directory path-dir))
        (setq load-path
              (append
               (let ((load-path (copy-sequence load-path))) ;; Shadow
                 (append
                  (copy-sequence (normal-top-level-add-to-load-path '(".")))
                  (normal-top-level-add-subdirs-to-load-path)))
               load-path))))))

(defun distopico:to-list-path-subdir (path-dir appent-to)
  (add-to-list appent-to path-dir)
  (dolist (abs-dir (distopico:get-path-subdirs path-dir))
    (add-to-list appent-to abs-dir)))

(defun distopico:startup-get-all-user-lisp-dirs ()
  "Returns a list of absolute paths for all
lisp directories containing code under the control of the user."
  (let ((user-lisp-dirs ()))
    (dolist (path-dir distopico:load-path)
      (add-to-list 'user-lisp-dirs (expand-file-name path-dir))
      (dolist (list-paths (distopico:get-path-subdirs path-dir))
        (add-to-list 'user-lisp-dirs  (expand-file-name list-paths))))
    (mapcar 'distopico:get-absolute-path user-lisp-dirs)))

;;;###autoload
(defun distopico:startup-byte-recompile ()
  "Compile all packages in `distopico:load-path'"
  (interactive)
  (dolist (abs-dir (distopico:startup-get-all-user-lisp-dirs))
    (message abs-dir)
    (let ((generated-autoload-file (distopico:path-join abs-dir "loaddefs.el")))
      (update-directory-autoloads abs-dir))
    (byte-recompile-directory abs-dir 0)))

;;;###autoload
(defun distopico:startup-load-path ()
  (dolist (path-dir distopico:load-path)
    (distopico:load-path-subdir path-dir))
  ;; (dolist (abs-dir (arv/startup-get-all-user-lisp-dirs))
  ;;   (add-to-list 'load-path abs-dir))
  ;; Add cutoms exec-path
  ;; (setenv "PATH" (mapconcat 'identity my-exec-paths  ":") )
  (setenv "PATH" (concat (getenv "PATH") ":" (mapconcat 'identity distopico:exec-paths ":") ))
  (setq exec-path (append exec-path distopico:exec-paths)))

(provide 'setup-path)
