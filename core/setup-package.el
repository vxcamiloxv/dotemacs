;;; Code:
;; Some functions adapted from https://github.com/patxoca/dot-emacs/ and prelude
;; When switching between Emacs 23 and 24, we always use the bundled package.el in Emacs 24

(let ((package-el-site-lisp-dir
       (expand-file-name "site-elisp/package" user-emacs-directory)))
  (when (and (file-directory-p package-el-site-lisp-dir)
             (> emacs-major-version 23))
    (message "Removing local package.el from load-path to avoid shadowing bundled version")
    (setq load-path (remove package-el-site-lisp-dir load-path))))

(require 'package)
(require 'packages)

(setq nsm-settings-file (expand-file-name ".cache/network-security.data" user-emacs-directory))

(dolist (source '(("gnu"       . "https://elpa.gnu.org/packages/")
                  ;; ("marmalade" . "http://marmalade-repo.org/packages/") marmalade died
                  ("melpa"     . "https://melpa.org/packages/")
                  ("org"       . "http://orgmode.org/elpa/")))
  (add-to-list 'package-archives source t))

;; Update
(package-initialize)


;; Auto install
(defun distopico-packages-installed-p ()
  (cl-loop for pkg in distopico-packages
        when (not (package-installed-p pkg)) do (cl-return nil)
        finally (cl-return t)))

(defun distopico:available-package-p (pkg)
  "Return t if NAME is an available package."
  (unless package-archive-contents
    (package-refresh-contents))
  (not (null (assoc pkg package-archive-contents))))

;;;###autoload
(defun distopico:ensure-required-packages ()
  "Check if dependencies are installer and update for new packages (package versions)"
  (interactive)
(unless (distopico-packages-installed-p)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (pkg distopico-packages)
    (when (and (distopico:available-package-p pkg)
               (not (package-installed-p pkg)))
      (package-install pkg)))))

;;;###autoload
(defun distopico:check-required-packages ()
  "Check if `distopico-packages' is up to date."
  (interactive)
  (dolist (pkg package-alist)
    (unless (member (car pkg) required-packages)
      (message "Package missing: %s" (car pkg)))))

;;;###autoload
(defun distopico:load-require-libs ()
  ;; Load all libs in utils-dir
  (dolist (file (directory-files utils-dir t "\\w+"))
    (when (file-regular-p file)
      (load file))))

;;;###autoload
(defun distopico:package-init-load-hook ()
  (if (getenv "EMACS_INSTALL")
      (distopico:startup-byte-recompile)))

;; Hooks
(add-hook 'distopico:after-init-load-hook 'distopico:package-init-load-hook)

;; Run package
(distopico:ensure-required-packages)


(provide 'setup-package)
