(require 'package)
(require 'my-package)

(dolist (source '(("gnu" . "http://elpa.gnu.org/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa" . "http://melpa.org/packages/")
                  ))

  (add-to-list 'package-archives source t))
(package-initialize)


(defun distopico-packages-installed-p ()
  (loop for p in distopico-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (distopico-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p distopico-packages)
    (when (not (package-installed-p p))
      (package-install p))))


(provide 'setup-package)
