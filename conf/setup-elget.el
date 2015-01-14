(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(mapc (lambda (filename)
        (let ((filename (expand-file-name filename el-get-dir)))
          (when (file-directory-p filename)
            (setq load-path (cons filename load-path)))))
      (cddr (directory-files el-get-dir)))

(require 'el-get)

;; local sources
(setq
 el-get-byte-compile t
 el-get-generate-autoloads t
 el-get-sources

 '((:name "popwin"
          :description "Popwin Mode"
          :type git
          :url "https://github.com/m2ym/popwin-el.git")
   (:name "neotree"
          :description "Neotree Mode"
          :type git
          :url "https://github.com/jaypei/emacs-neotree.git")
   (:name "emacs-nav"
          :description "Emacs Nav Mode"
          :type git
          :url "https://github.com/ancane/emacs-nav")
   (:name "guide-key"
          :description "Guide key bindings"
          :type git
          :url "https://github.com/kai2nenobu/guide-key.git")
   (:name "powerline"
          :description "Powerline"
          :type git
          :url "https://github.com/milkypostman/powerline")
   (:name "python-django"
          :description "Python Django"
          :type git
          :url "https://github.com/fgallina/python-django.el.git")
   (:name "emacs-for-python"
          :description "emacs-for-python"
          :type git
          :url "https://github.com/gabrielelanaro/emacs-for-python.git")
   (:name "Project Explorer"
          :description "Project Explorer"
          :type git
          :url "https://github.com/sabof/project-explorer.git")
   (:name "dark-theme"
          :description "Dark theme"
          :type http
          :url "https://raw.github.com/suvayu/.emacs.d/master/themes/dark-emacs-theme.el")
   (:name "highlight-sexps"
          :description "Highlight sexps"
          :type http
          :url "http://david.rysdam.org/src/emacs/highlight-sexps.el")
   (:name "highlight-parentheses"
          :description "Highlight parentheses"
          :type git
          :url "https://github.com/nschum/highlight-parentheses.el.git")
   (:name "hideshowvis"
          :description "Hide Show Vis"
          :type git
          :url "https://github.com/emacsmirror/hideshowvis.git")
   (:name "naquadah-theme"
          :description "Naquadah theme"
          :type git
          :url "https://github.com/suhailshergill/naquadah-theme.git")
   (:name "kivy-mode"
          :description "Kivy highlighting mode"
          :type http
          :url "https://raw.github.com/kivy/kivy/master/kivy/tools/highlight/kivy-mode.el")
   (:name "web-mode"
          :description "emacs major mode for html templates"
          :type git
          :url "https://github.com/fxbois/web-mode.git")
   (:name "org-reveal"
          :description "reveal.js stuff for orgmode"
          :type git
          :url "https://github.com/yjwen/org-reveal.git")
   (:name "git-modes"
          :type git
          :url "https://github.com/magit/git-modes.git")
   (:name "cyberpunk-theme"
          :description "Cyberpunk theme"
          :type git
          :url "https://github.com/n3mo/cyberpunk-theme.el.git")
   (:name goto-last-change
          :after (progn
                   (global-set-key (kbd "C-x C-_") 'goto-last-change)))
   ))

(setq elget-packages
      (append
       '("el-get")
       (mapcar 'el-get-source-name el-get-sources)
                                        ;'("magit")
       ))

(el-get 'sync elget-packages)

(provide 'setup-elget)
