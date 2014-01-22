((Project\ Explorer status "installed" recipe
		    (:name "Project Explorer" :description "Project Explorer" :type git :url "https://github.com/sabof/project-explorer.git"))
 (cl-lib status "installed" recipe
	 (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (cyberpunk-theme status "installed" recipe
		  (:name "cyberpunk-theme" :description "Cyberpunk theme" :type git :url "https://github.com/n3mo/cyberpunk-theme.el.git"))
 (dark-theme status "installed" recipe
	     (:name "dark-theme" :description "Dark theme" :type http :url "https://raw.github.com/suvayu/.emacs.d/master/themes/dark-emacs-theme.el"))
 (django-python status "installed" recipe
		(:name "django-python" :description "Django-Python" :type git :url "https://github.com/myfreeweb/django-mode.git"))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (emacs-for-python status "installed" recipe
		   (:name "emacs-for-python" :description "emacs-for-python" :type git :url "https://github.com/gabrielelanaro/emacs-for-python.git"))
 (expand-region status "installed" recipe
		(:name "expand-region" :description "Expand region" :type git :url "https://github.com/magnars/expand-region.el.git"))
 (flycheck status "installed" recipe
	   (:name "flycheck" :description "Flycheck" :type git :url "https://github.com/flycheck/flycheck.git"))
 (git-modes status "installed" recipe
	    (:name "git-modes" :type git :url "https://github.com/magit/git-modes.git"))
 (highlight-parentheses status "installed" recipe
			(:name "highlight-parentheses" :description "Highlight parentheses" :type git :url "https://github.com/nschum/highlight-parentheses.el.git"))
 (highlight-sexps status "installed" recipe
		  (:name "highlight-sexps" :description "Highlight sexps" :type http :url "http://david.rysdam.org/src/emacs/highlight-sexps.el"))
 (kivy-mode status "installed" recipe
	    (:name "kivy-mode" :description "Kivy highlighting mode" :type http :url "https://raw.github.com/kivy/kivy/master/kivy/tools/highlight/kivy-mode.el"))
 (magit status "installed" recipe
	(:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :depends
	       (cl-lib git-modes)
	       :info "." :build
	       (if
		   (version<= "24.3" emacs-version)
		   `(("make" ,(format "EMACS=%s" el-get-emacs)
		      "all"))
		 `(("make" ,(format "EMACS=%s" el-get-emacs)
		    "docs")))
	       :build/berkeley-unix
	       (("touch" "`find . -name Makefile`")
		("gmake"))))
 (multiple-cursors status "installed" recipe
		   (:name "multiple-cursors" :description "Multiple Cursors" :type git :url "https://github.com/emacsmirror/multiple-cursors.git"))
 (naquadah-theme status "installed" recipe
		 (:name "naquadah-theme" :description "Naquadah theme" :type git :url "https://github.com/suhailshergill/naquadah-theme.git"))
 (org-reveal status "installed" recipe
	     (:name "org-reveal" :description "reveal.js stuff for orgmode" :type git :url "https://github.com/yjwen/org-reveal.git"))
 (pony-mode status "installed" recipe
	    (:name "pony-mode" :description "Pony Mode" :type git :url "https://github.com/davidmiller/pony-mode"))
 (powerline status "installed" recipe
	    (:name "powerline" :description "Powerline" :type git :url "https://github.com/milkypostman/powerline"))
 (visual-regexp status "installed" recipe
		(:name "visual-regexp" :description "Visual regexps" :type git :url "https://github.com/benma/visual-regexp.el.git"))
 (web-mode status "installed" recipe
	   (:name "web-mode" :description "emacs major mode for html templates" :type git :url "https://github.com/fxbois/web-mode.git"))
 (yasnippet status "installed" recipe
	    (:name yasnippet :website "https://github.com/capitaomorte/yasnippet.git" :description "YASnippet is a template system for Emacs." :type github :pkgname "capitaomorte/yasnippet" :features "yasnippet" :compile "yasnippet.el")))
