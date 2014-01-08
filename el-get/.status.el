((ace-jump-mode status "installed" recipe
		(:name ace-jump-mode :website "https://github.com/winterTTr/ace-jump-mode/wiki" :description "A quick cursor location minor mode for emacs" :type github :pkgname "winterTTr/ace-jump-mode" :features ace-jump-mode))
 (auto-complete status "installed" recipe
		(:name auto-complete :website "https://github.com/auto-complete/auto-complete" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
		       (popup fuzzy)))
 (bbdb status "installed" recipe
       (:name bbdb :website "http://bbdb.sourceforge.net/" :description "The Insidious Big Brother Database (BBDB) is a contact management utility." :type git :url "git://git.savannah.nongnu.org/bbdb.git" :load-path
	      ("./lisp")
	      :build
	      `("./autogen.sh" "./configure" "make")
	      :features bbdb-loaddefs :autoloads nil :info "doc" :post-init
	      (bbdb-initialize)))
 (browse-kill-ring status "installed" recipe
		   (:name browse-kill-ring :description "Interactively insert items from kill-ring" :type github :pkgname "browse-kill-ring/browse-kill-ring" :prepare
			  (progn
			    (autoload 'browse-kill-ring-default-keybindings "browse-kill-ring"))))
 (calfw status "installed" recipe
	(:name calfw :type github :pkgname "kiwanami/emacs-calfw" :load-path "." :description "A calendar framework for Emacs (with support for `org-mode', `howm' and iCal files)" :website "https://github.com/kiwanami/emacs-calfw"))
 (cl-lib status "installed" recipe
	 (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (ctable status "installed" recipe
	 (:name ctable :description "Table Component for elisp" :type github :pkgname "kiwanami/emacs-ctable"))
 (dark-theme status "installed" recipe
	     (:name "dark-theme" :description "Dark theme" :type http :url "https://raw.github.com/suvayu/.emacs.d/master/themes/dark-emacs-theme.el"))
 (dash status "installed" recipe
       (:name dash :description "A modern list api for Emacs. No 'cl required." :type github :pkgname "magnars/dash.el"))
 (deferred status "installed" recipe
   (:name deferred :description "Simple asynchronous functions for emacs lisp" :website "https://github.com/kiwanami/emacs-deferred" :type github :pkgname "kiwanami/emacs-deferred" :features "deferred"))
 (dired+ status "installed" recipe
	 (:name dired+ :description "Extensions to Dired" :type emacswiki :features dired+))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (emacs-w3m status "required" recipe nil)
 (epc status "installed" recipe
      (:name epc :description "An RPC stack for Emacs Lisp" :type github :pkgname "kiwanami/emacs-epc" :depends
	     (deferred ctable)))
 (flymake-cursor status "installed" recipe
		 (:name flymake-cursor :type github :pkgname "illusori/emacs-flymake-cursor" :description "displays flymake error msg in minibuffer after delay (illusori/github)" :website "http://github.com/illusori/emacs-flymake-cursor"))
 (fuzzy status "installed" recipe
	(:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (git-modes status "installed" recipe
	    (:name "git-modes" :type git :url "https://github.com/magit/git-modes.git"))
 (highlight-parentheses status "installed" recipe
			(:name "highlight-parentheses" :description "Highlight parentheses" :type git :url "https://github.com/nschum/highlight-parentheses.el.git"))
 (highlight-sexps status "installed" recipe
		  (:name "highlight-sexps" :description "Highlight sexps" :type http :url "http://david.rysdam.org/src/emacs/highlight-sexps.el"))
 (hy-mode status "installed" recipe
	  (:name hy-mode :website "https://github.com/hylang/hy-mode" :description "Emacs support for the Hy language." :type github :pkgname "hylang/hy-mode"))
 (identica-mode status "installed" recipe
		(:name identica-mode :description "An identi.ca/status.net client" :type git :url "http://git.savannah.gnu.org/cgit/identica-mode.git" :features identica-mode))
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
 (naquadah-theme status "installed" recipe
		 (:name "naquadah-theme" :description "Naquadah theme" :type git :url "git://git.naquadah.org/naquadah-theme.git"))
 (org-mode status "installed" recipe
	   (:name org-mode :website "http://orgmode.org/" :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system." :type git :url "git://orgmode.org/org-mode.git" :info "doc" :build/berkeley-unix `,(mapcar
																																				       (lambda
																																					 (target)
																																					 (list "gmake" target
																																					       (concat "EMACS="
																																						       (shell-quote-argument el-get-emacs))))
																																				       '("oldorg"))
		  :build `,(mapcar
			    (lambda
			      (target)
			      (list "make" target
				    (concat "EMACS="
					    (shell-quote-argument el-get-emacs))))
			    '("oldorg"))
		  :load-path
		  ("." "lisp" "contrib/lisp")))
 (org-reveal status "installed" recipe
	     (:name "org-reveal" :description "reveal.js stuff for orgmode" :type git :url "https://github.com/yjwen/org-reveal.git"))
 (package status "installed" recipe
	  (:name package :description "ELPA implementation (\"package.el\") from Emacs 24" :builtin "24" :type http :url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el" :shallow nil :features package :post-init
		 (progn
		   (setq package-user-dir
			 (expand-file-name
			  (convert-standard-filename
			   (concat
			    (file-name-as-directory default-directory)
			    "elpa")))
			 package-directory-list
			 (list
			  (file-name-as-directory package-user-dir)
			  "/usr/share/emacs/site-lisp/elpa/"))
		   (make-directory package-user-dir t)
		   (unless
		       (boundp 'package-subdirectory-regexp)
		     (defconst package-subdirectory-regexp "^\\([^.].*\\)-\\([0-9]+\\(?:[.][0-9]+\\)*\\)$" "Regular expression matching the name of\n a package subdirectory. The first subexpression is the package\n name. The second subexpression is the version string."))
		   (setq package-archives
			 (bound-and-true-p package-archives))
		   (mapc
		    (lambda
		      (pa)
		      (add-to-list 'package-archives pa 'append))
		    '(("ELPA" . "http://tromey.com/elpa/")
		      ("gnu" . "http://elpa.gnu.org/packages/")
		      ("marmalade" . "http://marmalade-repo.org/packages/")
		      ("SC" . "http://joseito.republika.pl/sunrise-commander/"))))))
 (paredit status "installed" recipe
	  (:name paredit :description "Minor mode for editing parentheses" :type http :prepare
		 (progn
		   (autoload 'enable-paredit-mode "paredit")
		   (autoload 'disable-paredit-mode "paredit"))
		 :url "http://mumble.net/~campbell/emacs/paredit.el"))
 (popup status "installed" recipe
	(:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :submodule nil :pkgname "auto-complete/popup-el"))
 (rainbow-mode status "installed" recipe
	       (:name rainbow-mode :description "Colorize color names in buffers" :minimum-emacs-version 24 :type elpa))
 (smartparens status "installed" recipe
	      (:name smartparens :description "Autoinsert pairs of defined brackets and wrap regions" :type github :pkgname "Fuco1/smartparens" :depends dash))
 (smex status "installed" recipe
       (:name smex :description "M-x interface with Ido-style fuzzy matching." :type github :pkgname "nonsequitur/smex" :features smex :post-init
	      (smex-initialize)))
 (visual-regexp status "installed" recipe
		(:name "visual-regexp" :description "Visual regexps" :type git :url "https://github.com/benma/visual-regexp.el.git"))
 (web-mode status "installed" recipe
	   (:name "web-mode" :description "emacs major mode for html templates" :type git :url "https://github.com/fxbois/web-mode.git"))
 (yasnippet status "installed" recipe
	    (:name "yasnippet" :description "YASnippet is a template system for Emacs." :type git :url "https://github.com/capitaomorte/yasnippet.git")))
