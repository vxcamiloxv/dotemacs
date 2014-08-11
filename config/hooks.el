;;; package --- Conf Hooks
;;;
;;; Commentary:
;;; Hooks for each type of mode for me
;;;
;;; Code:

(add-hook 'after-init-hook
	  (lambda ()
	    (helm-mode 1)
	    (yas-global-mode 1)
	    (global-auto-complete-mode)
	    (projectile-global-mode)
	    (global-flycheck-mode)
	    (auto-indent-global-mode)
	    (autopair-global-mode)
	    (global-pretty-mode t)
	    (semantic-mode 1)
	    ;; (global-centered-cursor-mode)
	    (global-move-dup-mode)
	    (global-relative-buffers-mode)
	    (global-auto-highlight-symbol-mode)
	    (global-rainbow-delimiters-mode)))

(add-hook 'python-mode-hook
  (lambda ()
    (toggle-truncate-lines)
    (nlinum-mode)
    (setq ac-sources '(ac-source-features
		       ac-source-filename
		       ac-source-files-in-current-dir
		       ac-source-gtags
		       ac-source-semantic
		       ac-source-words-in-same-mode-buffers
		       ac-source-yasnippet))))

(add-hook 'go-mode-hook 'go-eldoc-setup)

(add-hook 'c++-mode
	  (lambda ()
	      (toggle-truncate-lines)
	      (nlinum-mode)
	      (setq ac-sources '(ac-source-features
				 ac-source-filename
				 ac-source-files-in-current-dir
				 ac-source-gtags
				 ac-source-semantic
				 ac-source-words-in-same-mode-buffers
				 ac-source-yasnippet))))

(add-hook 'term-mode-hook
	  (lambda ()
	    (toggle-truncate-lines)
	    (centered-cursor-mode -1)
	    (yas-minor-mode -1)))


(add-hook 'html-mode
          (lambda ()
	    (nlinum-mode)
	    (toggle-truncate-lines)
	    (setq ac-sources '(ac-source-css-property
			       ac-source-files-in-current-dir
			       ac-source-gtags ac-source-semantic
			       ac-source-words-in-same-mode-buffers
                               ac-source-yasnippet
			       ac-source-filename))))
(add-hook 'javascript-mode
	  (lambda ()
	      (nlinum-mode)
	      (toggle-truncate-lines)
	      (setq ac-sources '(ac-source-files-in-current-dir
                                 ac-source-gtags ac-source-semantic
                                 ac-source-words-in-same-mode-buffers
                                 ac-source-yasnippet
                                 ac-source-filename))))

(add-hook 'css-mode
          (lambda ()
	    (toggle-truncate-lines)
	    (nlinum-mode)
	    (setq ac-sources '(ac-source-css-property
			       ac-source-files-in-current-dir
			       ac-source-gtags ac-source-semantic
			       ac-source-words-in-same-mode-buffers
			       ac-source-yasnippet))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (toggle-truncate-lines)
	    (add-hook (make-local-variable 'after-init-hook)
		      (lambda ()
			(byte-force-recompile default-directory)))
	    (eldoc-mode)
	    (setq ac-sources '(ac-source-symbols
			       ac-source-words-in-same-mode-buffers
			       ac-source-functions ac-source-variables
			       ac-source-yasnippet))))

(add-hook 'lisp-interaction-mode-hook
	  (lambda ()
	    (toggle-truncate-lines)))

(add-hook 'text-mode
	  (lambda ()
	      (ac-ispell-ac-setup)))


(add-hook 'markdown-mode #'flyspell-mode)
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'before-save-hook 'py-autopep8-before-save)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'before-save-hook 'web-beautify-html-buffer t t)
(add-hook 'after-save-hook 'auto-byte-recompile)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setup)


(provide 'hooks)

;;; hooks ends here
