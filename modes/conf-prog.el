;;; Code:
(require 'ispell)
(require 'emr)
(require 'move-dup)
(require 'aggressive-indent)
(require 'dumb-jump)

(defconst distopico:editorconfig-regexp
  (concat "\\`" (regexp-quote ".editorconfig") "\\'"))

(setq semanticdb-default-save-directory (in-emacs-d ".cache/semanticdb")
      dumb-jump-selector 'ivy)

;; Define additional keybindings

;; Emcas refactor
(define-key prog-mode-map (kbd "C-M-<return>") 'emr-show-refactor-menu)

;; Exclude some modes fro agressive indent
(dolist (source '(diary-mode css-mode less-css-mode jade-mode))
  (add-to-list 'aggressive-indent-excluded-modes source t))

;; TODO: Check which-func (which-function-mode 1)

;; Functions
(defun distopico:local-comment-auto-fill ()
  "Set comment style."
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun distopico:font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming.
from: prelude"
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(defun distopico:prog-mode-hook ()
  "Hook for all modes derivate of `prog-mode'."
  (distopico:local-comment-auto-fill)
  (distopico:font-lock-comment-annotations)
  ;; Auto indent
  (aggressive-indent-mode t)
  ;; Emacs refactor
  (emr-initialize)
  ;; Moving and duplicating lines or rectangles
  (move-dup-mode)
  ;; Search references
  (dumb-jump-mode)
  ;; If editorconfig not found clean but only
  ;; if the whitespace in the buffer was initially clean
  (unless (distopico:locate-parent-file distopico:editorconfig-regexp)
    (whitespace-cleanup-mode t))

  (when (executable-find ispell-program-name)
    (flyspell-prog-mode)))

;; Load several utils for programming mode
(add-hook 'prog-mode-hook #'distopico:prog-mode-hook)

(provide 'conf-prog)
