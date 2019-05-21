;;; Code:
(require 'ispell)
(require 'emr)
(require 'move-dup)
;; (require 'aggressive-indent)
(require 'dumb-jump)

;; Control
(defconst distopico:editorconfig-regexp
  (concat "\\`" (regexp-quote ".editorconfig") "\\'"))

;; Configuration
(setq semanticdb-default-save-directory (in-emacs-d ".cache/semanticdb")
      dumb-jump-selector 'ivy)

;; Exclude some modes fro agressive indent
;; (dolist (source '(diary-mode css-mode less-css-mode jade-mode))
;;   (add-to-list 'aggressive-indent-excluded-modes source t))

;; TODO: Check which-func (which-function-mode 1)

;; Native emacs pair
(electric-pair-mode t)

;; Define additional/custom keybindings
(define-key prog-mode-map (kbd "C-M-<return>") 'emr-show-refactor-menu)
(global-set-key [home] 'distopico:smart-beginning-of-line)
(global-set-key (kbd "C-a") 'distopico:smart-beginning-of-line)

;; Functions
(defun distopico:smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line'.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

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
  ;; Return and indent
  (local-set-key [(return)] 'newline-and-indent)
  ;; Custom behavior
  (distopico:local-comment-auto-fill)
  (distopico:font-lock-comment-annotations)
  ;; Automatic symbol highlighting
  (highlight-symbol-mode)
  ;; Enable docs
  (eldoc-mode)
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
  ;; Enable spell check on comments
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode)))

;; Hooks
(add-hook 'prog-mode-hook #'distopico:prog-mode-hook)

(provide 'conf-prog)
