;;; Code:
(require 'emr)
(require 'move-dup)

(setq semanticdb-default-save-directory (in-emacs-d ".cache/semanticdb"))
;; Load several utils for programming mode
(add-hook 'prog-mode-hook 'distopico:prog-mode-hook)

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
  ;; Emacs refactor
  (emr-initialize)
  ;; Moving and duplicating lines or rectangles
  (move-dup-mode))

(provide 'conf-prog)
