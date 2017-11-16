;;; Code:
(require 'flyspell)
(require 'flyspell-correct)
(require 'flyspell-correct-ido)

(defcustom distopico:flyspell-prog-only-comments t
  "Non-nil means Flyspell reports comments in `prog-mode'."
  :group 'flyspell
  :type 'boolean)

;; Re-define some keys
(define-key flyspell-mode-map (kbd "C-c $") #'flyspell-correct-word-generic)
(define-key flyspell-mode-map flyspell-auto-correct-binding #'flyspell-correct-previous-word-generic)

;; Functions
(defun distopico:flyspell-prog-mode-hook ()
  "Hook when turn on `flyspell-prog-mode'."
  (when distopico:flyspell-prog-only-comments
    (setq-local flyspell-prog-text-faces
                '(font-lock-comment-face font-lock-doc-face))))

;; Hooks
(add-hook 'flyspell-prog-mode-hook #'distopico:flyspell-prog-mode-hook)

(provide 'conf-flyspell)
