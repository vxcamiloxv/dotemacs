;;; Code:
(require 'flyspell)
(require 'flyspell-correct)
(require 'flyspell-correct-ido)

(defcustom distopico:flyspell-prog-only-comments t
  "Non-nil means Flyspell reports comments in `prog-mode'."
  :group 'flyspell
  :type 'boolean)

;; Re-define some keys
(define-key flyspell-mode-map (kbd "C-c $") #'flyspell-correct-at-point)
(define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper)

;; Functions
(defun distopico:flyspell-prog-mode-hook ()
  "Hook when turn on `flyspell-prog-mode'."
  (when distopico:flyspell-prog-only-comments
    (setq-local flyspell-prog-text-faces
                '(font-lock-comment-face font-lock-doc-face))))

(defun distopico:flyspell-enable-hook ()
  "Function to enable spell in deferments modes hooks."
  (when (executable-find ispell-program-name)
    (flyspell-mode 1)
    (add-hook 'flyspell-mode-hook 'distopico:flyspell-auto-dictionary nil 'make-it-local)))

(defun distopico:flyspell-auto-dictionary ()
  "Enabled auto directory per mode."
  (auto-dictionary-mode 1))

;; Hooks
(add-hook 'flyspell-prog-mode-hook #'distopico:flyspell-prog-mode-hook)
(add-hook 'org-mode-hook #'distopico:flyspell-enable-hook)

(provide 'conf-flyspell)
