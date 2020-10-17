;;; conf-flyspell.el --- Flyspell with multi-language/multi-dictionary
;;; support for `text-mode', `org-mode' etc, and `prog-mode'.

;;; Commentary:

;; Usually I write code on english (comments/vars/commits etc)
;; but my `org-mode' notes/todos or my attempt of blog are a
;; mix between English/Spanish, so I was looking for an alternative
;; to have spellchecker in both languages, ideally at the same time
;; or per buffer, after tested `auto-dictionary' and `guess-language'
;; even `spell-fu' no of those solved my needs, so after read the docs
;; of `aspell' I found out it has a option for multi-dictionaries
;; `--add-extra-dicts' but not worked to me, in reddit I found
;; the solution with `hunspell' the works really great with
;; multi-language after some additional tweaks/advices.
;; see `https://www.reddit.com/r/emacs/comments/8kbdhq/emails_with_spellcheck_in_two_languages')

;;; Code:
(require 'flyspell)
(require 'flyspell-correct)
(require 'flyspell-correct-ido)

(defcustom distopico:flyspell-prog-only-comments t
  "Non-nil means Flyspell reports comments in `prog-mode'."
  :group 'flyspell
  :type 'boolean)
(defcustom distopico:default-dictionaries '("en_GB" "en_US" "en" "es_ANY" "es_CO" "es_MX")
  "List of dictionaries to load global by default."
  :group 'flyspell
  :type 'list)

(defvar distopico:ispell-current-dictionary nil
  "Current dictionary to check if is necesary print meesage about new dictionary.")

;; Functions
(defun distopico:flyspell-get-available-dictionaries ()
  "Check available dictionaries from `distopico:default-dictionaries'.
return return a string of those available."
  (let ((dictionaries nil)
        (missing nil))
    (dolist (own-dic distopico:default-dictionaries dictionaries)
      (if (assoc own-dic ispell-hunspell-dict-paths-alist)
          (setq dictionaries (cons own-dic dictionaries))
        (setq missing (cons own-dic missing))))
    (when missing
      (message (format "flyspell: Dictionary [%s] is not available" (mapconcat 'concat missing ","))))
    (when dictionaries
        (mapconcat 'concat dictionaries ","))))

(defun distopico:flyspell-update-dictionaries ()
  "Set and update available dictionaries."
  (interactive)
  (ispell-set-spellchecker-params)
  (setq ispell-dictionary (distopico:flyspell-get-available-dictionaries))
  ;; ispell-set-spellchecker-params has to be called
  ;; to setup the dictionaries path
  (ispell-hunspell-add-multi-dic ispell-dictionary)
  ;; Defines as local dictionary to avoid kill/start ispell all the time
  (setq ispell-local-pdict ispell-dictionary))

(defun distopico:flyspell-prog-mode-hook ()
  "Hook when turn on `flyspell-prog-mode' to only check in comments."
  (when distopico:flyspell-prog-only-comments
    (setq-local flyspell-prog-text-faces
                '(font-lock-comment-face font-lock-doc-face))))

(defun distopico:flyspell-enable-hook ()
  "Enable spell in different modes hooks."
  (when (executable-find ispell-program-name)
    (flyspell-mode)))

;; Advice
(defun distopico:ispell-init-process (orig-fun &rest args)
  "Advice to avoid unnecessary `ispell' if the message not changed.
this call the `ORIG-FUN' with `ARGS' but inhibit the message
that show on each buffer switch, now only display the message if the
dictionary `ispell-current-dictionary' really changed."
  (if (equal ispell-current-dictionary distopico:ispell-current-dictionary)
      (let ((inhibit-message t)
            (message-log-max nil))
        (apply orig-fun args))
    (progn
      (setq distopico:ispell-current-dictionary ispell-current-dictionary)
      (apply orig-fun args))))

(defun distopico:ispell-kill-ispell (orig-fun &rest args)
  "Advice to avoid annoying kill `ìspell' process if not a interactive call.
this call the `ORIG-FUN' with the original `ARGS' but inhibit kill message
because looks like an error, kill the process on each buffer switch make sense
to avoid call the incorrect dictionary per buffer but is not really necessary
show the message if not a interactive call."
  (if (called-interactively-p 'interactive)
      (apply orig-fun args)
    (let ((inhibit-message t)
          (message-log-max nil))
      (apply orig-fun args))))

(advice-add 'ispell-init-process :around #'distopico:ispell-init-process)
(advice-add 'ispell-kill-ispell :around #'distopico:ispell-kill-ispell)

;; Hooks
(add-hook 'flyspell-prog-mode-hook #'distopico:flyspell-prog-mode-hook)

;; Init
(with-eval-after-load "ispell"
  (setq flyspell-use-meta-tab nil
        ispell-program-name (executable-find "hunspell"))
  ;; Update available dictionaries
  (distopico:flyspell-update-dictionaries)
  ;; Re-define some keys
  (define-key flyspell-mode-map (kbd "C-c $") #'flyspell-correct-at-point)
  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper)
  (define-key flyspell-mode-map (kbd "C-M-i") nil))

;;; conf-flyspell ends here
(provide 'conf-flyspell)
