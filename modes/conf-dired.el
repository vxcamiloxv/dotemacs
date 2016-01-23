;;; Code:

(require 'dired+)
(require 'dired-details+)
(require 'direx-project)
(require 'dired-x)
(require 'single-dired)
(require 'fsdired)

;; Autoload
(autoload 'dired-async-mode "dired-async.el" nil t)

;; Async mdoe
(dired-async-mode t)

;; Config
(setq-default dired-omit-files-p t
              dired-omit-files "^\\.?#\\|^\\.[^\\.]+"
              dired-listing-switches "-al"
              diredp-font-lock-keywords-1
              (append
               diredp-font-lock-keywords-1
               (list
                (list dired-re-exe
                      `(".+"
                        (dired-move-to-filename)
                        nil
                        (0 diredp-executable-file-name t)))))
              image-dired-dir (expand-file-name "~/.thumbnails/emacs"))

;; Direx
(setq direx:leaf-icon " " direx:open-icon "▾ " direx:closed-icon "▸ ")

;; shorter dired output
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; Move files between split panes
(setq dired-dwim-target t)

;; Faces
(defface diredp-executable-file-name
  '((t (:foreground "Red" :weight bold)))
  "*Face used for names of executable files in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-executable-file-name 'diredp-executable-file-name)

;; Functions
(defun dired-narrow-window ()
  "make the current dired mode window 30 chars wide"
  (interactive)
  (adjust-window-trailing-edge (selected-window) (- 30 (window-width)) t))

(defun distopico:dired-mode-hook ()
  (visual-line-mode 0) ;; unwrap lines.
  (linum-mode 0) ;; turn off line numbers.
  (hl-line-mode) ;; hl-line - highlight current-line
  (auto-revert-mode) ;; auto-refresh dired
  (font-lock-mode 1) ;; Switch-on font-lock
  (define-key dired-mode-map [mouse-3] 'dired-maybe-insert-subdir)
  (define-key dired-mode-map (kbd "C-{") 'dired-narrow-window)
  (define-key dired-mode-map
    (vector 'remap 'end-of-buffer) 'distopico:dired-jump-to-bottom)
  (define-key dired-mode-map
    (vector 'remap 'beginning-of-buffer) 'distopico:dired-back-to-top)

  ;; Use the same buffer for visited directories
  (toggle-diredp-find-file-reuse-dir 1)

  ;; Set omit-mode by default
  (dired-omit-mode 1)

  ;; Ensure that the byte-compiled version picks-up the dired+ fonts
  (set (make-local-variable 'font-lock-defaults)
       (cons '(dired-font-lock-keywords diredp-font-lock-keywords-1)
             (cdr font-lock-defaults))))

;; Hooks
(add-hook 'dired-mode-hook 'distopico:dired-mode-hook)

(defun distopico:dired-back-to-top ()
  "make end-of-buffer and beginning-of-buffer behave properly"
  (interactive)
  (beginning-of-buffer)
  (next-line 2))

(defun distopico:dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (next-line -1))

(defun distopico:dired-next-file-line ()
  "Move to the next dired line that have a file or directory name on it."
  (interactive)
  (call-interactively 'dired-next-line)
  (if (eobp)
      (dired-previous-line 1)))

(defun distopico:dired-previous-file-line ()
  "Move to the previous dired line that have a file or directory name on it."
  (interactive)
  (call-interactively 'dired-previous-line)
  (if (not (dired-move-to-filename))
      (dired-next-line 1)))

(defadvice dired-insert-directory
    (before my-dired-insert-directory
            (dir switches &optional file-list wildcard hdr))
  "and hack the dired-insert-directory function to add the
--group-directories-first option:"
  (setq switches (concat switches " --group-directories-first")))
(ad-activate 'dired-insert-directory)

;; Reload dired after making changes
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
  (eval `(defadvice ,it (after revert-buffer activate)
           (revert-buffer))))

(provide 'conf-dired)
