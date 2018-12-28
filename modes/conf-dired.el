;;; Code:
(require 'dired+)
(require 'dired-x)
(require 'image-dired+)
(require 'dired-details+)
(require 'direx-project)
(require 'dired-subtree)
(require 'dired-hacks-utils)
(require 'dired-ranger)
(require 'dired-narrow)
(require 'dired-sort)
(require 'single-dired)
(require 'dired-imenu)

;; Autoload
(autoload 'dired-async-mode "dired-async.el" nil t)

;; Async mode
(dired-async-mode t)

;; Config
(setq dired-omit-files-p t
      dired-omit-mode t
      dired-omit-files "^\\..*[a-zA-Z]"
      dired-listing-switches "-al"
      dired-dwim-target t ;; Move files between split panes
      dired-details-hidden-string " "
      diredp-font-lock-keywords-1
      (append
       diredp-font-lock-keywords-1
       (list
        (list dired-re-exe
              `(".+"
                (dired-move-to-filename)
                nil
                (0 diredp-executable-file-name t)))))
      image-dired-track-movement nil
      image-dired-dir (expand-file-name "~/.thumbnails/emacs")
      image-dired-db-file (in-emacs-d ".cache/image-dired/image-dired_db")
      image-dired-temp-image-file (in-emacs-d ".cache/image-dired/image-dired_temp")
      image-dired-gallery-dir (in-emacs-d ".cache/image-dired/image-dired_gallery"))

;; Enable details
(dired-details-install)

;; Dired image+
(eval-after-load 'image-dired+ '(image-diredx-async-mode 1))
(eval-after-load 'image-dired+ '(image-diredx-adjust-mode 1))

;; Faces
(defface diredp-executable-file-name
  '((t (:foreground "Red" :weight bold)))
  "*Face used for names of executable files in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-executable-file-name 'diredp-executable-file-name)

;; Custom keymap
(define-key dired-mode-map (kbd "C-<right>") 'dired-subtree-insert)
(define-key dired-mode-map (kbd "C-<left>") 'dired-subtree-remove)
(define-key dired-mode-map [mouse-3] 'dired-maybe-insert-subdir)
(define-key dired-mode-map (kbd "C-{") 'distopico:dired-narrow-window)
(define-key dired-mode-map "e" 'distopico:dired-ediff-files)
(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'distopico:dired-jump-to-bottom)
(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'distopico:dired-back-to-top)

(define-key image-dired-thumbnail-mode-map "\C-n" 'image-diredx-next-line)
(define-key image-dired-thumbnail-mode-map "\C-p" 'image-diredx-previous-line)
(define-key image-dired-thumbnail-mode-map "g" 'revert-buffer)
(define-key image-dired-thumbnail-mode-map "x" 'image-diredx-flagged-delete)

;; Functions
(defun distopico:dired-narrow-window ()
  "Make the current dired mode window 30 chars wide."
  (interactive)
  (adjust-window-trailing-edge (selected-window) (- 30 (window-width)) t))

(defun distopico:dired-mode-hook ()
  "Enable modes in dired."
  (visual-line-mode 0) ;; unwrap lines.
  (linum-mode 0) ;; turn off line numbers.
  (hl-line-mode) ;; hl-line - highlight current-line
  (auto-revert-mode) ;; auto-refresh dired
  (font-lock-mode 1) ;; Switch-on font-lock

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
  "Make `end-of-buffer' and `beginning-of-buffer' behave properly."
  (interactive)
  (beginning-of-buffer)
  (next-line 2))

(defun distopico:dired-jump-to-bottom ()
  "Jump to button of buffer."
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

(defun distopico:dired-ediff-files ()
  "Ediff two marked files.
from: https://oremacs.com/."
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "No more than 2 files should be marked"))))

;; Modified behavior
(defadvice dired-insert-directory
    (before my-dired-insert-directory
            (dir switches &optional file-list wildcard hdr))
  "And hack the dired-insert-directory function to add the group-directories-first option."
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
