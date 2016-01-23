;;; Code:
;; Buffer-related defuns

(require 'imenu)
(require 's)

(defvar buffer-local-mode nil)
(make-variable-buffer-local 'buffer-local-mode)

;; Toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])

(defun mode-keymap (mode-sym)
  (symbol-value (intern (concat (symbol-name mode-sym) "-map"))))

(defun* buffer-local-set-key (key action)
  (when buffer-local-mode
    (define-key (mode-keymap buffer-local-mode)
      key action)
    (return-from set-key-buffer-local))
  (let* ((mode-name-loc (gensym "-blm")))
    (eval `(define-minor-mode ,mode-name-loc nil nil nil (make-sparse-keymap)))
    (setq buffer-local-mode mode-name-loc)
    (funcall mode-name-loc 1)
    (define-key (mode-keymap mode-name-loc) key action)))

(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (funcall initial-major-mode)
    (insert initial-scratch-message)))

(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun kill-this-buffer-and-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also.
   from: http://www.emacswiki.org/emacs/KillingBuffers"
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun kill-next-buffer-and-pane ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also.
  from: http://www.emacswiki.org/emacs/KillingBuffers"
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun get-buffers-with-major-mode (mode)
  "Returns list of buffers with major-mode MODE or derived from MODE."
  (cl-loop
   for buf in (buffer-list)
   if (and (buffer-live-p buf)
           (with-current-buffer buf
             (derived-mode-p mode)))
   collect buf))

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0)
                      'next-window
                    'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg)
                    (1- arg)
                  (1+ arg))))))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun file-name-with-one-directory (file-name)
  (concat (cadr (reverse (split-string file-name "/"))) "/"
          (file-name-nondirectory file-name)))


(defvar user-home-directory (concat (expand-file-name "~") "/"))
(defun shorter-file-name (file-name)
  (s-chop-prefix user-home-directory file-name))

;;Recent open
(defun recentf--file-cons (file-name)
  (cons (shorter-file-name file-name) file-name))

(defun ido-recentf-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let* ((recent-files (mapcar 'recentf--file-cons recentf-list))
         (files (mapcar 'car recent-files))
         (file (completing-read "Choose recent file: " files)))
    (find-file (cdr (assoc file recent-files)))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  ;;(timer-cleanup-recentf)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defvar cleanup-recentf-timer nil
  "Timer for `recentf-cleanup' to reschedule itself, or nil.")
(defun timer-cleanup-recentf ()
  "Clean unecesary recent files."
  (unless cleanup-recentf-timer
    (recentf-cleanup)
    (setq cleanup-recentf-timer (run-at-time t 10800 'recentf-cleanup))))

(defun switch-to-minibuffer ()
  "Make the active minibuffer the selected window."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun todo-highlight ()
    "TODO/FIXME Highlighting"
    (font-lock-add-keywords nil
                 '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                    1 font-lock-warning-face t)))
)

;; code from https://gist.github.com/anonymous/1061884
(defun auto-byte-recompile ()
  "If the current buffer is in emacs-lisp-mode and there already exists an `.elc'
file corresponding to the current buffer file, then recompile the file."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
	   (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

;; Compiled  lips files
(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun open-buffer-delete-others(buffer-name name-register action &optional open-same-window)
  "Open buffer in fullscreen and delete other windows.
need buffer-name: name of buffer, name-register: the name to register current window
action: the action execute, optional, open-same-window"
  (if (not (equal (buffer-name) buffer-name))
      (progn
        (window-configuration-to-register name-register)
        (let ((name-buffer (get-buffer buffer-name)))
          (if name-buffer
              (if open-same-window
                  (switch-to-buffer name-buffer)
                (switch-to-buffer-other-window name-buffer)
                )
            (funcall action)))
        (delete-other-windows))))

(defun bury-buffer-restore-prev (name-register)
  "Restores the previous window configuration and burry buffer"
  (interactive)
  (bury-buffer)
  (jump-to-register name-register))

;; (defun remove-elc-on-save ()
;;   "If you're saving an elisp file, likely the .elc is no longer valid."
;;   (add-hook 'after-save-hook
;;             (lambda ()
;;               (if (file-exists-p (concat buffer-file-name "c"))
;;                   (delete-file (concat buffer-file-name "c"))))
;;             nil
;;             t))

(provide 'buffer-defuns)
