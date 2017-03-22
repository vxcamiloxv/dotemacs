
;;; Code:

(require 'ido)
(require 'ido-sort-mtime)
(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
(require 'ido-select-window)
(require 'ido-at-point)
;;(require 'ido-exit-target)
(require 'ido-complete-space-or-hyphen)
;; (require 'crm-custom)

(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
(ido-vertical-mode t)
(ido-sort-mtime-mode t)
(ido-at-point-mode t)
;; (crm-custom-mode)

(setq ido-default-buffer-method 'selected-window ; Always open in the same window
      ido-create-new-buffer 'always
      ido-max-prospects 200
      ido-enable-flex-matching t
      ido-enable-dot-prefix t
      ido-everywhere t
      ido-ignore-extensions t ; Ignore object files
      ;; ido-use-virtual-buffers t
      ido-use-filename-at-point 'guess
      ido-vertical-define-keys 'C-n-C-p-up-down-left-right ;C-n-C-p-up-down
      ;;ido-exit-target-keymap-prefix (kbd "M-RET")
      ido-save-directory-list-file (in-emacs-d ".cache/ido.last")
      ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".js" ".conf") )

;; Functions
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido optional `SYMBOL-LIST'."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(defun ido-for-mode(prompt the-mode)
  (switch-to-buffer
   (ido-completing-read
    prompt
    (save-excursion
      (delq nil
            (mapcar (lambda (buf)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (and (eq major-mode the-mode)
                               (buffer-name buf)))))
                    (buffer-list)))))))

(defun ido-for-this-mode ()
  "Show ido by mode."
  (interactive)
  (let ((the-mode major-mode))
    (switch-to-buffer
     (ido-completing-read
      (format "Buffers of %s: " the-mode)
      (save-excursion
        (delq nil
              (mapcar (lambda (buf)
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (and (eq major-mode the-mode)
                                 (buffer-name buf)))))
                      (buffer-list))))))))


;; (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
;;   "Fix ido-ubiquitous for newer packages."
;;   `(eval-after-load ,package
;;      '(defadvice ,cmd (around ido-ubiquitous-new activate)
;;         (let ((ido-ubiquitous-enable-compatibility nil))
;;           ad-do-it))))

;; (ido-ubiquitous-use-new-completing-read webjump 'webjump)
;; (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
;; (ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

(provide 'conf-ido)
