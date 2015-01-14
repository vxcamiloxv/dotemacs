;;; Code:

(require 'ido)
(ido-mode t)
(ido-everywhere t)

(setq ido-max-prospects 200
      ido-enable-flex-matching t
      ido-enable-dot-prefix t
      ido-everywhere t
      ;; ido-use-virtual-buffers t
      ido-use-filename-at-point 'guess
      ido-save-directory-list-file (in-emacs-d ".cache/ido.last")
      )

(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order
      '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".js" ".conf"))

; Ignore object files
(setq ido-ignore-extensions t)

;; Maybe we can disable tramp stuff via this?
;(setq ido-work-directory-list-ignore-regexps

; Always open in the same window
(setq ido-default-buffer-method 'selected-window)

;;;;;;;;;;;;;;
;; imenu & ido
;;;;;;;;;;;;;;

; Thanks emacswiki.org!
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
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

(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                      (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                  (if (= (nth 0 ta) (nth 0 tb))
                      (> (nth 1 ta) (nth 1 tb))
                    (> (nth 0 ta) (nth 0 tb)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (if (string-equal (substring x 0 1) ".") x))
              ido-temp-list))))

;; Fix Warning
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)
(defvar ido-context-switch-command nil)

;; Use ido ubiquitous
(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)

(require 'ido-vertical-mode)
(ido-vertical-mode t)
;(setq ido-vertical-define-keys 'C-n-C-p-up-down)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)


;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

;; Hooks
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
