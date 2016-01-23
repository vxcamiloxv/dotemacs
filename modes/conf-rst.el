;;; Code:
(require 'org-table)

;; Functions
(defun distopico:orgtbl-to-rst-paddings (table)
  (let* ((pruned-table (remove 'hline table))
         (size-table (mapcar (lambda (row)
                               (mapcar #'length row))
                             pruned-table)))
    (apply #'mapcar* #'max size-table)))

(defun distopico:orgtbl-padded-hline (paddings &optional chr)
  (let ((chr (or chr ?-)))
    (concat (format "+%c" chr)
            (mapconcat (lambda (size)
                         (make-string size chr)) paddings
                         (format "%c+%c" chr chr))
            (format "%c+" chr))))

(defun distopico:orgtbl-to-rst (table params)
  "Convert the Orgtbl mode TABLE to ReST."
  (let* ((indent (make-string (or (plist-get params :indent) 0) ?\ ))
         (paddings (yh/orgtbl-to-rst-paddings table))
         (hline (concat indent (yh/orgtbl-padded-hline paddings)))
         (hlend (concat indent (yh/orgtbl-padded-hline paddings ?=)))
         (lfmt (concat indent "| "
                       (mapconcat (lambda (size)
                                    (format "%%-%ds" size)) paddings
                                    " | ") " |"))
         (hlfmt (concat lfmt "\n" hlend))
         (params2
          (list
           :tstart (concat "\n" hline) :tend (concat hline "\n") :hline hline
           :lfmt lfmt :hlfmt hlfmt :skipheadrule t)))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

(push `(rst-mode ,(concat
                   ".. BEGIN RECEIVE ORGTBL %n\n"
                   "\n"
                   ".. END RECEIVE ORGTBL %n\n"
                   "\n"
                   "..\n"
                   "    #+ORGTBL: SEND %n yh/orgtbl-to-rst :splice nil :skip 0\n"
                   "    | | |\n"))
      orgtbl-radio-table-templates)

(defun rst-validate-buffer ()
  "Tests to see if buffer is valid reStructured Text."
  (interactive)
  (if (eq major-mode 'rst-mode)         ; only runs in rst-mode
      (let ((name (buffer-name))
            (filename (buffer-file-name)))
        (cond
         ((not (file-exists-p "/usr/bin/rst2pseudoxml")) ; check that the program used to process file is present
          (error "Docutils programs not available."))
         ((not (file-exists-p filename)) ; check that the file of the buffer exists
          (error "Buffer '%s' is not visiting a file!" name))
         (t                             ; ok, process the file, producing pseudoxml output
          (let* ((pseudoxml (split-string (shell-command-to-string (concat "rst2pseudoxml " filename)) "\n"))
                 (firstline (car pseudoxml)) ; take first line of output
                 (lastline (nth (- (length pseudoxml) 2) pseudoxml))) ; take last line of output text
            (if (or (string-match "ERROR/" firstline)
                    (string-match "WARNING/" firstline)
                    ;; for reasons I don't understand, sometimes the warnings/errors are at the end of output
                    (string-match "ERROR/" lastline)
                    (string-match "WARNING/" lastline))
                (progn (ding)
                       (message "There's an problem in this buffer."))
              (message "Buffer is valid reStructured Text."))))))))

;; Hooks
;; (add-hook 'rst-mode-hook
;;           (lambda()
;;             (add-hook 'after-save-hook 'rst-validate-buffer)
;;             (turn-on-orgtbl)))

(add-hook 'rst-adjust-hook 'rst-toc-update)

(provide 'conf-rst)
