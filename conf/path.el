;; Gnereal Paths

(add-to-list 'load-path "~/.emacs.d")
(let ((default-directory "~/.emacs.d/"))
  (setq load-path
      (append
        (let ((load-path (copy-sequence load-path))) ;; Shadow
          (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(provide 'path)
