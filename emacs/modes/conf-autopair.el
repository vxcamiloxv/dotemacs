(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

(add-hook 'web-mode-hook
          #'(lambda ()
              (push '(?< . ?>)
                  (getf autopair-extra-pairs :code))))

(provide 'conf-autopair)
