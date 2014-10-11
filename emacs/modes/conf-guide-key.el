(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x 4" "C-x 5"))
(setq guide-key/highlight-command-regexp "rectangle\\|register")

(guide-key-mode 1)  ; Enable guide-key-mode

(provide 'conf-guide-key)
