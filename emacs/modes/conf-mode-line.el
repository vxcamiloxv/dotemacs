;; --- Main-line only on window systems ----- (a fork of Powerline
(require 'main-line)

(when (window-system)
  (load-library "main-line")
  (setq main-line-separator-style 'wave))

(provide 'conf-mode-line)  
