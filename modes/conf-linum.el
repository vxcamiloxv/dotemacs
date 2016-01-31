;;; Code:
(require 'linum)
(require 'hlinum)
(require 'linum-off)

;; highlight current line number
(hlinum-activate)

(global-linum-mode 1)   ; Show line numbers everywhere
(column-number-mode 1)
(line-number-mode 1)

(setq linum-eager nil)  ; Better linum performance

(provide 'conf-linum)
