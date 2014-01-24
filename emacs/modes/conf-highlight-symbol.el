(require 'package)
(package-initialize)
(require 'highlight-symbol)
(global-set-key (kbd "s-<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "s-<f4>") 'highlight-symbol-prev)
(global-set-key (kbd "s-<f5>") 'highlight-symbol-next)
(global-set-key (kbd "s-<f6>") 'highlight-symbol-query-replace)

(provide 'conf-highlight-symbol)
