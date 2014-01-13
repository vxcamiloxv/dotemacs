(require 'package)
(package-initialize)
(require 'highlight-symbol)
(global-set-key (kbd "s-<f10>") 'highlight-symbol-at-point)
(global-set-key (kbd "s-<f11>") 'highlight-symbol-prev)
(global-set-key (kbd "s-<f12>") 'highlight-symbol-next)
(global-set-key (kbd "s-<f9>") 'highlight-symbol-query-replace)

(provide 'conf-highlight-symbol)
