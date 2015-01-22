;;; Code:

(require 'smex)
(setq smex-save-file (in-emacs-d ".cache/smex-items"))
(smex-initialize)
(global-set-key (kbd "s-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


(provide 'conf-smex)
