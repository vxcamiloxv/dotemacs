(provide 'conf-jedi)

;(require 'jedi)
(autoload 'jedi "jedi" t)

(add-hook 'python-mode-hook 'jedi:setup)
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional
(setq jedi:tooltip-method '(popup))
(setq jedi:tooltip-show nil)
(setq jedi:ac-setup t)  

