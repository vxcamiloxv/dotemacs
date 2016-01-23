;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "setup-package" "setup-package.el" (22177 43936
;;;;;;  639938 156000))
;;; Generated autoloads from setup-package.el

(autoload 'distopico:ensure-required-packages "setup-package" "\
Check if dependencies are installer and update for new packages (package versions)

\(fn)" t nil)

(autoload 'distopico:check-required-packages "setup-package" "\
Check if `distopico-packages' is up to date.

\(fn)" t nil)

(autoload 'distopico:load-require-libs "setup-package" "\


\(fn)" nil nil)

(autoload 'distopico:package-init-load-hook "setup-package" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "setup-path" "setup-path.el" (22177 44947 499930
;;;;;;  699000))
;;; Generated autoloads from setup-path.el

(autoload 'distopico:startup-byte-recompile "setup-path" "\
Compile all packages in `distopico:load-path'

\(fn)" t nil)

(autoload 'distopico:startup-load-path "setup-path" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("colors.el" "my-package.el" "setup-color.el"
;;;;;;  "setup-elget.el" "setup-general.el" "setup-gui.el" "setup-keybindings.el"
;;;;;;  "setup-theme.el") (22177 45022 694121 2000))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
