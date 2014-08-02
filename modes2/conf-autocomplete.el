;;; package --- Conf Auto-Complete Mode
;;;
;;; Commentary:
;;; Initial configuration for auto-complete
;;;
;;; Code:

(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)
(ac-ispell-setup)
(setq ac-candidate-limit 20
      ac-ignore-case 'Yes
      ;ac-max-width 0.5
      ac-use-menu-map t
      ac-use-fuzzy t)

;;(global-auto-complete-mode t) realy need global?
;;(add-to-list 'ac-dictionary-directories  "~/.emacs.d/ac-dict")
;;(autoload 'auto-complete-mode "auto-complete" t)
;; (ac-config-default)
;; (ac-linum-workaround)
;;                                         ;(setq ac-delay 0.2)
;;                                         ;(setq ac-auto-show-menu 0.3)
;; (setq-default ac-sources '(ac-source-yasnippet
;;                            ac-source-abbrev
;;                            ac-source-dictionary
;;                            ac-source-words-in-same-mode-buffers
;;                            ac-source-files-in-current-dir))

;;; conf-autocomplete.el ends here
