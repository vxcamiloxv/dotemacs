;;; package --- Conf Org Mode
;;;
;;; Commentary:
;;; All Configuration for org-mode
;;;
;;; Code:

(require 'org)
;; not needed since Emacs 22.2
;;(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-agenda-files "~/Documentos/org/todo.org")
(setq org-log-done 'time)


(setq org-todo-keyword-faces
      '(("CANCELED" . (:foreground "orange" :weight bold))
        ("NEXT" . (:foreground "DeepPink2" :weight bold))
        ("DONE-NOT-ME" . (:foreground "SpringGreen4" :weight bold))
        ("PASSED-OFF" . (:foreground "peru" :weight bold))
        ("CAL-CANCEL" . (:foreground "orange" :weight bold))
        ("WAITING" . (:foreground "dodger blue" :weight bold))
        ("PAYMENT-WAITING" . (:foreground "orchid" :weight bold))
        ("PAID" . (:foreground "sea green" :weight bold))
        ))

; Priorities??

(setq org-priority-faces
      '((?A . "#f01a0f")
        (?B . "#f0640f")
        (?C . "light sea green")
        (?D . "slate blue")))

(setq org-default-priority ?C)
(setq org-lowest-priority ?D)

(provide 'conf-org)

;;; conf-org.el ends here
