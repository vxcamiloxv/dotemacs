;;; Code:
(autoload 'org-present "org-present" nil t)
(autoload 'hide-mode-line "hide-mode-line" nil t)

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 ;;(org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)
                 (hide-mode-line)
                 (toggle-frame-fullscreen)
                 (tabbar-local-mode 1)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 ;;(org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)
                 (hide-mode-line)
                 (toggle-frame-fullscreen)
                 (tabbar-local-mode -1)))))

(provide 'conf-present)
