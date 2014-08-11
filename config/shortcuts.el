;;; package --- Conf shortcuts
;;;
;;; Commentary:
;;; My list of keybindings
;;;
;;; Code:

(global-set-key (kbd "C-c fd") 'find-dired)
(global-set-key (kbd "C-c d") 'diff-buffer-with-file)
(global-set-key (kbd "C-c R") 'revert-buffer)
(global-set-key (kbd "s-i") 'dired-omit-mode)

(global-set-key (kbd "C-<right>") 'next-buffer)
(global-set-key (kbd "C-<left>") 'previous-buffer)
(global-set-key (kbd "<backtab>") 'helm-buffers-list)
(global-set-key (kbd "C-<tab>") 'ibuffer-other-window)
(global-set-key (kbd "C-c b") 'list-buffers)

;;(global-set-key (kbd "M-t") 'dirtree-in-buffer)
;;(global-set-key (kbd "M-d") 'dirtree)

(global-set-key [f9] 'nav-toggle)


;; Life-hack keybindings
(global-set-key (kbd "C-c lc") 'calendar)
(global-set-key (kbd "C-c lp") 'plan)

;; Horizontal scroll
;;(global-set-key (kbd "<mouse-6>") 'scroll-right)
;;(global-set-key (kbd "<mouse-7>") 'scroll-left)

;; Errors
(global-set-key (kbd "s-<f2>") 'flycheck-list-errors)

;; Basics
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "M-v") 'browse-kill-ring)
(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "M-w") 'kill-region)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-s-r") 'isearch-backward)

(global-set-key (kbd "C-S-x") 'clipboard-kill-region) ; cut.
(global-set-key (kbd "C-c c") 'clipboard-kill-ring-save) ; copy.
(global-set-key (kbd "C-v") 'clipboard-yank) ; paste.
(global-set-key [mouse-2] 'clipboard-kill-ring-save)

(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)

(global-set-key  [f8] 'toggle-truncate-lines)
(global-set-key (kbd "C-c l") 'goto-line)
;;(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-q") 'kill-this-buffer)

;; window positioning
(global-set-key (kbd "s-?") 'enlarge-window)
(global-set-key (kbd "s-=") 'shrink-window)
(global-set-key (kbd "s-+") 'enlarge-window-horizontally)
(global-set-key (kbd "s--") 'shrink-window-horizontally)

(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
(global-set-key (kbd "<C-s-left>")   'buf-move-left)
(global-set-key (kbd "<C-s-right>")  'buf-move-right)

;; window configuration
(global-set-key (kbd "C-x j") 'jump-to-register)
(global-set-key (kbd "C-c s") 'window-configuration-to-register)

(global-set-key (kbd "C-c x") 'execute-extended-command)
(global-set-key [S-prior] 'beginning-of-buffer)
(global-set-key [S-next] 'end-of-buffer)

;; easy keys to split window. Key based on ErgoEmacs keybinding
(global-set-key (kbd "M-0") 'delete-window) ; close current pane
(global-set-key (kbd "M-1") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "M-2") 'split-window-horizontally) ; split pane left/rigth
(global-set-key (kbd "M-3") 'split-window-vertically) ; split pane top/bottom
(global-set-key (kbd "M-4") 'other-window) ; cursor to other pane
(global-set-key (kbd "M-5") 'toggle-window-split) ; toggle window position
(global-set-key (kbd "M-6") 'window-toggle-split-direction) ; toggle window position

;; Django
(global-set-key (kbd "C-x d") 'python-django-open-project)

;; Toogle Bars
(global-set-key (kbd "C-<f1>") 'tool-bar-mode)
(global-set-key (kbd "C-<f2>") 'menu-bar-mode)

;; Toogle whitespace
(global-set-key (kbd "C-<f3>") 'whitespace-mode)

;; indentation
(global-set-key (kbd "C-c C->") 'increase-left-margin)
(global-set-key (kbd "C-c C-<") 'decrease-left-margin)

;; misc
(global-set-key (kbd "C-`") 'rename-buffer)

;; zooming
(global-set-key (kbd "<M-mouse-5>") 'text-scale-increase)
(global-set-key (kbd "<M-mouse-4>") 'text-scale-decrease)

;; terminal
(global-set-key
 (kbd "C-c t")
 (lambda () (interactive) (ansi-term "/bin/bash")))
(global-set-key (kbd "C-x t") (lambda () (interactive) (popwin-term:term))
)

;; Open shell with C-z
(global-set-key (kbd "M-t") 'shell)

;; Browse URLs with C-x /
(global-set-key (kbd "C-x /") 'browse-url)

;; commenting/uncommenting
(defun my/comment-or-uncomment-current-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-x C-;") 'my/comment-or-uncomment-current-line)

;; Error jumping
(global-set-key (kbd "C-x <C-down>") 'next-error)
(global-set-key (kbd "C-x <C-up>") 'previous-error)

;; sort lines
(global-set-key (kbd "C-c C-s") 'sort-lines)

;; window opacity utilities

;; C-+ will increase opacity (== decrease transparency)
(global-set-key (kbd "C-<kp-add>")
                '(lambda()
                 (interactive)
                 (sa-opacity-modify)))

;; C-- will decrease opaccity (== increase transparency
(global-set-key (kbd "C-<kp-subtract>")
                '(lambda()
                 (interactive)
                 (sa-opacity-modify t)))

;; C-0 will returns the state to normal
(global-set-key (kbd "C-=")
                '(lambda()
                 (interactive)
                 (modify-frame-parameters nil `((alpha . 100)))))


;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-q") 'quick-switch-buffer)

(global-set-key (kbd "s-y") 'bury-buffer)

;; Revert without any fuss
;;(global-set-key (kbd "M-<escape>") (λ (revert-buffer t t)))


;; Copy file path to kill ring
(global-set-key (kbd "C-x M-w") 'copy-current-file-path)

;; PopWin
(global-set-key (kbd "C-<escape>") 'popwin:close-popup-window)
(global-set-key (kbd "s-4") 'popwin:popup-buffer)
(global-set-key (kbd "s-3") 'popwin:popup-last-buffer)
(global-set-key (kbd "s-2") 'popwin:display-buffer)
(global-set-key (kbd "s-1") 'popwin:one-window)

;; Skewer
(global-set-key (kbd "M-<f12>") 'run-skewer)
(global-set-key (kbd "M-<f11>") 'skewer-start)
(global-set-key (kbd "M-<f10>") 'skewer-demo)

;; Projects
(global-set-key (kbd "C-<f11>") 'dired-jump-other-window)
(global-set-key (kbd "C-<f10>") 'direx-project:jump-to-project-root-other-window)
(global-set-key (kbd "C-<f9>") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "C-<f8>") 'project-explorer-open)
(global-set-key (kbd "C-<f7>") 'direx:jump-to-directory)

;; Smart-forward
(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; YaSnippet
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "M-q") 'yas-expand)

;; Smartparens
(global-set-key (kbd "C-<f6>") 'sp-select-next-thing)

;; Recent Files
(global-set-key (kbd "C-x r <down>") 'ido-recentf-open)
(global-set-key (kbd "C-c C-f") 'helm-for-files)
(global-set-key (kbd "C-c r <down>") 'helm-recentf)

;; Highlight
(global-set-key (kbd "C-c C-o") 'highlight-symbol-occur)
(global-set-key (kbd "C-c M-<right>") 'highlight-symbol-next-in-defun)
(global-set-key (kbd "C-c M-<left>") 'highlight-symbol-prev-in-defun)
(global-set-key (kbd "s-<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "s-<f4>") 'highlight-symbol-prev)
(global-set-key (kbd "s-<f5>") 'highlight-symbol-next)
(global-set-key (kbd "s-<f6>") 'highlight-symbol-query-replace)
(global-set-key (kbd "s-<f7>") 'unhighlight-regexp)

;; Buffer
(global-set-key (kbd "C-c o") 'switch-to-minibuffer)
(global-set-key (kbd "<f5>") 'select-active-minibuffe)

;; Go to char
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(autoload 'magit-status "magit")

;; Helm
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-ñ") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-x y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c C-e") 'emacs-d-find-file)

;; Org
(global-set-key "\C-c \C-l" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-c \C-b" 'org-iswitchb)


(provide 'shortcuts)

;;; shortcuts.el ends here
