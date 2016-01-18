;;; Code:
;; ---------
;; Generic keybindings
;; ---------

(global-set-key (kbd "C-c g d") 'find-dired)
(global-set-key (kbd "C-c d") 'diff-buffer-with-file)
(global-set-key (kbd "C-c R") 'revert-buffer)
(global-set-key (kbd "s-i") 'dired-omit-mode)

(global-set-key [C-M-next] 'next-buffer)
(global-set-key [C-M-prior] 'previous-buffer)
(global-set-key (kbd "C-c b") 'ibuffer-other-window)
(global-set-key (kbd "M-s-t") 'toggle-truncate-lines)

(global-set-key [f9] 'distopico:nav-toggle)
(global-set-key [f8] 'distopico:neotree-toggle)

(global-set-key (kbd "C-c l c") 'calendar)

(global-set-key (kbd "C-x M-t") 'toggle-cli-theme)
;; Horizontal scrollb
;; (global-set-key (kbd "<mouse-6>") 'scroll-right)
;; (global-set-key (kbd "<mouse-7>") 'scroll-left)

;; Errors
(global-set-key (kbd "s-<f1>") 'flycheck-list-errors)

;; Basicas
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "M-v") 'browse-kill-ring)
(global-set-key (kbd "M-w") 'kill-ring-save) ;; or clipboard-kill-ring-save?
(global-set-key (kbd "C-w") 'kill-region) ;; or clipboard-kill-region?
(global-set-key (kbd "C-v") 'clipboard-yank)
(global-set-key [mouse-2] 'clipboard-kill-ring-save)

(global-set-key (kbd "C-c v") 'copy-to-end-of-line)
(global-set-key (kbd "C-c C") 'copy-whole-lines)
(global-set-key (kbd "C-c c") 'copy-line)

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-s-f") 'isearch-backward)
(global-set-key (kbd "C-c f o") 'isearch-occur)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-s-z") 'undo-tree-redo)

(global-set-key (kbd "C-c g l") 'goto-line)
(global-set-key (kbd "C-q") 'kill-this-buffer)

;; Copy file path to kill ring
(global-set-key (kbd "C-x M-w") 'copy-current-file-path)

;; Query replace to anzu-replace
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key (kbd "M-s-%") 'anzu-query-replace-at-cursor)

;; Actions
(global-set-key (kbd "C-c a <right>") 'select-current-line)
(global-set-key (kbd "C-c a r") 'er/expand-region)
(global-set-key (kbd "C-c a w") 'er/mark-word)
(global-set-key (kbd "C-c a c") 'er/mark-comment)

;; Avy
(global-set-key (kbd "C-: c") 'avy-goto-char)
(global-set-key (kbd "C-: v") 'avy-goto-char-2)
(global-set-key (kbd "C-: l") 'avy-goto-line)
(global-set-key (kbd "C-: w") 'avy-goto-word-1)
(global-set-key (kbd "C-: e") 'avy-goto-word-0)
(global-set-key (kbd "M-z") 'avy-zap-to-char)
(global-set-key (kbd "M-Z") 'avy-zap-up-char)

;; Cleanup buffer.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; window positioning
(global-set-key (kbd "s-?") 'enlarge-window)
(global-set-key (kbd "s-=") 'shrink-window)
(global-set-key (kbd "s-+") 'enlarge-window-horizontally)
(global-set-key (kbd "s--") 'shrink-window-horizontally)

(global-set-key (kbd "<C-s-up>") 'buf-move-up)
(global-set-key (kbd "<C-s-down>") 'buf-move-down)
(global-set-key (kbd "<C-s-left>") 'buf-move-left)
(global-set-key (kbd "<C-s-right>") 'buf-move-right)

;; window configuration
(global-set-key (kbd "C-x j") 'jump-to-register)
(global-set-key (kbd "C-c s") 'window-configuration-to-register)

(global-set-key (kbd "C-c x") 'execute-extended-command)
(global-set-key [S-prior] 'beginning-of-buffer)
(global-set-key [S-next] 'end-of-buffer)

;; easy keys to split window. Key based on ErgoEmacs keybinding
(global-set-key (kbd "M-1") 'ace-window)
(global-set-key (kbd "M-2") 'split-window-horizontally) ; split pane left/rigth
(global-set-key (kbd "M-3") 'split-window-vertically) ; split pane top/bottom
(global-set-key (kbd "M-4") 'other-window) ; cursor to other pane
(global-set-key (kbd "M-5") 'toggle-window-split) ; toggle window position
(global-set-key (kbd "M-6") 'window-toggle-split-direction) ; toggle window position
(global-set-key (kbd "M-9") 'delete-window) ; close current pane
(global-set-key (kbd "M-0") 'delete-other-windows) ; expand current pane

;; zooming
(global-set-key (kbd "<M-mouse-5>") 'text-scale-increase)
(global-set-key (kbd "<M-mouse-4>") 'text-scale-decrease)
(global-set-key (kbd "<C-M-mouse-1>") 'text-scale-adjust)

;; Tabbar
(global-set-key (kbd "C-1") (lambda () (interactive) (switch-tabbar 1)))
(global-set-key (kbd "C-2") (lambda () (interactive) (switch-tabbar 2)))
(global-set-key (kbd "C-3") (lambda () (interactive) (switch-tabbar 3)))
(global-set-key (kbd "C-4") (lambda () (interactive) (switch-tabbar 4)))
(global-set-key (kbd "C-5") (lambda () (interactive) (switch-tabbar 5)))
(global-set-key (kbd "C-6") (lambda () (interactive) (switch-tabbar 6)))
(global-set-key (kbd "C-7") (lambda () (interactive) (switch-tabbar 7)))
(global-set-key (kbd "C-8") (lambda () (interactive) (switch-tabbar 8)))
(global-set-key (kbd "C-9") (lambda () (interactive) (switch-tabbar 9)))
(global-set-key (kbd "C-0") (lambda () (interactive) (switch-tabbar -1)))

(global-set-key (kbd "C-<right>") 'tabbar-forward-tab)
(global-set-key (kbd "C-<left>") 'tabbar-backward-tab)
(global-set-key (kbd "C-x <right>") 'tabbar-forward-group)
(global-set-key (kbd "C-x <left>") 'tabbar-backward-group)

(global-set-key (kbd "C-M-g") 'toggle-tabbar-mode)
(global-set-key (kbd "C-c M-t d") 'tabbar-switch-to-grouping-by-dir)
(global-set-key (kbd "C-c M-t g") 'tabbar-switch-to-default-grouping)

;; Django
(global-set-key (kbd "C-x p d") 'python-django-open-project)

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

;; terminal
(global-set-key (kbd "C-c t") 'popwin-term:ansi-term)
(global-set-key (kbd "C-x t") 'popwin-term:multi-term)
;;(global-set-key (kbd "C-x t")
;;    (lambda () (interactive) (popwin-term:term)))


;; Shell
(global-set-key (kbd "M-s s") 'shell)
(global-set-key (kbd "M-s e") 'eshell)

;; Browse URLs with C-x /
(global-set-key (kbd "C-x /") 'browse-url)

;; commenting/uncommenting
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


;; Toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-q") 'quick-switch-buffer)

(global-set-key (kbd "s-y") 'bury-buffer)

;; Revert without any fuss
;;(global-set-key (kbd "M-<escape>") (λ (revert-buffer t t)))

;; Emcas refactor
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)

;; PopWin
(global-set-key (kbd "C-<escape>") 'popwin:close-popup-window)
(global-set-key (kbd "<f12>") 'popwin:close-popup-window)
(global-set-key (kbd "C-<f12>") 'popwin:popup-last-buffer)

;;Skewer
(global-set-key (kbd "M-<f12>") 'run-skewer)
(global-set-key (kbd "M-<f11>") 'skewer-start)
(global-set-key (kbd "M-<f10>") 'skewer-demo)

;; Projects
(global-set-key (kbd "C-<f11>") 'dired-jump-other-window)
(global-set-key (kbd "C-<f10>") 'direx-project:jump-to-project-root-other-window)
(global-set-key (kbd "C-<f9>") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "C-<f8>") 'project-explorer-open)
(global-set-key (kbd "C-<f7>") 'direx:jump-to-directory)

;; smart-forward
(global-set-key (kbd "C-x M-<up>") 'smart-up)
(global-set-key (kbd "C-x M-<down>") 'smart-down)
(global-set-key (kbd "C-x M-<left>") 'smart-backward)
(global-set-key (kbd "C-x M-<right>") 'smart-forward)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-b") 'delete-blank-lines)

;;YaSnippet
;;(define-key yas-minor-mode-map (kbd "<tab>") nil)
;;(define-key yas-minor-mode-map (kbd "TAB") nil)
;;(define-key yas-minor-mode-map (kbd "M-q") 'yas-expand)

;; Smartparens
(global-set-key (kbd "C-<f6>") 'sp-select-next-thing)

;; Recent Files
(global-set-key (kbd "C-x r <down>") 'ido-recentf-open)
(global-set-key (kbd "C-c h f") 'helm-for-files)
(global-set-key (kbd "C-c h r") 'helm-recentf)

;; Helm
(global-set-key (kbd "<backtab>") 'helm-buffers-list)
(global-set-key (kbd "C-c h b") 'helm-buffers-list)
(global-set-key (kbd "C-c h m") 'helm-imenu)
(global-set-key (kbd "C-c h i") 'helm-mini)
(global-set-key (kbd "C-c h I") 'helm-swoop-from-isearch)
(global-set-key (kbd "C-c h s") 'helm-swoop)
(global-set-key (kbd "C-c h S") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c h k") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h x") 'helm-M-x)
(global-set-key (kbd "C-c h p") 'helm-projectile)
(global-set-key (kbd "C-c h y") 'helm-show-kill-ring)

;; Ido
(global-set-key (kbd "C-c C-i r") 'ido-recentf-open)
(global-set-key (kbd "C-c C-i s") 'ido-goto-symbol)
(global-set-key (kbd "C-c C-i b") 'ido-switch-buffer)

;; Imenu
(global-set-key (kbd "C-c m") 'imenu)
(global-set-key (kbd "C-c i") 'ido-imenu)

;; Highlight
(global-set-key (kbd "C-c C-o") 'highlight-symbol-occur)
(global-set-key (kbd "C-c M-<right>") 'highlight-symbol-next-in-defun)
(global-set-key (kbd "C-c M-<left>") 'highlight-symbol-prev-in-defun)
(global-set-key (kbd "s-<f2>") 'auto-highlight-symbol-mode)
(global-set-key (kbd "s-<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "s-<f4>") 'highlight-symbol-prev)
(global-set-key (kbd "s-<f5>") 'highlight-symbol-next)
(global-set-key (kbd "s-<f6>") 'highlight-symbol-query-replace)
(global-set-key (kbd "s-<f7>") 'unhighlight-regexp)

;; Move-dup
(global-set-key (kbd "M-s-<up>") 'md/move-lines-up)
(global-set-key (kbd "M-s-<down>") 'md/move-lines-down)
(global-set-key (kbd "M-s-<left>") 'md/duplicate-up)
(global-set-key (kbd "M-s-<right>") 'md/duplicate-down)

;; Buffer
(global-set-key (kbd "C-x o") 'switch-to-minibuffer)
(global-set-key (kbd "<f5>") 'select-active-minibuffe)

;; Go to char
(global-set-key (kbd "C-c g g") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(autoload 'magit-status "magit")

;; Mail
(global-set-key (kbd "C-x M-m") 'distopico:mu4e-open)

;; News
(global-set-key (kbd "C-x M-n") 'distopico:elfeed-open)

;; Pomodoro
(global-set-key (kbd "M-p") 'pomodoro-action)

;; Erc
(global-set-key (kbd "C-x M-e") 'distopico:erc-start-ask-or-switch)

;; GnuSocial
(global-set-key (kbd "C-x M-g") 'distopico:gnusocial-open)
(define-key global-map (kbd "C-c g")
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'gnusocial-update-status-interactive)
    (define-key map "a" 'gnu-social-direct-message-interactive)
    map))

;; Jabebr
(global-set-key (kbd "C-x M-j") 'distopico:jabber-display-roster)
(define-key global-map (kbd "C-c j")
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'jabber-connect-all)
    (define-key map "d" 'jabber-disconnect)
    (define-key map "r" 'distopico:jabber-display-roster)
    (define-key map "j" 'jabber-chat-with)
    (define-key map "l" 'jabber-activity-switch-to)
    (define-key map "a" 'jabber-send-away-presence)
    (define-key map "o" 'jabber-send-default-presence)
    (define-key map "x" 'jabber-send-xa-presence)
    (define-key map "p" 'jabber-send-presence)
    map))

;; Org
(define-key global-map (kbd "C-c o")
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'find-org-file)
    (define-key map "a" 'distopico:org-show-agenda)
    (define-key map "v" 'org-agenda)
    (define-key map "A" 'org-agenda-list)
    (define-key map "c" 'org-capture)
    (define-key map "C" 'org-clock-goto)
    (define-key map "p" 'pomodoro-org)
    (define-key map "P" 'org-projectile:project-todo-completing-read)
    (define-key map "r" 'org-refile)
    (define-key map "R" 'distopico:org-capture-refile-and-jump)
    (define-key map "n" 'org-annotate-file)
    (define-key map "d" 'distopico:open-diary)
    (define-key map "l" 'org-store-link)
    (define-key map "L" 'org-insert-link-global)
    (define-key map "o" 'org-open-at-point-global)
    (define-key map "b" 'distopico:org-tree-to-indirect-buffer-renamed)
    (define-key map "q" 'bury-buffer)
    map))


(provide 'shortcuts)
