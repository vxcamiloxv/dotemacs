;;; setup-keybindings.el --- Global keybindings.
;;
;;; Commentary:
;;  List of global keybindings by major-mode.

(require 'region-bindings-mode)

;;; Code:

;; Some bindings in active regions
(region-bindings-mode-enable)

;; ---------
;; Generic keybindings
;; ---------

;; Basic
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c c p") 'copy-current-file-path)
(global-set-key (kbd "C-c c e") 'copy-to-end-of-line)
(global-set-key (kbd "C-c c w") 'copy-whole-lines)
(global-set-key (kbd "<C-S-SPC>") 'cua-rectangle-mark-mode)

;; Command with default hard keys such ^
(global-set-key (kbd "M--") 'delete-indentation)

;; File manager
(global-set-key [f7] 'dired-jump-other-window)
(global-set-key [f8] 'distopico:neotree-toggle)
(global-set-key (kbd "M-<f8>") 'neotree-refresh)

;; Navigation
(global-set-key (kbd "M-s-n") 'distopico:next-link)
(global-set-key (kbd "M-s-p") 'distopico:previous-link)
(global-set-key (kbd "M-s-<return>") 'distopico:show-link-url)

;; Buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c k") 'kill-this-buffer-and-pane)
(global-set-key (kbd "C-q") 'bury-buffer)
(global-set-key (kbd "s-k") 'kill-buffer)
(global-set-key (kbd "s-b") 'bury-buffer)
(global-set-key (kbd "<C-tab>") 'switch-to-previous-buffer)
(global-set-key [C-M-next] 'next-buffer)
(global-set-key [C-M-prior] 'previous-buffer)
(global-set-key (kbd "C-c b b") 'ibuffer-other-window)
(global-set-key (kbd "C-c b d") 'rename-buffer-with-directory)
(global-set-key (kbd "C-c b m") 'switch-to-minibuffer)
(global-set-key (kbd "C-c b r") 'rename-buffer)
(global-set-key (kbd "C-c b f") 'distopico:rename-current-buffer-file)
(global-set-key (kbd "C-c b k") 'distopico:delete-current-buffer-file)
;; Revert without any fuss
(global-set-key (kbd "C-c b c") (λ (revert-buffer t)))

;; Window
(global-set-key (kbd "C-c w t") 'distopico:toggle-window-split)
(global-set-key (kbd "C-c w r") 'distopico:rotate-windows)
(global-set-key (kbd "C-c w s") 'windresize)
(global-set-key (kbd "C-c w z") 'zoom-window-zoom)
(global-set-key (kbd "C-x 9") 'delete-windows-on)
(global-set-key (kbd "M-0") 'ace-delete-window)
(global-set-key (kbd "M-1") 'ace-delete-other-windows)
(global-set-key (kbd "M-2") 'ace-window)
(global-set-key (kbd "M-3") 'window-toggle-split-direction) ; toggle window position
(global-set-key (kbd "M-4") 'windresize)

;; Reposition window
(global-set-key (kbd "<M-s-home>") 'buf-move-up)
(global-set-key (kbd "<M-s-end>") 'buf-move-down)
(global-set-key (kbd "<M-s-next>") 'buf-move-left)
(global-set-key (kbd "<M-s-prior>") 'buf-move-right)
;; Move bewteen windows
(global-set-key (kbd "C-x M-<left>") (ignore-error-wrapper 'windmove-left))
(global-set-key (kbd "C-x M-<right>") (ignore-error-wrapper 'windmove-right))
(global-set-key (kbd "C-x M-<up>") (ignore-error-wrapper 'windmove-up))
(global-set-key (kbd "C-x M-<down>") (ignore-error-wrapper 'windmove-down))
(global-set-key (kbd "C-c w <left>") (ignore-error-wrapper 'windmove-left))
(global-set-key (kbd "C-c w <right>") (ignore-error-wrapper 'windmove-right))
(global-set-key (kbd "C-c w <up>") (ignore-error-wrapper 'windmove-up))
(global-set-key (kbd "C-c w <down>") (ignore-error-wrapper 'windmove-down))

;; commenting/uncommenting
;; (global-set-key (kbd "M-;") 'comment-or-uncomment-current-line)
(global-set-key (kbd "M-;") 'comment-dwim-line)

;; Tabbar
(global-set-key (kbd "C-x <right>") 'tabbar-forward-tab)
(global-set-key (kbd "C-x <left>") 'tabbar-backward-tab)
(global-set-key (kbd "C-x <up>") 'tabbar-forward-group)
(global-set-key (kbd "C-x <down>") 'tabbar-backward-group)

;; Zooming
(global-set-key (kbd "<M-mouse-5>") 'text-scale-increase)
(global-set-key (kbd "<M-mouse-4>") 'text-scale-decrease)
(global-set-key (kbd "<C-M-mouse-1>") 'text-scale-adjust)
(global-set-key (kbd "<C-M-mouse-3>") (λ (text-scale-decrease 0)))

;; PopWin
(global-set-key (kbd "C-<escape>") 'popwin:close-popup-window)
(global-set-key (kbd "s-<escape>") 'popwin:popup-last-buffer)

;; Query replace to anzu-replace
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key (kbd "C-%") 'anzu-query-replace-at-cursor)

;; Isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(define-key isearch-mode-map [next] 'isearch-repeat-forward)
(define-key isearch-mode-map [prior] 'isearch-repeat-backward)

;; Toogle whitespace
(global-set-key (kbd "C-x w") 'whitespace-mode)

;; Misc
(global-set-key (kbd "C-c _") 'goto-last-change)
(global-set-key (kbd "C-c u") 'distopico:pg-uline)

;; Terminal/Shell
(global-set-key (kbd "C-c t a") 'popwin-term:ansi-term)
(global-set-key (kbd "C-c t m") 'popwin-term:multi-term)
(global-set-key (kbd "C-c t t") 'popwin-term:term)
(global-set-key (kbd "C-c t s") 'shell)
(global-set-key (kbd "C-c t e") 'eshell)

;; Toogle Bars
(global-set-key (kbd "C-<f1>") 'tool-bar-mode)
(global-set-key (kbd "C-<f2>") 'menu-bar-mode)

;; Perform general cleanup
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'distopico:cleanup-buffer)
(global-set-key (kbd "C-c C-w") 'delete-trailing-whitespace)

;; Ido
(global-set-key (kbd "C-x r <down>") 'ido-recentf-open)
(global-set-key (kbd "C-c i r") 'ido-recentf-open)
(global-set-key (kbd "C-c i s") 'ido-goto-symbol)
(global-set-key (kbd "C-c i b") 'ido-switch-buffer)
(global-set-key (kbd "C-c i w") 'ido-select-window)
(global-set-key (kbd "C-c i o") 'ido-occur)
(global-set-key (kbd "C-x B") 'ido-for-this-mode)
(global-set-key (kbd "C-x o") 'ido-select-window)

;; Imenu
(global-set-key (kbd "C-x c") 'imenu)
(global-set-key (kbd "C-c m") 'helm-imenu)
(global-set-key [S-mouse-3] 'imenu)

;; Avy
(global-set-key (kbd "C-: c") 'avy-goto-char)
(global-set-key (kbd "C-: v") 'avy-goto-char-2)
(global-set-key (kbd "C-: l") 'avy-goto-line)
(global-set-key (kbd "C-: w") 'avy-goto-word-1)
(global-set-key (kbd "C-: e") 'avy-goto-word-0)
(global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)
(global-set-key (kbd "M-Z") 'avy-zap-up-to-char-dwim)

;; Move-dup
;; default M-<up> md-move-lines-up / M-<down> md-move-lines-down
(global-set-key (kbd "M-s-<up>") 'md-duplicate-up)
(global-set-key (kbd "M-s-<down>") 'md-duplicate-down)

;; smart-forward
(global-set-key (kbd "C-s-<up>") 'smart-up)
(global-set-key (kbd "C-s-<down>") 'smart-down)
(global-set-key (kbd "C-s-<left>") 'smart-backward)
(global-set-key (kbd "C-s-<right>") 'smart-forward)

;; Change inner/outer
(global-set-key (kbd "s--") 'change-inner)
(global-set-key (kbd "s-.") 'change-outer)

;; Actions
(global-set-key (kbd "C-x <next>") 'select-current-line)
(global-set-key (kbd "C-c e <right>") 'select-current-line)

;; HideShow
(global-set-key (kbd "C-x |") 'toggle-hiding)

;; Magit
(global-set-key (kbd "C-x g s") 'magit-status)

;; Mail
(global-set-key (kbd "C-x M-m") 'distopico:mu4e-open)

;; News
(global-set-key (kbd "C-x M-n") 'distopico:elfeed-open)

;; Pomodoro
(global-set-key (kbd "C-x M-p") 'pomodoro-action)

;; Erc
(global-set-key (kbd "C-x M-e") 'distopico:erc-start-ask-or-switch)

;; GnuSocial
(global-set-key (kbd "C-x M-g") 'distopico:gnusocial-open)
(define-key global-map (kbd "C-c g")
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'gnusocial-update-status-interactive)
    (define-key map "a" 'gnu-social-direct-message-interactive)
    (define-key map "o" 'distopico:gnusocial-open)
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
    (define-key map "f" 'org-iswitchb)
    (define-key map "a" 'distopico:org-show-agenda)
    (define-key map "v" 'org-agenda)
    (define-key map "A" 'org-agenda-list)
    (define-key map "c" 'org-capture)
    (define-key map "C" 'org-clock-goto)
    (define-key map "p" 'pomodoro-org)
    (define-key map "u" 'distopico:org-update-appt)
    (define-key map "P" 'org-projectile-project-todo-completing-read)
    (define-key map "r" 'org-refile)
    (define-key map "R" 'distopico:org-capture-refile-and-jump)
    (define-key map "n" 'org-annotate-file)
    (define-key map "l" 'org-store-link)
    (define-key map "L" 'org-insert-link-global)
    (define-key map "o" 'org-open-at-point-global)
    (define-key map "jp" 'org-jira-get-projects)
    (define-key map "ji" 'org-jira-get-issues)
    (define-key map "jh" 'org-jira-get-issues-headonly)
    (define-key map "jc" 'org-jira-create-issue)
    (define-key map "jj" 'org-jira-todo-to-jira)
    map))

(provide 'setup-keybindings)
;;; setup-keybindings.el ends here
