;;; Code:
(require 'region-bindings-mode)

;; Some bindings in active regions
(region-bindings-mode-enable)
;; ---------
;; Generic keybindings
;; ---------

;; (global-set-key (kbd "C-c g d") 'find-dired)
;; (global-set-key (kbd "C-c d") 'diff-buffer-with-file)
;; (global-set-key (kbd "C-c R") 'revert-buffer)
;; (global-set-key (kbd "s-i") 'dired-omit-mode)

;; (global-set-key (kbd "C-c l c") 'calendar)

;; ;; Horizontal scrollb
;; ;; (global-set-key (kbd "<mouse-6>") 'scroll-right)
;; ;; (global-set-key (kbd "<mouse-7>") 'scroll-left)

;; ;; Basics
;; (global-set-key (kbd "M-v") 'browse-kill-ring)
;; (global-set-key [mouse-2] 'clipboard-kill-ring-save)
;; (global-set-key [mouse-2] 'mouse-yank-at-click)

;; (global-set-key [next] 'gcm-scroll-down)
;; (global-set-key [prior]  'gcm-scroll-up)

;; ;; Djang
;; (global-set-key (kbd "C-x p d") 'python-django-open-project)

;; ;; Indentation
;; (global-set-key (kbd "C-c C->") 'increase-left-margin)
;; (global-set-key (kbd "C-c C-<") 'decrease-left-margin)

;; ;; Browse URLs with C-x /
;; (global-set-key (kbd "C-x /") 'browse-url)

;; sort lines
;; (global-set-key (kbd "C-c C-s") 'sort-lines)

;; ;; window opacity utilities

;; ;; C-+ will increase opacity (== decrease transparency)
;; (global-set-key (kbd "C-<kp-add>")
;;                 '(lambda()
;;                    (interactive)
;;                    (sa-opacity-modify)))

;; ;; C-- will decrease opaccity (== increase transparency
;; (global-set-key (kbd "C-<kp-subtract>")
;;                 '(lambda()
;;                    (interactive)
;;                    (sa-opacity-modify t)))

;; ;; C-0 will returns the state to normal
;; (global-set-key (kbd "C-=")
;;                 '(lambda()
;;                    (interactive)
;;                    (modify-frame-parameters nil `((alpha . 100)))))


;; ;;Skewer
;; (global-set-key (kbd "M-<f12>") 'run-skewer)
;; (global-set-key (kbd "M-<f11>") 'skewer-start)
;; (global-set-key (kbd "M-<f10>") 'skewer-demo)

;; ;;YaSnippet
;; ;;(define-key yas-minor-mode-map (kbd "<tab>") nil)
;; ;;(define-key yas-minor-mode-map (kbd "TAB") nil)
;; ;;(define-key yas-minor-mode-map (kbd "M-q") 'yas-expand)
;; ;;(define-key yas-minor-mode-map (kbd "M-ñ") 'yas-ido-expand)

;; ;; Hippie
;; (define-key global-map (kbd "M-#") 'hippie-expand)

;; ;; Smex
;; (global-set-key (kbd "s-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ;; Highlight
;; (global-set-key (kbd "C-c C-o") 'highlight-symbol-occur)
;; (global-set-key (kbd "C-c M-<right>") 'highlight-symbol-next-in-defun)
;; (global-set-key (kbd "C-c M-<left>") 'highlight-symbol-prev-in-defun)
;; (global-set-key (kbd "s-<f1>") 'distopico:highlight-symbol-toggle)
;; (global-set-key (kbd "s-<f2>") 'highlight-symbol-at-point)
;; (global-set-key (kbd "s-<f3>") 'highlight-symbol-prev)
;; (global-set-key (kbd "s-<f4>") 'highlight-symbol-next)
;; (global-set-key (kbd "s-<f5>") 'highlight-symbol-query-replace)
;; (global-set-key (kbd "s-<f6>") 'unhighlight-regexp)
;; (global-set-key (kbd "s-<f7>") 'highlight-indentation-current-column-mode)
;; (global-set-key (kbd "s-<f8>") 'highlight-indentation-mode)

;; Basic
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c c p") 'copy-current-file-path)
(global-set-key (kbd "C-c c e") 'copy-to-end-of-line)
(global-set-key (kbd "C-c c w") 'copy-whole-lines)

;; Nav tree
(global-set-key [f9] 'distopico:nav-toggle)
(global-set-key [f8] 'distopico:neotree-toggle)

;; Buffer
(global-set-key (kbd "C-q") 'kill-this-buffer)
(global-set-key (kbd "M-q") 'kill-this-buffer-and-pane)
(global-set-key (kbd "s-q") 'bury-buffer)
(global-set-key (kbd "<C-tab>") 'quick-switch-buffer)
(global-set-key [C-M-next] 'next-buffer)
(global-set-key [C-M-prior] 'previous-buffer)
(global-set-key (kbd "C-c b b") 'ibuffer-other-window)
(global-set-key (kbd "C-c b r") 'rename-buffer)
(global-set-key (kbd "C-c b m") 'switch-to-minibuffer)
(global-set-key (kbd "C-c b f") 'distopico:rename-current-buffer-file)
(global-set-key (kbd "C-c b d") 'rename-buffer-with-directory)
;; Revert without any fuss
(global-set-key (kbd "s-w") (λ (revert-buffer t t)))

;; Window
(global-set-key (kbd "s-}") 'enlarge-window)
(global-set-key (kbd "s-{") 'shrink-window)
(global-set-key (kbd "C-c w t") 'distopico:toggle-window-split)
(global-set-key (kbd "C-c w r") 'distopico:rotate-windows)
(global-set-key (kbd "M-1") 'ace-window)
(global-set-key (kbd "M-2") 'toggle-window-split) ; toggle window position
(global-set-key (kbd "M-3") 'window-toggle-split-direction) ; toggle window position
;; Reposition window
(global-set-key (kbd "<M-s-home>") 'buf-move-up)
(global-set-key (kbd "<M-s-end>") 'buf-move-down)
(global-set-key (kbd "<M-s-next>") 'buf-move-left)
(global-set-key (kbd "<M-s-prior>") 'buf-move-right)
;; Move bewteen windows
(global-set-key (kbd "M-<left>") (ignore-error-wrapper 'windmove-left))
(global-set-key (kbd "M-<right>") (ignore-error-wrapper 'windmove-right))
(global-set-key (kbd "M-<up>") (ignore-error-wrapper 'windmove-up))
(global-set-key (kbd "M-<down>") (ignore-error-wrapper 'windmove-down))

;; commenting/uncommenting
(global-set-key (kbd "C-;") 'comment-or-uncomment-current-line)
(global-set-key (kbd "M-;") 'comment-dwim-line)

;; Tabbar
(global-set-key (kbd "<C-s-next>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-s-prior>") 'tabbar-backward-tab)
(global-set-key (kbd "<C-s-home>") 'tabbar-forward-group)
(global-set-key (kbd "<C-s-backspace>") 'tabbar-backward-group)

;; Zooming
(global-set-key (kbd "<M-mouse-5>") 'text-scale-increase)
(global-set-key (kbd "<M-mouse-4>") 'text-scale-decrease)
(global-set-key (kbd "<C-M-mouse-1>") 'text-scale-adjust)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-*") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-adjust)

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
(define-key isearch-mode-map (kbd "M-o") 'distopico:isearch-process-search-char)

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

;; Projects
(global-set-key (kbd "C-<f7>") 'direx:jump-to-directory)
(global-set-key (kbd "C-<f8>") 'project-explorer-open)
(global-set-key (kbd "C-<f9>") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "C-<f10>") 'direx-project:jump-to-project-root-other-window)
(global-set-key (kbd "C-<f11>") 'dired-jump-other-window)

;; Perform general cleanup
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'distopico:cleanup-buffer)
(global-set-key (kbd "C-c C-b") 'delete-blank-lines)

;; Recent Files
(global-set-key (kbd "C-x r <down>") 'ido-recentf-open)
(global-set-key (kbd "C-c h r") 'helm-recentf)

;; Helm
(global-set-key (kbd "<backtab>") 'helm-buffers-list)
(global-set-key (kbd "C-c h f") 'helm-for-files)
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
(global-set-key (kbd "C-c i r") 'ido-recentf-open)
(global-set-key (kbd "C-c i s") 'ido-goto-symbol)
(global-set-key (kbd "C-c i b") 'ido-switch-buffer)
(global-set-key (kbd "C-c i m") 'ido-imenu)

;; Imenu
(global-set-key (kbd "C-c m") 'imenu)

;; Emcas refactor
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)

;; Go to char
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)

;; Avy
(global-set-key (kbd "C-: c") 'avy-goto-char)
(global-set-key (kbd "C-: v") 'avy-goto-char-2)
(global-set-key (kbd "C-: l") 'avy-goto-line)
(global-set-key (kbd "C-: w") 'avy-goto-word-1)
(global-set-key (kbd "C-: e") 'avy-goto-word-0)
(global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)
(global-set-key (kbd "M-Z") 'avy-zap-up-to-char-dwim)

;; Move-dup
(global-set-key (kbd "C-s-<up>") 'md/move-lines-up)
(global-set-key (kbd "C-s-<down>") 'md/move-lines-down)
(global-set-key (kbd "C-s-<left>") 'md/duplicate-up)
(global-set-key (kbd "C-s-<right>") 'md/duplicate-down)

;; smart-forward
(global-set-key (kbd "M-s-<up>") 'smart-up)
(global-set-key (kbd "M-s-<down>") 'smart-down)
(global-set-key (kbd "M-s-<left>") 'smart-backward)
(global-set-key (kbd "M-s-<right>") 'smart-forward)

;; Change inner/outer
(global-set-key (kbd "s--") 'change-inner)
(global-set-key (kbd "s-.") 'change-outer)

;; Actions
(global-set-key (kbd "C-x <next>") 'select-current-line)
(global-set-key (kbd "C-c <next>") 'er/expand-region)
(global-set-key (kbd "C-c e <right>") 'select-current-line)
(global-set-key (kbd "C-c e <left>") 'er/expand-region)
(global-set-key (kbd "C-c e w") 'er/mark-word)
(global-set-key (kbd "C-c e c") 'er/mark-comment)

;; HideShow
(global-set-key (kbd "C-x |") 'toggle-hiding)

;; Magit
(global-set-key (kbd "C-x g s") 'magit-status)

;; ;; Mail
(global-set-key (kbd "C-x M-m") 'distopico:mu4e-open)

;; ;; News
(global-set-key (kbd "C-x M-n") 'distopico:elfeed-open)

;; Pomodoro
(global-set-key (kbd "M-p") 'pomodoro-action)

;; ;; Erc
(global-set-key (kbd "C-x M-e") 'distopico:erc-start-ask-or-switch)

;; ;; GnuSocial
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


(provide 'setup-keybindings)