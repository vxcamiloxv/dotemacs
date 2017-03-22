;;; Code:
(require 'org-jira)

(setq org-jira-working-dir (concat distopico:org-directory "jira/")
      org-jira-use-status-as-todo t)
(setq org-agenda-files (append org-agenda-files (file-expand-wildcards (concat org-jira-working-dir "*.org"))))

(setq org-todo-keyword-faces (append org-todo-keyword-faces
                                     '(("OPEN" . (:foreground "#df3800" :weight bold))
                                       ("TO-DO" . (:foreground "#df3800" :weight bold))
                                       ("IN-PROGRESS" . (:foreground "#566ea2" :weight bold))
                                       ("REJECTED" . (:foreground "#b68800" :weight bold))
                                       ("UNDER-REVIEW" . (:foreground "PaleGreen" :weight bold))
                                       ("IN-REVIEW" . (:foreground "#4d9694" :weight bold))
                                       ("APPROVED" . (:foreground "#448c27" :weight bold)))))

;; Remove default key map
(define-key org-jira-entry-mode-map (kbd "C-c pg") nil)
(define-key org-jira-entry-mode-map (kbd "C-c ib") nil)
(define-key org-jira-entry-mode-map (kbd "C-c ig") nil)
(define-key org-jira-entry-mode-map (kbd "C-c ia") nil)
(define-key org-jira-entry-mode-map (kbd "C-c ih") nil)
(define-key org-jira-entry-mode-map (kbd "C-c if") nil)
(define-key org-jira-entry-mode-map (kbd "C-c iF") nil)
(define-key org-jira-entry-mode-map (kbd "C-c iu") nil)
(define-key org-jira-entry-mode-map (kbd "C-c iw") nil)
(define-key org-jira-entry-mode-map (kbd "C-c ir") nil)
(define-key org-jira-entry-mode-map (kbd "C-c ic") nil)
(define-key org-jira-entry-mode-map (kbd "C-c ik") nil)
(define-key org-jira-entry-mode-map (kbd "C-c sc") nil)
(define-key org-jira-entry-mode-map (kbd "C-c sg") nil)
(define-key org-jira-entry-mode-map (kbd "C-c cu") nil)
(define-key org-jira-entry-mode-map (kbd "C-c wu") nil)
(define-key org-jira-entry-mode-map (kbd "C-c tj") nil)
(define-key org-jira-entry-mode-map (kbd "C-c wa") nil)
(define-key org-jira-entry-mode-map (kbd "C-c ii") nil)
(define-key org-jira-entry-mode-map (kbd "C-c id") nil)

;; Set in my org key map
(define-key org-jira-entry-mode-map (kbd "C-c o jp") 'org-jira-get-projects)
(define-key org-jira-entry-mode-map (kbd "C-c o jb") 'org-jira-browse-issue)
(define-key org-jira-entry-mode-map (kbd "C-c o ji") 'org-jira-get-issues)
(define-key org-jira-entry-mode-map (kbd "C-c o ja") 'org-jira-get-issue-in-current-buffer)
(define-key org-jira-entry-mode-map (kbd "C-c o jh") 'org-jira-get-issues-headonly)
(define-key org-jira-entry-mode-map (kbd "C-c o jf") 'org-jira-get-issues-from-filter-headonly)
(define-key org-jira-entry-mode-map (kbd "C-c o jF") 'org-jira-get-issues-from-filter)
(define-key org-jira-entry-mode-map (kbd "C-c o ju") 'org-jira-update-issue)
(define-key org-jira-entry-mode-map (kbd "C-c o jw") 'org-jira-progress-issue)
(define-key org-jira-entry-mode-map (kbd "C-c C-t") 'org-jira-progress-issue)
(define-key org-jira-entry-mode-map (kbd "C-c o jr") 'org-jira-refresh-issue)
(define-key org-jira-entry-mode-map (kbd "C-c o jc") 'org-jira-create-issue)
(define-key org-jira-entry-mode-map (kbd "C-c o jk") 'org-jira-copy-current-issue-key)
(define-key org-jira-entry-mode-map (kbd "C-c o jt") 'org-jira-create-subtask)
(define-key org-jira-entry-mode-map (kbd "C-c o js") 'org-jira-get-subtasks)
(define-key org-jira-entry-mode-map (kbd "C-c o jm") 'org-jira-update-comment)
(define-key org-jira-entry-mode-map (kbd "C-c o jl") 'org-jira-update-worklog)
(define-key org-jira-entry-mode-map (kbd "C-c o jj") 'org-jira-todo-to-jira)
(define-key org-jira-entry-mode-map (kbd "C-c o jL") 'org-jira-add-blank-worklog)
(define-key org-jira-entry-mode-map (kbd "C-c o jn") 'org-jira-clock-in)
(define-key org-jira-entry-mode-map (kbd "C-c o jd") 'org-jira-done)

;; Functions
(defun distopico:turn-on-jira-mode-hook ()
  "Enable jira-mode by directory name."
                                        ;
  (when (and buffer-file-name (string-match "jira" buffer-file-name))
    (org-jira-mode t)))

;; Hooks
(add-hook 'org-mode-hook #'distopico:turn-on-jira-mode-hook 'append)

(provide 'conf-jira)
