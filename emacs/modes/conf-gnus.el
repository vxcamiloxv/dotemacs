;;; Code:

(require 'gnus)
(require 'gnus-art)
(require 'gnus-msg)
(require 'gnus-demon)
(require 'gnus-topic)
(require 'gnus-notifications)
;;(require 'gnus-desktop-notify)
(require 'nnir)
(require 'smtpmail)
(require 'smtpmail-multi)
(require 'mailcap)

;; Control vars
(defgroup distopico:gnus-switch nil
  "Smart switch with gnus buffers."
  :group 'gnus)

(defcustom distopico:gnus-switch-buffer-equal-list
  '("*Group*" "*BBDB*")
  "The list that contain buffer string and use `string-equal' for compare."
  :type 'list
  :group 'distopico:gnus-switch)

(defcustom distopico:gnus-switch-buffer-match-list
  '("\*Summary" "\*mail" "\*wide" "\*reply" "\*Article")
  "The list that contain buffer regexp and use `string-match' for compare."
  :type 'list
  :group 'distopico:gnus-switch)

(defcustom distopico:gnus-switch-on-after-hook nil
  "Run hook after do `distopico:gnus-switch-on'."
  :type 'hook
  :group 'gnus-warning)

(defvar distopico:gnus-switch-before-window-configuration nil
  "This variable storage window configuration before `distopico:gnus-switch-on'.")

(defvar distopico:gnus-maildir "~/rss2maildir/news")

(defface gnus-topic-faceq
  '((t (:inherit default :weight bold)))
  "Face for GNUS topics.
   By default GNUS doesn't support topic faces.
Let's add them, following the hack described here:
http://www.emacswiki.org/emacs/GnusFormatting#toc6")

(defface gnus-topic-empty-face
  '((t (:inherit default)))
  "Face for GNUS emtpy topics.")

;; General
;; ---------

;; Defult news server
(setq gnus-select-method '(nntp "news.gmane.org"))

;; Extend method use maildir backend
;; (let ((base distopico:gnus-maildir))
;;   (setf gnus-seconqdary-select-methods nil)
;;   (dolist (name (directory-files base))
;;     (let ((dirmail (concat base "/" name)))
;;       (when (and (file-directory-p dirmail)
;;                  (not (equal name ".."))
;;                  (not (equal name ".")))

;;         (add-to-list 'gnus-secondary-select-methods
;;                      `(nnmaildir ,name
;;                                  (directory ,dirmail)
;;                                  (directory-files nnheader-directory-files-safe)
;;                                  (get-new-mail t)))
;;         ))))

;; set smtpmail-multi data
;; (let ((smtps (remove-if-not 'distopico:gnus-is-smtp (distopico:gnus-auth-sources))))
;;   (eval (distopico:gnus-create-smtpmail-multi-accounts smtps))
;;   (eval (distopico:gnus-create-smtpmail-multi-associations smtps))
;;   (eval (distopico:gnus-create-gnu-posting-styles smtps))
;;   ;; this variable used to exclude own email address when doing reply to all
;;   (eval (distopico:gnus-create-message-dont-reply-to-names smtps))
;;   ;; the first smtp account in the list is the default one
;;   (setq smtpmail-multi-default-account (caar smtpmail-multi-accounts))
;;   ;; (setq user-mail-address (second (car smtpmail-multi-accounts)))
;;   )

;; set the first NNTP account as main
;; (let ((nntps (remove-if-not 'distopico:gnus-is-nntp (distopico:gnus-auth-sources))))
;;   (eval (distopico:gnus-set-gnus-select-method nntps)))


;; set the IMAP accounts
;; (let ((imaps (remove-if-not 'distopico:gnus-is-imap (distopico:gnus-auth-sources))))
;;   ;; (setf gnus-secondary-select-methods nil)
;;   (eval (distopico:gnus-imap-add-to-gnus-secondary-select-methods imaps 'distopico:gnus-is-gmail-vserver)))


;; initialize contacts support from distopico:gnus-contacts.el
;;(distopico:org-contacts-initialize)

;; default sending method - using internal smtp client
;; with the smtpmail-multi package to handle multiple email
;; accounts
;; (setq message-send-mail-function 'smtpmail-multi-send-it)

;; and workaround for Gmail folders
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; Notification
(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications);
;;(gnus-desktop-notify-mode)

;; set gnus-parameter
(setq gnus-parameters
      '(("^nnimap.*"
         (gnus-use-scoring nil)
         (expiry-wait . 2)
         (display . all))
        ("^nnmaildir.*"
         (gnus-use-scoring nil)
         (expiry-wait . 2)
         (display . all))
        (".*/emacs-devel$"
         (to-address . "emacs-devel@gnu.org")
         (to-list . "emacs-devel@gnu.org"))
        (".*/bug-gnu-emacs$"
         (to-list . "bug-gnu-emacs@gnu.org"))
        (".*/emacs-orgmode$"
         (to-address . "emacs-orgmode@gnu.org")
         (to-list . "emacs-orgmode@gnu.org"))))

;; No group considered big, download everything
;; see http://stackoverflow.com/questions/4982831/i-dont-want-to-expire-mail-in-gnus
;; for examples
;;(setq gnus-large-newsgroup 'nil)
(setq gnus-large-newsgroup 100)

;; cache for offline reading
(setq gnus-use-cache t)

;; do not use .newsrc file shared with other newsreaders
(setq gnus-save-newsrc-file nil)

;; do not store/read new newsgroups. We will subscribe
;; manually
(setq gnus-save-killed-list nil
      gnus-check-new-newsgroups nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files and directories
;; General idea: move everything to .emacs.d/gnus-files

;; ;; path to the file with newsgroups information, "~/.newsrc.eld"
;; (setq gnus-startup-file "~/.emacs.d/gnus-files/newsrc")

;; ;; path to the directory where GNUS stores all the mail
;; (setq message-directory "~/.emacs.d/gnus-files/Mail")
;; ;; path to the directory where GNUS stores all news related files
;; (setq gnus-directory "~/.emacs.d/gnus-files/News")
;; ;; where to save articles
;; (setq gnus-article-save-directory gnus-directory)
;; ;; articles cache
;; (setq gnus-cache-directory (concat (file-name-as-directory gnus-directory) "cache"))
;; ;; the cache active file
;; (setq gnus-cache-active-file (concat (file-name-as-directory gnus-cache-directory) "active"))
;; ;; directory where kill files are stored
;; (setq gnus-kill-files-directory gnus-directory)
;; ;; name of nnfolder-directory
;; (setq nnfolder-directory (concat (file-name-as-directory message-directory) "archive"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups view
;;
;; organize groups by topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Format of the group name presented.
;; see https://www.gnu.org/software/emacs/manual/html_mono/gnus.html#Group-Line-Specification
;; for details
;; The format is "group-name (number of unread messages)"
;; if number of unread messages is 0, do not show them
;; if the group-name is an inbox, take the account instead
;; i.e. if the group name is "nnimap+gmail-my.account:INBOX"
;; the printed name will be "gmail-my.account"
;; In order to achive this one have to implement custom formatting
;; function `gnus-user-format-function-group-line'
;;
(setf gnus-group-line-format "%p%3P%*%u&group-line;\n")

;; make all subscribed groups visible even if they don't have unread messages
(setq gnus-permanently-visible-groups ".*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Topics in Groups view
;;

;; format line like "Name (3)" with proper indentation
;; for more details, see
;; http://www.gnu.org/software/emacs/manual/html_node/gnus/Topic-Variables.html#Topic-Variables
(setq gnus-topic-line-format "%i%u&topic-line; %v\n")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window management
;;

;; Prevent changing the Emacs window layout
(setq gnus-use-full-window nil)

;; Bind to F5 force refresh all mail
;;(gnus-activate-all-groups)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary buffer (list of messages)
;;

;; use exact address components extraction
(setq gnus-extract-address-components 'mail-extract-address-components)

;; Set the line format
;; the default one:
;;(setf gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n")
;; Explanation:
;; Unread, Secondary mark, Zcore(%z), indentation
;; (start highlighted text
;; [opening bracket
;; 4 chars for number of lines
;; ':' and 23 characters of the name, To field or Newgroups header
;; ]closing)end highlighted text
;; space and subject
;;
(when nil
  (setq gnus-summary-line-format "%U%R%B %*%-40,40S %(%[%a%]%)  %o\n")
  (setq gnus-show-threads nil)
  (setq gnus-article-sort-functions '((not gnus-article-sort-by-date))))

;; turn on threading
(setq gnus-show-threads t)


;; organize in threads
;; the format and commands below are based on
;; http://www.emacswiki.org/emacs/GnusFormatting#toc4
;; with some modifications
;; 1) Fixed date/time size format
;; 2) date/time as in Thunderbird, today's messages with hh:mm and all others
;;    with dd/mm/yy
;; 3) Removed "Unread" flag, we use faces for it
(setq gnus-summary-line-format "%U%R %(%-15,15&user-date;%-20,20a %B%s%)\n")
(setq gnus-user-date-format-alist '(((gnus-seconds-today)
                                     . "%H:%M")
                                    (t . "%d/%m/%y %H:%M")))
;; organize threads by Reference header (subject not always works as it
;; groups sometimes all messages with the same subject as thread, which is
;; not correct for regular mails
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
;; sort by date descending (latest first)
(setq gnus-thread-sort-functions '((not gnus-thread-sort-by-date)))
;; set of variables specifying how to show thread characters
(setq gnus-sum-thread-tree-false-root "")
(setq gnus-sum-thread-tree-indent " ")
(setq gnus-sum-thread-tree-leaf-with-other "├► ")
(setq gnus-sum-thread-tree-root "")
(setq gnus-sum-thread-tree-single-leaf "╰► ")
(setq gnus-sum-thread-tree-vertical "│")

;; hook on after reading arcticle
(setq gnus-mark-article-hook '(gnus-summary-mark-read-and-unread-as-read))

;; when in windowed system use unicode characters to indicate
;; replied/forwarded mails
(when (window-system)
  (setq gnus-replied-mark 8592
        gnus-forwarded-mark 8594))

;; set Gnus to prefectch article asynchronously
(setq gnus-asynchronous t)
;; set Gnus to update mail every 10 minutes
(setq gnus-use-demon t)
(gnus-demon-add-handler 'gnus-demon-scan-mail 5 nil) ;
;;(gnus-demon-add-handler 'gnus-group-get-new-news 5 nil)
;; (gnus-demon-add-handler 'gnus-demon-scan-news 60 nil)
;; (gnus-demon-add-scanmail)
(gnus-demon-add-rescan)
(gnus-demon-init)

;; add article to cache- *, remove - M-*


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composing message customizations
;;

;; create a signature with the first name from full name
;; (when (and (user-full-name)
;;            (stringp (user-full-name)))
;;   (let ((name (car (split-string (user-full-name) " "))))
;;     (add-to-list 'gnus-posting-styles `(".*" (signature ,(concat "Br,\n/" name))))
;;     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some hotkeys redefinition
;;
(define-key gnus-summary-mode-map (kbd "C-q") 'distopico:gnus-switch)
(define-key gnus-article-mode-map (kbd "C-q") 'distopico:gnus-switch)
(define-key gnus-group-mode-map   (kbd "C-q") 'distopico:gnus-switch)
(global-set-key (kbd "C-x M-l") 'distopico:gnus-switch)

;; Delete article
(define-key gnus-summary-mode-map "D" 'gnus-summary-delete-article)

;; use g instead of Y g (gnus-summary-prepare) to refresh state of the summary buffer
;; to set read/unread etc
(define-key gnus-summary-mode-map "g" 'gnus-summary-prepare)

;; 'u' will mark the current mail as unread and move to the next line
(define-key gnus-summary-mode-map "u" '(lambda () (interactive)
                                         (gnus-summary-clear-mark-forward 1)
                                         (let ((line (line-number-at-pos)))
                                           (gnus-summary-prepare)
                                           (goto-line line))))

;; Bind M-up/down to scroll the article
(define-key gnus-summary-mode-map [M-up] (lambda () (interactive) (gnus-summary-scroll-up -1)))
(define-key gnus-summary-mode-map [M-down] (lambda () (interactive) (gnus-summary-scroll-up 1)))

;; additional to it 'q' in article mode should close the article window
(define-key gnus-article-mode-map "q" 'gnus-summary-expand-window)

;; TAB should switch focus between article and summary
(define-key gnus-article-mode-map (kbd "TAB") 'gnus-article-show-summary)
(define-key gnus-summary-mode-map (kbd "TAB") 'gnus-summary-select-article-buffer)

;; Enter in summary mode should open article full window
(define-key gnus-summary-mode-map (kbd "\r") '(lambda () (interactive)
                                                (let ((gnus-widen-article-window t))
                                                  (gnus-summary-select-article-buffer))))

;; Use ctrl up/down to move between topics in group view
(define-key gnus-group-mode-map [C-up] 'gnus-topic-goto-previous-topic)
(define-key gnus-group-mode-map [C-down] 'gnus-topic-goto-next-topic)

;; Use ctrl up/down to move between threads in summary view
(define-key gnus-summary-mode-map [C-up] 'gnus-summary-prev-thread)
(define-key gnus-summary-mode-map [C-down] 'gnus-summary-next-thread)
;;------------------
;; Functions
;;------------------

(defun gnus-user-format-function-topic-line (dummy)
  "this corresponds to a topic line format of '%n (%a)'"
  ;; TODO: this works only knowing GNUS internals.
  ;; therefore it could be broken later
  (let* ((num-unread (gnus-topic-articles-in-topic entries))
         (topic-face (if (zerop num-unread)
                         'gnus-topic-empty-face
                       'gnus-topic-face)))
    (propertize
     (format "%s (%d)" name num-unread)
     'face topic-face)))

(defun gnus-user-format-function-group-line (dummy)
  (let ((number-of-msgs-suffix
         ;; check if number of unread messages > 0
         (if (> (string-to-number gnus-tmp-number-of-unread) 0)
             (concat " (" gnus-tmp-number-of-unread ")")
           ;; otherwise do no show anything
           ""))
        ;; if it is an inbox, extract the account, since we want all
        ;; inboxes to be in one separate topic [Inbox]
        (group-name (if (distopico:gnus-news-group-is-imap-inbox gnus-tmp-group)
                        (distopico:gnus-account-name-from-group gnus-tmp-group)
                      gnus-tmp-qualified-group)))
    (concat group-name
            number-of-msgs-suffix)))

(defun distopico:gnus-news-group-is-imap-inbox (group)
  "Determine if GROUP is the IMAP inbox"
  (string-match "^nnimap\\+.*INBOX$" group))

(defun distopico:gnus-account-name-from-group (group)
  "Extract account name from the GROUP"
  (string-match "\\(^.*\\)\\+\\(.*\\):\\(.*\\)" group)
  (let ((addr (match-string 2 group)))
    (if (null addr) "(unknown)" addr)))

(defun distopico:gnus-auth-sources ()
  "Return the list of all auth sources from the .authinfo[.gpg]
Temporary wrapper around auth-source-search to avoid bug #22188"
  (auth-source-search :port '(25 465 587 993) :max 999)
  ;; '((:host "machine1" :port "25")
  ;;   (:host "machine2" :port "587")
  ;;   (:host "machine3")
  ;;   (:host "machine4"))
  )


(defun distopico:gnus-preprocess-email (email)
  "Replaces charaters in email address to prepare
it to be interned as a symbol"
  (replace-regexp-in-string (regexp-quote "@") "." email nil 'literal))


(defun distopico:gnus-is-smtp (source)
  "Naive way to determine if the SOURCE from .authinfo is a smtp
account"
  (let ((host (plist-get source :host))
        (port (plist-get source :port)))
    (or (and host (string-match "^smtp\\." host))
        (and port
             (or
              (string= port "25")
              (string= port "465")
              (string= port "587"))))))

(defun distopico:gnus-is-nntp (source)
  "Naive way to determine if the SOURCE from .authinfo is a nntp
account"
  (let ((host (plist-get source :host))
        (port (plist-get source :port)))
    (or (and host (or (string-match "^news\\." host)
                      (string-match "^nntp\\." host)))
        (and port
             (or
              (string= port "119")
              (string= port "563"))))))


(defun distopico:gnus-is-imap (source)
  "Naive way to determine if the SOURCE from .authinfo is a nntp
account"
  (let ((host (plist-get source :host))
        (port (plist-get source :port)))
    (or (and host (or (string-match "^imap\\." host)
                      (string-match "^gmail-" host)))
        (and port
             (or
              (string= port "993")
              (string= port "143")))
        )))

  (defun distopico:gnus-is-gmail-vserver (source)
    "Naive way to determine if the SOURCE from .authinfo is a gmail
virtual server account.
See https://lists.gnu.org/archive/html/info-gnus-english/2010-07/msg00013.html
for examples.
Every virtual server for all gmail accounts assumed to start with
'gmail-', like 'gmail-mymail1', for simplisity"
    (string-match "^gmail-" (plist-get source :host)))


(defun distopico:gnus-create-smtpmail-multi-accounts (smtps)
  "Given the SMTPS - list of smtp accounts from authinfo, create a variable
`smtpmail-multi-accounts' from smtpmail-multi package, assuming
login is the email address.
Best suitable for gmail-like services.
If port is not specified use the 25 port and no encryption.
If port is 587 use starttls encryption.
For all other port numbers assuming no encryption.
Example:
Suppose .authinfo[.gpg] contains entries like this:
machine smtp.googlemail.com login mylogin@gmail.com password mypass1 port 587
machine mymailserv1.com login mylogin1@mymailserv password mypass2 port 25
machine mymailserv2.com login mylogin2@mymailserv password mypass3
Then the list of these smtp accounts (filtered) will produce the following
code:
(setq smtpmail-multi-accounts
      ((mylogin\.gmail\.com \"mylogin@gmail.com\"
                            \"smtp.googlemail.com\"
                            587
                            \"mylogin@gmail.com\"
                            starttls nil nil nil)
       (mylogin1\.mymailserv \"mylogin1@mymailserv\"
                             \"mymailserv1.com\"
                             25
                             \"mylogin1@mymailserv\"
                             nil nil nil nil)
       (mylogin2\.mymailserv \"mylogin2@mymailserv\"
                             \"smtp.mymailserv2.com\"
                             25
                             \"mylogin2@mymailserv\"
                             nil nil nil nil)))
This code could be later `eval'uated. "
  (let ((accounts nil))
    (dolist (source smtps)
      (let* ((user (plist-get source :user))
             (host (plist-get source :host))
             (port-from-source (plist-get source :port))
             (port (if port-from-source port-from-source "25")))
          (push
           `(,(intern (distopico:gnus-preprocess-email user)) .
             (,user
               ,host
               ,(string-to-number port)
               ,user
               ,(if (string= port "587") 'starttls 'nil)
               nil nil nil))
           accounts)))
    `(setq smtpmail-multi-accounts (quote ,(reverse accounts)))))

(defun distopico:gnus-create-smtpmail-multi-associations (smtps)
  "Given the SMTPS - list of smtp accounts from authinfo, create a variable
`smtpmail-multi-associations' from smtpmail-multi package, assuming
login is the email address.
Best suitable for gmail-like services.
Example:
Suppose .authinfo[.gpg] contains entries like this:
machine smtp.googlemail.com login mylogin@gmail.com password mypass1 port 587
machine mymailserv1.com login mylogin1@mymailserv password mypass2 port 25
machine mymailserv2.com login mylogin2@mymailserv password mypass3
Then the list of these smtp accounts (filtered) will produce the following
code:
(setq smtpmail-multi-associations
      ((\"mylogin@gmail.com\" mylogin\.gmail\.com)
       (\"mylogin1@mymailserv\" mylogin1\.mymailserv)
       (\"mylogin2@mymailserv\" mylogin2\.mymailserv)))
This code could be later `eval'uated. "
  (let ((accounts nil))
    (dolist (source smtps)
      (let* ((mail (plist-get source :user))
             (symb (distopico:gnus-preprocess-email mail)))
          (push
           `(,mail ,(intern symb))
           accounts)))
    `(setq smtpmail-multi-associations (quote ,(reverse accounts)))))


(defun distopico:gnus-create-gnu-posting-styles (smtps)
  "Given the SMTPS - list of smtp accounts from authinfo, create a variable
`gnus-posting-styles' from GNUS package, assuming
login is the email address.
Best suitable for gmail-like services.
Example:
Suppose .authinfo[.gpg] contains entries like this:
machine smtp.googlemail.com login mylogin@gmail.com password mypass1 port 587
machine mymailserv1.com login mylogin1@mymailserv password mypass2 port 25
machine mymailserv2.com login mylogin2@mymailserv password mypass3
Then the list of these smtp accounts (filtered) will produce the following
code:
(setq gnus-posting-styles
      (((header \"to\" \"mylogin@gmail.com\")
        (address \"mylogin@gmail.com\"))
       ((header \"cc\" \"mylogin@gmail.com\")
        (address \"mylogin@gmail.com\"))
       ((header \"to\" \"mylogin1@mymailserv\")
        (address \"mylogin1@mymailserv\"))
       ((header \"cc\" \"mylogin1@mymailserv\")
        (address \"mylogin1@mymailserv\"))
       ((header \"to\" \"mylogin2@mymailserv\")
        (address \"mylogin2@mymailserv\"))
       ((header \"cc\" \"mylogin2@mymailserv\")
        (address \"mylogin2@mymailserv\"))))
This code could be later `eval'uated. "
  (let ((accounts nil))
    (dolist (source smtps)
      (let ((mail (plist-get source :user)))
        (push
         `((header "to" ,mail)
           (address ,mail))
         accounts)
        (push
         `((header "cc" ,mail)
           (address ,mail))
         accounts)))
    `(setq gnus-posting-styles (quote ,(reverse accounts)))))


(defun distopico:gnus-set-gnus-select-method (nntps)
  "Given the NNTPS - list of NNTP accounts from authinfo, create a variable
`gnus-select-method' from GNUS package, taking the first NNTP account
from the list. If the list is empty, the variable is not changed.
Example:
Suppose .authinfo[.gpg] contains entries like this:
machine news.myserver.net login mylogin password mypass
machine nntp.myserver1.net login mylogin1 password mypass1 port 563
Then the list of these (filtered) NNTP accounts will produce the following
code:
(setq gnus-select-method
      '(nntp \"news.myserver.net\"))
This code could be later `eval'uated. "
  (when nntps
    (let ((nntp (car nntps)))
      `(setq gnus-select-method '(nntp ,(plist-get nntp :host))))))



(defun distopico:gnus-imap-add-to-gnus-secondary-select-methods (imaps is-gmail)
  "Given the IMAPS - list of IMAP accounts from authinfo, append the list
`gnus-secondary-select-methods' from GNUS package with the generated
entries for typical gmail-alike IMAP servers.
Example:
Suppose .authinfo[.gpg] contains entries like this:
Then the list of these (filtered) NNTP accounts will produce the following
code:
This code could be later `eval'uated. "
  (let ((accounts nil))
    (dolist (source imaps)
      (let ((user (plist-get source :user))
            (host (plist-get source :host)))
        (if (funcall is-gmail source)   ; gmail account
            (push
             `(add-to-list 'gnus-secondary-select-methods
                           '(nnimap ,host
                                    (nnimap-address "imap.gmail.com")
                                    (nnimap-server-port "imaps")
                                    (nnimap-stream ssl)
                                    (nnir-search-engine imap)))
             accounts)
          (push
           `(add-to-list 'gnus-secondary-select-methods
                         '(nnimap ,user
                                  (nnimap-address ,host)
                                  (nnimap-server-port "imaps")
                                  (nnimap-stream ssl)
                                  (nnir-search-engine imap)))
           accounts))))
    `(progn ,@(reverse accounts))))


(defun distopico:gnus-create-message-dont-reply-to-names (smtps)
  "Given the SMTPS - list of smtp accounts from authinfo, create a variable
`message-dont-reply-to-names' from message.el, assuming
login is the email address.
Example:
Suppose .authinfo[.gpg] contains entries like this:
machine smtp.googlemail.com login mylogin@gmail.com password mypass1 port 587
machine mymailserv1.com login mylogin1@mymailserv password mypass2 port 25
machine mymailserv2.com login mylogin2@mymailserv password mypass3
Then the message-dont-reply-to-names variable with regexps will be generated:
(setq message-dont-reply-to-names
      \"\\\\(mylogin@gmail.com\\\\|mylogin1@mymailserv\\\\|mylogin2@mymailserv\\\\)\")
This code could be later `eval'uated. "
  (let ((accounts (mapcar (lambda (source) (plist-get source :user)) smtps)))
    `(setq message-dont-reply-to-names ,(concat "\\("
                                                (mapconcat 'identity accounts "\\|")
                                                "\\)"))))


(defun distopico:gnus-switch ()
  "Switch gnus buffers smart."
  (interactive)
  (let ((bufname (buffer-name)))
    (if (distopico:gnus-switch-buffer-exist bufname)
        (distopico:gnus-switch-off) ;; If current buffer is match of gnus buffers, switch off
      (distopico:gnus-switch-on)    ;; If current buffer is not match of gnus buffers, switch on
      )))

(defun distopico:gnus-switch-on ()
  "Switch gnus buffer on."
  (setq distopico:gnus-switch-before-window-configuration (current-window-configuration))
  (delete-other-windows)
  (if (get-buffer "*Group*")
      (switch-to-buffer "*Group*")
    (gnus))
  (run-hooks 'distopico:gnus-switch-on-after-hook))

(defun distopico:gnus-switch-off ()
  "Swtich gnus buffer off."
  (let (buf bufname)
    (when (and distopico:gnus-switch-before-window-configuration
               (boundp 'distopico:gnus-switch-before-window-configuration))
      (set-window-configuration distopico:gnus-switch-before-window-configuration))
    ;; Bury buffer if is still "gnus-buffer"
    ;; after change window configuration.
    (dolist (buf (buffer-list))
      (setq bufname (buffer-name buf))
      (if (distopico:gnus-switch-buffer-exist bufname)
          (bury-buffer buf)))))

(defun distopico:gnus-switch-buffer-exist (bufname)
  "If gnus buffer have exist, return t.
Otherwise return nil."
  (let (buffer-exist-p)
    (catch 'exist
      (dolist (element distopico:gnus-switch-buffer-equal-list)
        (when (string-equal element bufname)
          (setq buffer-exist-p t)
          (throw 'exist "Equal with `distopico:gnus-switch-buffer-equal-list'.")))
      (dolist (element distopico:gnus-switch-buffer-match-list)
        (when (string-match (regexp-quote element) bufname)
          (setq buffer-exist-p t)
          (throw 'exist "Match with `distopico:gnus-switch-buffer-match-list'."))))
    buffer-exist-p))

;; Run gnus on startup
;; (gnus)
;;(run-at-time t 60 '(lambda () ( shell-command "~/rss2maildir/rss2maildir.py -c ~/rss2maildir/example_config.json" )))

(provide 'conf-gnus)
