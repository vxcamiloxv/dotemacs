;;; Code:
(require 'web-mode)
(require 'company)
(require 'company-web-html)
(require 'company-web-jade)
(require 'web-beautify)

;; Control
(defcustom distopico:web-company-backends
  '(company-yasnippet
    company-dabbrev company-capf
    company-keywords company-restclient
    company-dabbrev-code company-gtags company-etags)
  "General `company-mode' backends for diferents web modes."
  :type 'list
  :group 'distopico)


;; Baisc
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.swig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.*tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.*tml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode))

;; Engines
(setq web-mode-engines-alist
      '(
        ("php" . "\\.phtml\\'")
        ("blade" . "\\.blade\\'")
        ("django" . "\\.[sd]tpl\\'")
        ("django" . "\\.[sd]tml\\'")
        ("swig" . "\\.swig\\'")))

;; Settings
(setq-default web-mode-enable-auto-pairing t
              web-mode-enable-auto-opening t
              web-mode-enable-auto-closing t
              web-mode-enable-auto-indentation t
              web-mode-enable-auto-quoting t
              web-mode-enable-auto-expanding t
              web-mode-enable-block-face t
              web-mode-enable-part-face t
              web-mode-enable-comment-keywords t
              web-mode-enable-css-colorization t
              web-mode-enable-current-element-highlight t
              web-mode-enable-current-column-highlight t
              web-mode-enable-heredoc-fontification t
              web-mode-enable-engine-detection t

              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2

              web-mode-style-padding 2
              web-mode-script-padding 2
              web-mode-block-padding 0
              web-mode-comment-style 2)

;; Custom web-mode colors
(custom-set-faces
 '(web-mode-html-tag-face
   ((t (:foreground "#729fcf"))))
 '(web-mode-html-tag-bracket-face
   ((t (:foreground "#FFE84B"))))
 '(web-mode-current-element-highlight-face
   ((t (:foreground "#FF8A4B"))))
 '(web-mode-current-element-highlight-face
   ((t (:background "#000000"
                    :foreground "#FF8A4B")))))

(eval-after-load 'web-mode
  '(progn
     ;; Set some keys
     (define-key web-mode-map (kbd "C-c C-b f") 'web-beautify-html)
     (define-key web-mode-map (kbd "C-'") 'company-web-html)))

;; Functions
(defun distopico:web-mode-hook ()
  "Hooks for `web-mode'."
  ;; Company-mode
  (add-to-list (make-local-variable 'distopico:web-company-backends) 'company-web-html)
  (add-to-list (make-local-variable 'company-backends) distopico:web-company-backends))


(defun distopico:pug-mode-hook ()
  "Hooks for `pug-mode'."
  (aggressive-indent-mode -1)
  ;; Company-mode
  (add-to-list (make-local-variable 'distopico:web-company-backends) 'company-web-jade)
  (add-to-list (make-local-variable 'company-backends) distopico:web-company-backends))

(defun distopico:web-mode-before-auto-complete-hooks ()
  (let ((web-mode-cur-language
         (web-mode-language-at-pos)))
    (if (string= web-mode-cur-language "php")
        (yas-activate-extra-mode 'php-mode)
      (yas-deactivate-extra-mode 'php-mode))
    (if (string= web-mode-cur-language "css")
        (setq emmet-use-css-transform t)
      (setq emmet-use-css-transform nil))))

;; Hooks
(add-hook 'web-mode-hook 'distopico:web-mode-hook)
(add-hook 'web-mode-before-auto-complete-hooks 'distopico:web-mode-before-auto-complete-hooks)
(add-hook 'pug-mode-hook 'distopico:pug-mode-hook)

(provide 'conf-web-mode)
