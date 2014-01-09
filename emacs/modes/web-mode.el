(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))

(defun cwebber-web-mode-customizations ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)

  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (setq web-mode-comment-style 2)

  ;(local-set-key (kbd "RET") 'newline-and-indent)
 )

(add-hook 'web-mode-hook 'cwebber-web-mode-customizations)

(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-current-element-highlight t)

;; More tango-y colors
(set-face-attribute 'web-mode-html-tag-face nil
                    :foreground "#729fcf")
(set-face-attribute 'web-mode-html-tag-bracket-face nil
                    :foreground "#FFE84B")
(set-face-attribute 'web-mode-current-element-highlight-face nil
                    :foreground "#FF8A4B")

; zencoding
(require 'emmet-mode)
(setq emmet-indentation 2)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(define-key emmet-mode-keymap (kbd "C-j") nil)
(define-key emmet-mode-keymap (kbd "<C-return>") nil)
(define-key emmet-mode-keymap (kbd "C-c i") 'emmet-expand-line)

; js2-mode
(require 'js2-mode)
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-hook 'css-mode-hook '(lambda ()
         (define-key css-mode-map (kbd "M-i") 'helm-css-scss)))

;; Autocomplete
(add-hook 'web-mode-hook 'auto-complete-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'css-mode)

;; Auto-pairs
(setq web-mode-extra-auto-pairs
  '(
    ("angular"    . (("{{ " " }}")))
    ("asp"        . (("<% " " %>")))
    ("aspx"       . (("<% " " %>")
                     ("<%=" "%>")
                     ("<%#" "%>")
                     ("<%$" "%>")
                     ("<%@" "%>")
                     ("<%:" "%>")
                     ("<%-" "- " " --%>")))
    ("blade"      . (("{{ " " }}")
                     ("{{-" "- " " --}}")))
    ("django"     . (("{{ " " }}")
                     ("{% " " %}")
                     ("{# " " #}")))
    ("erb"        . (("<% " " %>")
                     ("<%=" "%>")
                     ("<%#" "%>")))
    ("freemarker" . (("<% " " %>")
                     ("${ " " }")
                     ("[% " " %]")
                     ("[# " " #]")
                     ("[#-" "- " " --]")))
    ("jsp"        . (("<% " " %>")
                     ("<%-" "- " " %>")
                     ("<%=" "%>")
                     ("<%!" "%>")
                     ("<%@" "%>")
                     ("${ " " }")))
    ("html"        . (("<" ">")))
    ("mason"      . (("<% " " %>")))
    ("php"        . (("<?p" "hp " " ?>")
                     ("<? " " ?>")
                     ("<?=" "?>")))
    ("underscore" . (("<% " " %>")))
    (nil          . (("<!-" "- " " -->")))
    )
)

(provide 'web-mode)
