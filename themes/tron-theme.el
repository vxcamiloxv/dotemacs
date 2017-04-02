;;; tron-theme.el --- A theme loosely based on Tron: Legacy colors

;; Copyright (C) 2012 Tom Willemsen <tom@ryuslash.org>

;; Author: Tom Willemsen <tom@ryuslash.org>
;; Created: Wed Jan 4 2012
;; Version: 15
;; Keywords: faces
;; URL: http://ryuslash.org/projects/tron-theme.html
;; Modified by Distopico Vegan <distopico@riseup.net>

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; tron-theme is an Emacs theme that is loosely based on the colors
;; observed in the movie Tron: Legacy.

;;; Change Log:

;; 1 - Initial release.
;; 2 - Decrease size of the `org-level-*' faces.
;; 3 - Add `info-title-1' through `info-title-4', `info-xref' and
;;     `info-xref-visited' faces.
;; 4 - Add `custom-button', `custom-button-mouse' and
;;     `custom-button-pressed' faces.
;; 5 - Add `css-property' and `css-proprietary-property' faces.
;; 6 - Add `erc-default-face', `erc-input-face' and
;;     `erc-nick-default-face' faces.
;; 7 - Change `mode-line', `mode-line-buffer-id' and
;;     `mode-line-inactive' faces.
;; 8 - Add `region' face.
;;   - Add riley's changes:
;;     - Add `show-paren-match-face', `eshell-ls-archive-face',
;;       `eshell-ls-backup-face', `eshell-ls-clutter-face',
;;       `eshell-ls-directory-face', `eshell-ls-executable-face',
;;       `eshell-ls-missing-face', `eshell-ls-product-face',
;;       `eshell-ls-readonly-face', `eshell-ls-readonly-face',
;;       `eshell-ls-special-face', `eshell-ls-symlink-face',
;;       `eshell-ls-unreadable-face', `eshell-prompt-face',
;;       `eshell-test-failed-face' and `eshell-test-ok-face' faces.
;;     - Change `font-lock-comment-face' face.
;; 9 - Fix typo in `font-lock-variable-name-face' face.
;;   - Add `magit-header', `magit-diff-add', `magit-diff-del',
;;     `magit-diff-hunk-header', `magit-diff-file-header',
;;     `magit-item-highlight' and `magit-item-mark' faces.
;; 10 - Add `rst-level-1-face' through `rst-level-6-face' faces.
;; 11 - Change `font-lock-warning-face', `header-line',
;;      `jabber-chat-prompt-local' and `jabber-chat-text-local'
;;      faces.
;;    - Add header.
;;    - Add local variables, enable `rainbow-mode'.
;; 12 - Remove quotes from :inherit properties.
;;    - Explicitly set colors for `fringe'.
;; 13 - Add `tabbar-mode' faces
;; 14 - Fix faces fro `gnusocial-mode' and `company-mode'
;; 15 - Add `ediff-mode' faces

;;; Code:

(deftheme tron
  "Create 2012-01-01")

(require 'ezimage)

(custom-theme-set-variables
 'tron

 '(powerline-active1 "#00779a")
 '(powerline-active2 "#00475a")
 '(linum-format "%6d") ;; Default 7i
 '(fringe-mode 10 nil (fringe))
 )

(custom-theme-set-faces
 'tron
 '(bold ((t (:foreground "#24c6e0" :weight bold))))
 '(bold-italic ((t (:inherit bold :slant italic))))
 '(cursor ((t (:background "#15abc3"))))
 '(default ((t (:background "#00080A" :foreground "#15abc3" :family "Hack"))))
 '(header-line ((t (:inherit mode-line))))
 '(mouse ((t (:foreground "#e0c625"))))
 '(region ((t (:background "#e0c625" :foreground "#000000"))))
 '(highlight ((t (:background "#001E21" ))))
 '(hl-line ((t (:background "gray13" ))))
 '(linum ((t (:background "#00080A" :foreground "#005050" :height 70))))
 '(linum-highlight-face ((t (:background "#00080A" :foreground "cyan" :height 70))))
 ;;'(show-paren-match-face ((t (:weight bold))))
 ;;'(fringe ((t (:background "#000000" :foreground "#15abc3"))))
 ;;'(linum ((t (:inherit default))))

 ;; Cursorg
 ;;'(cursor ((t (:foreground "#ffffff" :background "#013d4c"))))

 ;; Margin Fringes
 '(fringe ((t ( :background "#001214" :foreground "#006060" ))))

 ;; Others
 '(escape-glyph ;; Things like  and other control chars.
   ((t (:foreground "#FF6600" :background "#011d2c"))))

 ;; column-marker
 '(column-marker-1 ((t (:background "#a3e8ef" :foreground "#000000"))))
 '(column-marker-2 ((t (:background "#55c3f8" :foreground "#000000"))))
 '(column-marker-3 ((t (:background "#f6faf9" :foreground "#000000"))))

 ;; css
 '(css-property ((t (:inherit font-lock-variable-name-face))))
 '(css-proprietary-property ((t (:inherit mouse))))

 ;; custom
 '(custom-button ((t (:inherit default :box (:line-width 1 :color "#15abc3")))))
 '(custom-button-mouse ((t (:inherit custom-button :box (:color "#e0c624")))))
 '(custom-button-pressed ((t (:inherit custom-button :box (:color "#05e4a5")))))

 ;; Eshell
 '(eshell-ls-archive-face ((t (:foreground "#029cdc" :bold t))))
 '(eshell-ls-backup-face ((t (:inherit font-lock-builtin-face :italic t))))
 '(eshell-ls-clutter-face ((t (:foreground "OrangeRed" :bold t))))
 '(eshell-ls-directory-face ((t (:foreground "#e0c625" :bold t))))
 '(eshell-ls-executable-face ((t (:foreground "#00815b"))))
 '(eshell-ls-missing-face ((t (:foreground "#e0c624" :bold t))))
 '(eshell-ls-product-face ((t (:foreground "OrangeRed"))))
 '(eshell-ls-readonly-face ((t (:foreground "#808080" :italic t))))
 '(eshell-ls-special-face ((t (:foreground "Mauve" :bold t))))
 '(eshell-ls-symlink-face ((t (:foreground "#035390" :bold t))))
 '(eshell-ls-unreadable-face ((t (:foreground "Grey30"))))
 '(eshell-prompt-face ((t (:foreground "#e0c625"))))
 '(eshell-test-failed-face ((t (:foreground "OrangeRed" :bold t))))
 '(eshell-test-ok-face ((t (:foreground "#05e4a5" :bold t))))

 ;; font-lock
 '(font-lock-builtin-face ((t (:foreground "#029cdc"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#2e3436" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "#808080" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#029cdc"))))
 '(font-lock-doc-face ((t (:foreground "#00815b"))))
 '(font-lock-function-name-face ((t (:foreground "#e0c624" :bold t))))
 '(font-lock-keyword-face ((t (:inherit font-lock-builtin-face :weight bold))))
 '(font-lock-negation-char-face ((t (:foreground "#c3ab15"))))
 '(font-lock-preprocessor-face ((t (:foreground "#c3ab15" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#05e4a5"))))
 '(font-lock-type-face ((t (:foreground "#029cdc" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#00815b"))))
 '(font-lock-warning-face ((t (:foreground "OrangeRed"))))

 ;; info
 '(info-title-1 ((t (:inherit org-level-1))))
 '(info-title-2 ((t (:inherit org-level-2))))
 '(info-title-3 ((t (:inherit org-level-3))))
 '(info-title-4 ((t (:inherit org-level-4))))
 '(info-xref ((t (:inherit default :underline t))))
 '(info-xref-visited ((t (:inherit mouse :underline t))))

 ;; magit
 '(magit-header ((t (:foreground "#e0c624" :weight bold))))
 '(magit-diff-add ((t (:foreground "#00815b"))))
 '(magit-diff-del ((t (:foreground "OrangeRed"))))
 '(magit-diff-hunk-header ((t (:background "#101010"))))
 '(magit-diff-file-header ((t (:weight bold :inherit magit-diff-hunk-header))))
 '(magit-item-highlight ((t (:background "#101010"))))
 '(magit-item-mark ((t (:background "#808080"))))

 ;; Tabbar
 '(tabbar-default ((t (:inherit variable-pitch :height 0.8 :background "#001214"))))
 '(tabbar-unselected ((t (:inherit tabbar-default :background "#001214" :foreground "white" :box (:line-width 1 :color "cyan" ) ))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "cyan" :foreground "#001214" :box (:line-width 1 :color "cyan" ) ))))
 '(tabbar-modified ((t (:inherit tabbar-default :background "#001214" :foreground "green" :box (:line-width 1 :color "cyan" ) ))))
 '(tabbar-selected-modified ((t (:inherit tabbar-default :background "cyan" :foreground "#001214" :box (:line-width 1 :color "cyan" ) ))))
 '(tabbar-highlight ((t (:underline nil :box (:line-width 1 :color "#0cd6e4") ))))
 '(tabbar-button ((t (:inherit tabbar-default :box (:line-width 1 :color "#001214" :style none) ))))
 '(tabbar-button-highlight ((t (:inherit tabbar-button))))

 '(tabbar-separator ((t (:inherit tabbar-default :height 0.5))))

 ;; mode-line
 ;;'(mode-line ((t (:foreground "#e0c625" :background nil :box nil))))
 ;;'(mode-line-buffer-id ((t (:weight bold))))
 ;;'(mode-line-inactive ((t (:foreground "#15abc3" :background nil :box nil))))
 '(mode-line ((t (:background "#0b2c2d" :box nil :foreground "#0cd6e4" :height 90))))
 '(mode-line-inactive ((t (:weight light :box nil :background "#002329" :foreground "#ffffff" :inherit (mode-line)))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box nil (t (:inherit (highlight)))))))
 '(mode-line-buffer-id ((t (:weight bold :box nil))))

 ;; rainbow-delimiters
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#a3e8ef"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#55c3f8"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#f6faf9"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#4ed8f5"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#004a88"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#f0ffff"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#51d7f0"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#be9194"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#94949c"))))

 ;; rst
 '(rst-level-1-face ((t (:inherit rainbow-delimiters-depth-1-face :weight bold))))
 '(rst-level-2-face ((t (:inherit rainbow-delimiters-depth-2-face :weight bold))))
 '(rst-level-3-face ((t (:inherit rainbow-delimiters-depth-3-face :weight bold))))
 '(rst-level-4-face ((t (:inherit rainbow-delimiters-depth-4-face :weight bold))))
 '(rst-level-5-face ((t (:inherit rainbow-delimiters-depth-5-face :weight bold))))
 '(rst-level-6-face ((t (:inherit rainbow-delimiters-depth-6-face :weight bold))))

 ;; outline
 '(outline-1 ((t (:inherit org-level-1))))
 '(outline-2 ((t (:inherit org-level-2))))
 '(outline-3 ((t (:inherit org-level-3))))
 '(outline-4 ((t (:inherit org-level-4))))
 '(outline-5 ((t (:inherit org-level-5))))
 '(outline-6 ((t (:inherit org-level-6))))
 '(outline-7 ((t (:inherit org-level-7))))
 '(outline-8 ((t (:inherit org-level-8))))

 ;; markdown
 '(markdown-header-face-1 ((t (:inherit org-level-1))))
 '(markdown-header-face-2 ((t (:inherit org-level-2))))
 '(markdown-header-face-3 ((t (:inherit org-level-3))))
 '(markdown-header-face-4 ((t (:inherit org-level-4))))
 '(markdown-header-face-5 ((t (:inherit org-level-5))))
 '(markdown-header-face-6 ((t (:inherit org-level-6))))

 ;; jabber
 '(jabber-chat-prompt-local ((t (:foreground "#15abc3"  :weight bold))))
 '(jabber-chat-prompt-foreign ((t (:inherit font-lock-function-name-face))))
 '(jabber-chat-prompt-system ((t (:inherit font-lock-doc-face))))
 '(jabber-chat-text-local ((t (:foreground "#15abc3"))))
 '(jabber-chat-text-foreign ((t (:inherit font-lock-function-name-face :weight normal))))
 '(jabber-activity-face ((t (:foreground "#00815b"))))
 '(jabber-activity-personal-face ((t (:foreground "OrangeRed"))))
 '(jabber-roster-user-online ((t (:foreground "#05e4a5"))))
 '(jabber-roster-user-away ((t (:foreground "#0086AB"))))

 ;; erc
 '(erc-default-face ((t (:inherit jabber-chat-text-local))))
 '(erc-input-face ((t (:inherit jabber-chat-text-foreign))))
 '(erc-nick-default-face ((t (:inherit jabber-chat-prompt-system))))

 ;; Minibuffer
 '(minibuffer-prompt ((t (:weight bold :foreground "#e0c625"))))
 '(minibuffer-message ((t (:foreground "#ffffff"))))

 ;; Helm
 '(helm-selection ((t (:background "#15abc3" :foreground "#001214"))))

 ;; Org
 '(org-hide ((t (:foreground "#00080A"))))
 '(org-level-1 ((t (:inherit rainbow-delimiters-depth-1-face :weight bold))))
 '(org-level-2 ((t (:inherit rainbow-delimiters-depth-2-face :weight bold))))
 '(org-level-3 ((t (:inherit rainbow-delimiters-depth-3-face :weight bold))))
 '(org-level-4 ((t (:inherit rainbow-delimiters-depth-4-face :weight bold))))
 '(org-level-5 ((t (:inherit rainbow-delimiters-depth-5-face :weight bold))))
 '(org-level-6 ((t (:inherit rainbow-delimiters-depth-6-face :weight bold))))
 '(org-level-7 ((t (:inherit rainbow-delimiters-depth-7-face :weight bold))))
 '(org-level-8 ((t (:inherit rainbow-delimiters-depth-8-face :weight bold))))
 ;; '(org-level-1 ((t (:bold t :foreground "dodger blue" :height 1.5))))
 ;; '(org-level-2 ((t (:bold t :foreground "#6ac214" :height 1.2))))
 ;; '(org-level-3 ((t (:bold t :foreground "#edd400" :height 1.1))))
 ;; '(org-level-4 ((t (:bold t :foreground "tomato" :height 1.0))))

 '(org-date ((t (:underline t :foreground "Cyan"))))
 '(org-footnote  ((t (:underline t ))))
 '(org-link ((t (:foreground "#0cd6e4" :background "#001214"))))
 ;; '(org-special-keyword ((t (:foreground "brown"))))
 '(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
 '(org-block ((t (:foreground "#bbbbbc"))))
 '(org-tag ((t (:bold t :foreground "skyblue2"))))
 '(org-quote ((t (:inherit org-block :slant italic))))
 '(org-verse ((t (:inherit org-block :slant italic))))
 '(org-todo ((t (:bold t :foreground "Red"))))
 '(org-done ((t (:bold t :foreground "PaleGreen"))))

 ;; Company-mode
 '(company-tooltip ((t (:inherit default :background "#0b2c2d" :foreground "#0cd6e4"))))
 '(company-scrollbar-bg ((t (:background "#001214"))))
 '(company-scrollbar-fg ((t (:background "#00475a"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face :background "#0cd6e4" :foreground "#0b2c2d"))))
 '(company-tooltip-mouse ((t (:inherit (company-tooltip-selection)))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face :background "#0cd6e4" :foreground "#0b2c2d"))))
 '(company-tooltip-common-selection ((t (:background "#001E21" :foreground "#ffffff"))))
 '(company-tooltip-annotation ((t (:foreground "#f0dfff" :inherit (company-tooltip)))))

 ;; Auto-complete
 '(ac-candidate-face ((t (:inherit default :background "#0b2c2d" :foreground "#0cd6e4"))))
 '(ac-completion-face ((t (:inherit default :background "#0b2c2d" :foreground "#0cd6e4"))))
 '(ac-selection-face ((t (:inherit font-lock-function-name-face :background "#0cd6e4" :foreground "#0b2c2d"))))
 '(ac-candidate-mouse-face ((t (:inherit (ac-selection-face)))))

 ;; Gnu Social
 '(gnu-social-reply-face ((t (:inherit nil :background "#002125" :foreground "cyan"))))
 '(gnu-social-username-face ((t (:underline t :foreground "cyan" :weight bold))))
 '(gnu-social-uri-face ((t (:underline t :weight bold))))

 ;; Ediff
 '(ediff-fine-diff-A ((t (:background "#FF6600"))))
 '(ediff-fine-diff-B ((t (:background "#00815b"))))
 '(ediff-fine-diff-C ((t (:background "#e0c625"))))

 '(ediff-current-diff-C ((t (:background "#00629D"))))

 '(ediff-even-diff-A ((t (:background "#002125" :foreground "#fdf6e3" ))))
 '(ediff-odd-diff-A ((t (:background "#002125" :foreground "cyan" ))))
 '(ediff-even-diff-B ((t (:background "#002125" :foreground "cyan" ))))
 '(ediff-odd-diff-B ((t (:background "#002125" :foreground "cyan" ))))
 '(ediff-even-diff-C ((t (:background "#002125"  :foreground "#839496" ))))
 '(ediff-odd-diff-C ((t (:background "#002125" :foreground "cyan" ))))
 ;; Secondary region
 '(secondary-selection ((((class color) (min-colors 88) (background dark)) (:background "#029cdc" :foreground "#011d2c"))))
 )

;; Icons
(defezimage img:tron-email
  ((:type xpm :file "icons/mail-unread.xpm" :ascent center))
  "Image used for unread mail.")
(defezimage img:tron-feed
  ((:type xpm :file "icons/feed-unread.xpm" :ascent center))
  "Image used for unread feeds.")

(provide-theme 'tron)

;; Local Variables:
;; eval: (rainbow-mode 1)
;; End:

;;; tron-theme.el ends here
