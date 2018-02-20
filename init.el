;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load-library "url-handlers")

(when (display-graphic-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1))
(setq inhibit-splash-screen t)
(setq require-final-newline nil)
(setq fill-column 70)
(setq smooth-scroll-margin 5)
(setq user-mail-address "szpilman@intelie.com.br")
(setq user-full-name "Bernardo Szpilman")
(setq browse-url-browser-function 'browse-url-chrome)

(add-to-list 'load-path (expand-file-name "~/dotfiles/emacs"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/emacs/lib"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/emacs/evil"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/emacs/tree"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/emacs/js"))

;(setq default-abbrev-mode t)
(setq-default indent-tabs-mode nil)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

(load "packages.el")

(require 'evil)
(require 'rainbow-delimiters)
(require 'tree-mode)

(load "paredit.el")

(require 'hiwin)
(hiwin-activate)
(set-face-background 'hiwin-face "color-17")
;(set-face-background 'hiwin-face "color-52")
;(set-face-background 'hiwin-face "color-53")

(require 'column-enforce-mode)
(make-column-rule 80)
(add-hook 'text-mode-hook '80-column-rule)
(add-hook 'prog-mode-hook '80-column-rule)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)

(require 'whitespace)
(setq whitespace-style '(face empty tabs))
;(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
(setq indent-tabs-mode nil)

;(load-theme 'solarized-dark t)
;(load-theme 'zenburn t)
;(load-theme 'purple-haze t)
;(load-theme 'cyberpunk t)
(load-theme 'bubbleberry t)
;(load-theme 'soothe t)

(load "evil/binds.el")
(load "evil/m-binds.el")
(load "org-mode.el")
(load "git.el")
(load "javascript.el")
(load "py.el")

(load "tree/imenu-tree.el")
(load "tree/tags-tree.el")
(load "tree/tree-mode.el")
(load "tree/windata.el")

(require 'dirtree)

(eval-after-load "tree-widget"
  '(if (boundp 'tree-widget-themes-load-path)
       (add-to-list 'tree-widget-themes-load-path "~/dotfiles/emacs/widget/")))
(autoload 'imenu-tree "imenu-tree" "Imenu tree" t)
(autoload 'tags-tree "tags-tree" "TAGS tree" t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b825687675ea2644d1c017f246077cdd725d4326a1c11d84871308573d019f67" "bc40f613df8e0d8f31c5eb3380b61f587e1b5bc439212e03d4ea44b26b4f408a" default)))
 '(linum-format " %7d ")
 '(package-selected-packages
   (quote
    (evil-snipe zenburn-theme w3m soothe-theme solarized-theme smooth-scrolling rjsx-mode rainbow-delimiters purple-haze-theme projectile paredit-everywhere magit load-theme-buffer-local kite key-chord flycheck flx-ido evil-matchit evil-leader erc-hl-nicks cyberpunk-theme column-enforce-mode cider bubbleberry-theme auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
