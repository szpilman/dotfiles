(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(defvar my-packages '(evil
                      evil-leader
                      evil-matchit
                      evil-snipe
                      key-chord
                      org
                      w3m
                      xterm-frobs
                      ; project & file mgmt
                      company
                      projectile
                      dirtree
                      flx-ido
                      magit
                      ; movement
                      ;expand-region
                      smooth-scrolling
                      hiwin
                      ; lisp
                      paredit
                      paredit-everywhere
                      rainbow-delimiters
                      ;hy-mode
                      ; js
                      ;rjsx-mode
                      js3-mode
                      js2-mode
                      nodejs-repl
                      skewer-mode
                      ;ac-js2
                      ;auto-complete
                      ;web-mode
                      kite
                      tern
                      company-tern
                      ; python
                      anaconda-mode
                      company-anaconda
                      ; clojure
                      clojure-mode
                      cider
                      ;ac-nrepl
                      ; general workflow
                      column-enforce-mode
                      ;auto-complete
                      flycheck
                      ; misc
                      erc-hl-nicks
                      ; color theming
                      load-theme-buffer-local
                      solarized-theme
                      zenburn-theme
                      soothe-theme
                      purple-haze-theme
                      cyberpunk-theme
                      bubbleberry-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
