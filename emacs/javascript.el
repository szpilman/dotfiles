;(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("components\\/.*\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
(add-hook 'js3-mode-hook #'global-flycheck-mode)

(require 'gulpjs)
;;(require 'tern)
(require 'company-tern)
;M-: (delete-process "Tern") RET
;# must add tmpl ~/dotfiles/.tern-project to prj root
(autoload 'tern-mode "tern.el" nil t)
;(add-hook 'rjsx-mode-hook (lambda () (tern-mode t)))
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-tern))
(setq company-tern-meta-as-single-line t)
(setq company-tooltip-align-annotations t)
;(setq company-tern-property-marker " <p>")
;(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;(setq js2-basic-offset 2)

(add-hook 'js3-mode-hook
          (lambda ()
            (setq js3-auto-indent-p t
                  js3-curly-indent-offset 0
                  js3-enter-indents-newline t
                  js3-expr-indent-offset 2
                  js3-indent-on-enter-key t
                  js3-lazy-commas t
                  js3-lazy-dots t
                  js3-lazy-operators t
                  js3-paren-indent-offset 2
                  js3-square-indent-offset 4)
            (linum-mode 1)))

          ;; https://github.com/Fuco1/smartparens/issues/239
          ;; (defadvice js3-enter-key (after fix-sp-state activate)
          ;;   (setq sp-last-operation 'sp-self-insert))

          ;; (sp-local-pair 'js3-mode
          ;;                "{"
          ;;                nil
          ;;                :post-handlers
          ;;                '((ome-create-newline-and-enter-sexp js3-enter-key))))
;(add-to-list 'ac-modes 'js3-mode)

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'js3-mode-hook (lambda () (tern-mode t)))
(setq tern-command (cons (executable-find "tern") '()))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(add-hook 'js2-mode-hook
            (lambda ()
              (setq js2-basic-offset 2)))
(setq js2-bounce-indent-p t)
;(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;(defun grunt-find-root ()
;  (let ((root (substring (shell-command-to-string "git rev-parse --show-toplevel") 0 -1)))
;    (if (file-exists-p (concat root "/Gruntfile.js"))
;      root
;      (concat root "/src/main/webapp"))))

;(defun grunt-cmd ()
;  (concat "cd " (grunt-find-root) " && grunt --no-color"))

;(defun grunt ()
;  "Run grunt"
;  (interactive)
;  (let* ((grunt-buffer (get-buffer-create "*grunt*"))
;         (result (call-process-shell-command (grunt-cmd) nil grunt-buffer t))
;         (output (with-current-buffer grunt-buffer (buffer-string))))
;    (cond ((zerop result)
;           (message "Grunt completed without errors"))
;          (t
;           (message nil)
;           (split-window-vertically)
;           (set-window-buffer (next-window) grunt-buffer)))))

;(add-hook 'js2-mode-hook
;          '(lambda ()
;             (define-key evil-insert-state-map (kbd "C-l")
;               '(lambda () (interactive)
;                  (if (string-match "^[ \t]*console." (thing-at-point 'line))
;                    (progn
;                      (insert "JSON.stringify()")
;                      (backward-char 1))
;                    (progn
;                      (insert "console.log(');")
;                      (backward-char 2)))))
;             (define-key evil-normal-state-map (kbd "C-m") 'grunt)))
