(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(require 'gulpjs)
;;(require 'tern)
(require 'company-tern)
;M-: (delete-process "Tern") RET
;# must add tmpl ~/dotfiles/.tern-project to prj root
(add-to-list 'load-path "~/qfork/ternjs/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'rjsx-mode-hook (lambda () (tern-mode t)))
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-tern))
(setq company-tern-meta-as-single-line t)
;(setq company-tooltip-align-annotations t)
;(setq company-tern-property-marker " <p>")
;(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;(setq js2-basic-offset 2)

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
