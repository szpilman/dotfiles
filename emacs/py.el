(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(eval-after-load "company"
  '(add-to-list 'company-backends '(company-anaconda :with company-capf)))
