(define-derived-mode vue-mode web-mode "Vue Mode"
  "Major mode for .vue files"
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 0))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
