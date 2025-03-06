(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (set-face-attribute 'git-gutter-fr:added nil :background nil)
  (set-face-attribute 'git-gutter-fr:modified nil :background nil)
  (set-face-attribute 'git-gutter-fr:deleted nil :background nil)
  (define-fringe-bitmap 'git-gutter-fr:added [24 24 24 255 24 24 24] nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified [24 24 24 24 24 24 24 24 0 24 24 24] nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted [129 66 36 24 36 66 129] nil nil 'center))
