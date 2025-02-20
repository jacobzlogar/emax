(provide 'ui)
;;(load-theme 'doom-gruvbox)
  ;; Make customisations that affect Emacs faces BEFORE loading a theme
  ;; (any change needs a theme re-load to take effect).

  ;; If you like two specific themes and want to switch between them, you
  ;; can specify them in `ef-themes-to-toggle' and then invoke the command
  ;; `ef-themes-toggle'.  All the themes are included in the variable
  ;; `ef-themes-collection'.
  ;; (setq ef-themes-to-toggle '(ef-frost ef-symbiosis))

  ;; (setq ef-themes-headings ; read the manual's entry or the doc string
  ;; 	'((0 variable-pitch light 1.9)
  ;; 	  (1 variable-pitch light 1.8)
  ;; 	  (2 variable-pitch regular 1.7)
  ;; 	  (3 variable-pitch regular 1.6)
  ;; 	  (4 variable-pitch regular 1.5)
  ;; 	  (5 variable-pitch 1.4) ; absence of weight means `bold'
  ;; 	  (6 variable-pitch 1.3)
  ;; 	  (7 variable-pitch 1.2)
  ;; 	  (t variable-pitch 1.1)))

  ;; They are nil by default
  ;; (setq ef-themes-mixed-fonts t
  ;; 	ef-themes-variable-pitch-ui t)

  ;; Disable all other themes to avoid awkward blending:
  ;; (mapc #'disable-theme custom-enabled-themes)

  ;; Load the theme of choice:
  ;;(load-theme 'ef-night :no-confirm)

  ;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
  (ef-themes-select 'ef-duo-dark)

  ;; The themes we provide are recorded in the `ef-themes-dark-themes',
  ;; `ef-themes-light-themes'.

  ;; We also provide these commands, but do not assign them to any key:
  ;;
  ;; - `ef-themes-toggle'
  ;; - `ef-themes-select'
  ;; - `ef-themes-select-dark'
  ;; - `ef-themes-select-light'
  ;; - `ef-themes-load-random'
  ;; - `ef-themes-preview-colors'
  ;; - `ef-themes-preview-colors-current'

;; use git-gutter-mode
(setq git-gutter-mode t)
(setq git-gutter-fringe t)
(which-key-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(winner-mode 1)
(setq blink-cursor-mode nil)
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  (let ((bg-color (face-attribute 'menu :background))
	(fg-color (face-attribute 'default :foreground)))
    (custom-set-faces
     `(org-block-begin-line ((t (:foreground ,fg-color :background ,bg-color))))
     `(org-block-end-line ((t (:foreground ,fg-color :background ,bg-color))))))

(setq max-mini-window-height 1)
(set-fringe-style 20)
(let ((bg-color (face-attribute 'default :background)))
  (custom-set-faces
   `(flymake-error ((t :underline nil)))
   `(eglot-highlight-symbol-face ((t :inherit 'underline :weight normal)))
   `(eldoc-box-border ((t :background ,(face-attribute 'highlight :background))))
   `(line-number ((t (:background ,bg-color))))
   `(fringe ((t (:background ,bg-color))))))

(use-package origami
  :init
  (global-origami-mode))
(use-package kind-icon
  :ensure t
  :after corfu
  ;:custom
  ; (kind-icon-blend-background t)
  ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (setq kind-icon-use-icons nil)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
