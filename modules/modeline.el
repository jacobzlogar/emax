(setq flymake-modeline-counter-format '(flymake-mode-line-error-counter flymake-mode-line-warning-counter
 flymake-mode-line-note-counter))

(setq mode-line-format nil)
(kill-local-variable 'mode-line-format)
(force-mode-line-update)

(defface modeline/bold-shadow
  `((t :inherit bold :foreground ,(face-foreground 'shadow)))
  "Face for bold+shadow")

(defvar modeline/symbol
  (propertize "λ" 'face '(:background ,(face-background 'mode-line) :inherit 'bold)))

(put 'modeline/symbol 'risky-local-variable-p t)

(defun modeline/file ()
  (if (and (consp (project-current)) (buffer-file-name))
      (file-relative-name buffer-file-name (project-root (project-current))) (buffer-name)))

(defun modeline/project ()
  (if (project-current)
      (propertize (project-name (project-current)) 'face 'bold) ""))

(defun modeline/vcs ()
  (if (and (project-current) (mode-line-window-selected-p))
      (format "%s %s" (propertize "" 'face 'shadow) (vc-git--symbolic-ref (buffer-file-name))) ""))

(defun modeline/mode ()
  (format "%s %s" (propertize
		   (symbol-name major-mode)
		   'face 'modeline/bold-shadow)
		   (nerd-icons-icon-for-file buffer-file-name)))

(defun modeline/padding (item &optional dir)
  (cond ((eq dir 'left) (format " %s" item))
        ((eq dir 'right) (format "%s " item))
        (t (format "%s" item))))

(defun modeline/eglot ()
  (if (eglot-managed-p)
      (propertize "ᛥ" 'face '(:foreground "green")) ""))

(setq-default mode-line-format
	      '((:eval (modeline/padding (modeline/project) 'left))
		vc-mode
		(:eval (propertize "%+" 'face 'modeline/bold-shadow))
		" Line: %l"
		(:eval (modeline/padding (modeline/eglot) 'left))
		(flymake-mode-line-exception flymake-mode-line-counters)
		(:eval (propertize
			" " 'display
			`((space :align-to (- (+ right right-fringe right-margin)
					      ,(+ 2 (string-width (modeline/mode))))))))
		(:eval (modeline/padding (modeline/mode) 'right))))

(setq-default header-line-format
	      '((:eval (if (mode-line-window-selected-p) (modeline/padding modeline/symbol 'left)))
		(:eval (if (mode-line-window-selected-p) (modeline/padding (modeline/file) 'left)))))
