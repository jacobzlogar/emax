(setq mode-line-format nil)
(kill-local-variable 'mode-line-format)
(force-mode-line-update)

(defface modeline/bold-shadow
  `((t :inherit bold :foreground ,(face-foreground 'shadow)))
  "Face for bold+shadow")

(defvar modeline/symbol
  (propertize "λ" 'face 'modeline/bold-shadow))

(put 'modeline/symbol 'risky-local-variable-p t)

(defun modeline/file ()
  (file-relative-name buffer-file-name (project-root (project-current))))

(defun modeline/project ()
  (if (project-current)
      (propertize (project-name (project-current)) 'face 'bold) ""))

(defun modeline/vcs ()
  (if (project-current)
      (format "%s %s" (propertize "" 'face 'shadow) (vc-git--symbolic-ref (buffer-file-name))) ""))

(defun modeline/mode ()
  (format "%s %s" (nerd-icons-icon-for-file (buffer-file-name)) (symbol-name major-mode)))

(defun modeline/padding (item &optional dir)
  (cond ((eq dir 'left) (format " %s" item))
        ((eq dir 'right) (format "%s " item))
        (t (format "%s" item))))

(defun modeline/divider ()
  (if (> (string-width (modeline/file) 0))
      (propertize "ᛥ" 'face 'modeline/bold-shadow) ""))

(defun modeline/eglot ()
  (if (eglot-managed-p)
      (propertize "ᛇ" 'face 'shadow) ""))

(setq-default mode-line-format
	      '((:eval (modeline/padding modeline/symbol 'left))
		(:eval (modeline/padding (modeline/project) 'left))
		;;(:eval (modeline/padding (modeline/divider) 'left))
		(:eval (modeline/padding (modeline/file) 'left))
		(:eval (modeline/padding (modeline/vcs) 'left))
		(:eval (propertize "%+" 'face 'shadow))
		" Line: %l"
		(:eval (modeline/padding (modeline/eglot) 'left))
		(:eval (propertize
			" " 'display
			`((space :align-to (- (+ right right-fringe right-margin)
					      ,(+ 2 (string-width (modeline/mode))))))))
		(:eval (modeline/padding (modeline/mode) 'right))))
