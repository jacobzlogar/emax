(setq mode-line-format nil)
(kill-local-variable 'mode-line-format)
(force-mode-line-update)

(defface modeline/bold-shadow
  `((t :inherit bold :foreground ,(face-foreground 'shadow)))
  "Face for bold+shadow")

(defvar modeline/symbol
  (propertize "λ" 'face 'modeline/bold-shadow))

(put 'modeline/symbol 'risky-local-variable-p t)

(defun modeline/project ()
  (project-root (project-current)))

(defun modeline/padding (item &optional dir)
  (cond ((eq dir 'left) (format " %s" item))
        ((eq dir 'right) (format "%s " item))
        (t (format " %s " item))))

(setq-default mode-line-format
	      '(
		(:eval (modeline/padding modeline/symbol 'left))
		(:eval (modeline/padding (modeline/project)))
		(:eval (propertize "|" 'face 'modeline/bold-shadow))
		(:eval (modeline/padding (buffer-name) 'left))
		))
