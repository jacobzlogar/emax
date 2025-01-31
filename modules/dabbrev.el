  ;; Use Dabbrev with Corfu!
  (use-package dabbrev
    ;; Swap M-/ and C-M-/
    :bind (("M-/" . dabbrev-completion)
	   ("C-M-/" . dabbrev-expand))
    :config
    (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
    ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
    (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))
