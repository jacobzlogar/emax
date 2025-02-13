(setq org-src-window-setup 'current-window)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 0)
(add-hook 'org-mode-hook 'electric-indent-mode)
(setq project-switch-commands 'project-find-file)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(defun nasm/compile (instr &optional bits asm)
  (if (not bits)
    (setq bits 16))
  (let* ((temp-asm "/tmp/test.asm")
	 (temp-binary "/tmp/test")
	 (nasm (format "nasm %s -o %s | cat %s" temp-asm temp-binary temp-binary)))
    (with-temp-file temp-asm
      (insert (format "bits %s\n" bits))
      (insert instr)
      (shell-command-to-string nasm))))
