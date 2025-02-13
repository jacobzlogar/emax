;; (setq dape-adapter-dir "~/.emacs.d/debug-adapters/")
;; (defun dape-jest/find-file-buffer-default ()
;;   "Read filename at project root, defaulting to current buffer. Return vector of jest args to run said file"
;;   (let ((file (dape-buffer-default)))
;;     (if file
;;         `["--runInBand" "--no-coverage" ,file]
;;       (user-error "No file found"))))
