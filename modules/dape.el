;; (setq dape-adapter-dir "~/.emacs.d/debug-adapters/")
;; (defun dape-jest/find-file-buffer-default ()
;;   "Read filename at project root, defaulting to current buffer. Return vector of jest args to run said file"
;;   (let ((file (dape-buffer-default)))
;;     (if file
;;         `["--runInBand" "--no-coverage" ,file]
;;       (user-error "No file found"))))

;; (defun dape-jest/ensure (config)
;;   "Ensure node is available, jest is installed, that the dapDebugServer is installed"

;;   (dape-ensure-command config)
;;   (let ((cwd (dape-cwd))
;;         (js-debug-file (expand-file-name
;;                         (dape--config-eval-value (car (plist-get config 'command-args)))
;;                         (dape--config-eval-value (plist-get config 'command-cwd))))
;;         (node-jest-file (expand-file-name
;;                         (dape--config-eval-value (plist-get config :program))
;;                         (dape--config-eval-value (plist-get config :cwd)))))
;;     (unless (file-exists-p js-debug-file)
;;       (user-error "Debug server file %S does not exist" js-debug-file))
;;     (unless (file-exists-p node-jest-file)
;;       (user-error "Jest executable not found at %S" node-jest-file))))


;; (add-to-list 'dape-configs
;;              `(jest
;;                modes (js-mode js-ts-mode typescript-mode)
;;                ensure dape-jest/ensure
;;                command "node"
;;                command-cwd dape-command-cwd
;;                command-args (,(expand-file-name
;;                                (file-name-concat dape-adapter-dir
;;                                                  "js-debug"
;;                                                  "src"
;;                                                  "dapDebugServer.js"))
;;                              :autoport)
;;                port :autoport
;;                fn dape-config-autoport
;;                :type "pwa-node"
;;                :cwd dape-cwd
;;                :env (:VAR1 "some value" :VAR2 "another value")
;;                :program "node_modules/.bin/jest"
;;                :args dape-jest/find-file-buffer-default
;;                :outputCapture "console"
;;                :sourceMapRenames t
;;                :pauseForSourceMap nil
;;                :autoAttachChildProcesses t
;;                :console "internalConsole"
;;                :outputCapture "std"
;;                :killBehavior "forceful"))
