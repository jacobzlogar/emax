(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous)))

(use-package corfu-candidate-overlay
  :after corfu
  :config
  ;; enable corfu-candidate-overlay mode globally
  ;; this relies on having corfu-auto set to nil
  (corfu-candidate-overlay-mode +1))
