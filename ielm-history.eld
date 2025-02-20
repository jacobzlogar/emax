(with-current-buffer spacetraders/buffer
  (let ((res (json-parse-buffer)))
    (gethash "data" res)))
(with-current-buffer spacetraders/buffer
  (let ((res (json-parse-buffer)))
    (prinl gethash "data" res)))
(setq buf (get-buffer "*The Spice Must Flow*"))
(json-parse-buffer)
(json-parse-buffer buf)
