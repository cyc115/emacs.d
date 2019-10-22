(defun my/searchsploit-find (service)
  "search selected on searchsploit"
  (interactive "service name: ")
  (shell-command (concat"searchsploit --color " service)))


(defun my/searchsploit-view (service)
  "search selected on searchsploit"
  (interactive)
  (setq l (thing-at-point 'line t))

  (shell-command (concat"searchsploit --color " service)))
