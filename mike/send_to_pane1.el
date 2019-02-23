(defun my/tmux-send-to-pane-1 ()
  (interactive)
  (shell-command
   (s-concat "tmux send-keys -t 1 '" (thing-at-point 'line t) "'" )))
