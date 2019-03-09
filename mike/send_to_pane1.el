(defun my/tmux-send-to-pane-1 ()
  (interactive)
  (shell-command
   (s-concat "tmux send-keys -t 1 '" (thing-at-point 'line t) "'" )))

(defun my/tmux-send-C-c-to-pane-1 ()
  (interactive)
  (shell-command
   "tmux send-keys -t 1 C-c"
   ))

(defun my/tmux-send-C-d-to-pane-1 ()
  (interactive)
  (shell-command
   "tmux send-keys -t 1 C-d"
   ))
