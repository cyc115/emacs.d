;; here we use C-c b to send cmd to pane 1 and C-c w to send to pane 2


(defun my/tmux-swap-pane-1-with-0 ()
  (interactive)
  (shell-command "tmux swap-pane -s0 -t1")
  (shell-command "tmux select-pane -t2")
  )

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

(defun my/tmux-send-to-pane-2 ()
  (interactive)
  (shell-command
   (s-concat "tmux send-keys -t 2 '" (thing-at-point 'line t) "'" ))
  )

(global-set-key (kbd "C-c b") 'my/tmux-send-to-pane-1)
(global-set-key (kbd "C-c w") 'my/tmux-send-to-pane-2)
(global-set-key (kbd "C-c k") 'my/tmux-send-C-c-to-pane-1)
(global-set-key (kbd "C-c d") 'my/tmux-send-C-d-to-pane-1)


(defun my/send-selection-1 ()
  "show content of current region"
  (interactive)
  (let (pos1 pos2 bds)
    (if (use-region-p)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))

    ;;(message "Region: %s" (buffer-substring pos1 pos2) )
    (shell-command
     (s-concat "tmux send-keys -t 1 '" (buffer-substring pos1 pos2) "'" ))
    ))


(defun my/send-selection-2 ()
  "show content of current region"
  (interactive)
  (let (pos1 pos2 bds)
    (if (use-region-p)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))

    ;;(message "Region: %s" (buffer-substring pos1 pos2) )
    (shell-command
     (s-concat "tmux send-keys -t 2 '" (buffer-substring pos1 pos2) "'" ))
    ))

