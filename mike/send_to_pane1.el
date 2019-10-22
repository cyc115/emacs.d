;; here we use C-c b to send cmd to pane 1 and C-c w to send to pane 2

(defun silent-command (cmd)
  (message cmd)
  (call-process-shell-command
   cmd
   nil "*Shell Command Output*" t
   ))

(defun my/replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun my/tmux-send-key (cmd)
  "send cmd to tmux"
  (interactive)
  (silent-command
   (s-concat "tmux send-keys " cmd ))
  )

(defun my/tmux-swap-pane-1-with-0 ()
  "swap tmux pane 1 with pane 0"
  (interactive)
  (silent-command "tmux swap-pane -s1 -t2")
  (silent-command "tmux select-pane -t3")
  )

(defun my/tmux-send-to-pane-1 ()
  "send current line to pane 1"
  (interactive)
  (my/tmux-send-key
   (s-concat "-t 1 \"" (my/replace-in-string "\"" "\\\"" (thing-at-point 'line t)) "\"" )))

(defun my/tmux-send-selection-1 ()
  "send selected region to pane 1"
  (interactive)
  (let (pos1 pos2 bds)
    (if (use-region-p)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))

    ;;(message "Region: %s" (buffer-substring pos1 pos2) )
    (silent-command
     (s-concat "tmux send-keys -t 1 \""
               (my/replace-in-string "\"" "\\\"" (buffer-substring pos1 pos2))
               "\"" ))))


(defun my/tmux-send-selection-2 ()
  "send selected region to pane 2"
  (interactive)
  (let (pos1 pos2 bds)
    (if (use-region-p)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))

    ;;(message "Region: %s" (buffer-substring pos1 pos2) )
    (silent-command
     (s-concat "tmux send-keys -t 2 \""
               (my/replace-in-string "\"" "\\\"" (buffer-substring pos1 pos2))
               "\"" ))))

(defun my/tmux-toggle-full-pane ()
  "show content of current region"
  (interactive)
  (shell-command "tmux sned-keys -t 1 tmux resize-pane -Z" ))

(defun my/tmux-send-C-c-to-pane-1 ()
  (interactive)
  (my/tmux-send-key "-t 1 C-c" )
  )

(defun my/tmux-send-C-d-to-pane-1 ()
  (interactive)
  (my/tmux-send-key "-t 1 C-d" )
  )

(defun my/tmux-send-q ()
  (interactive)
  (my/tmux-send-key "q" )
  )

(defun my/tmux-send-to-pane-2 ()
  (interactive)
  (my/tmux-send-key
   (s-concat "-t 2 \"" (my/replace-in-string "\"" "\\\"" (thing-at-point 'line t)) "\"" )))

(defun my/tmux-send-pageup()
  (interactive)
  (my/tmux-send-key "PageUp" )
  )

(defun my/tmux-send-pagedown()
  (interactive)
  (my/tmux-send-key "PageDown" )
  )

(global-set-key (kbd "C-c b") 'my/tmux-send-to-pane-1)
(global-set-key (kbd "C-c w") 'my/tmux-send-to-pane-2)
(global-set-key (kbd "C-c B") 'my/tmux-send-selection-1)
(global-set-key (kbd "C-c W") 'my/tmux-send-selection-2)

(global-set-key (kbd "C-c k") 'my/tmux-send-C-c-to-pane-1)
(global-set-key (kbd "C-c d") 'my/tmux-send-C-d-to-pane-1)
(global-set-key (kbd "C-c q") 'my/tmux-send-q)


(global-set-key (kbd "C-c s") 'my/tmux-swap-pane-1-with-0)

(global-set-key (kbd "C-c <prior>") 'my/tmux-send-pageup)
(global-set-key (kbd "C-c <next>") 'my/tmux-send-pagedown)


