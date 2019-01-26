(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun my/_rails-test-str ()
  ;;; return rspec command on current file: line
  (s-concat "bundle exec rspec " (show-file-name) ":" (number-to-string (line-number-at-pos))))

(defun my/rails-test-command ()
  ;;; returns rspec test command in tmux pane
  ;;; eg `'clear; echo -e 'bundle exec rspec spec/authy_client/allowed_host_resource_spec.rb:10'; bundle exec rspec spec/authy_client/allowed_host_resource_spec.rb:10`
  (s-concat "clear; echo -e '" (my/_rails-test-str) "'; " (my/_rails-test-str)))

(defun my/_tmux-pane-to-run-rails-test ()
  ;;; return the pane id to run rails test in
  ;;; ruby > zsh > bash
  (progn
    ;; ruby
    (setq tmux_pane_id (shell-command-to-string "tmux list-panes -F '#{pane_index} #{pane_current_command}' | grep ruby | awk '{printf $1;exit}'"))
    ;; zsh
    (when (string= tmux_pane_id "")
      (setq tmux_pane_id (shell-command-to-string "tmux list-panes -F '#{pane_index} #{pane_current_command}' | grep zsh | awk '{printf $1;exit}'")))
    ;; bash
    (when (string= tmux_pane_id "")
      (setq tmux_pane_id (shell-command-to-string "tmux list-panes -F '#{pane_index} #{pane_current_command}' | grep bash | awk '{printf $1;exit}'"))))
  tmux_pane_id
  )

(defun my/rails-test-line-at-cursor ()
  (interactive)
  (shell-command
   (s-concat "tmux send-keys -t " (my/_tmux-pane-to-run-rails-test) " '" (my/_rails-test-str) "' Enter" )))
