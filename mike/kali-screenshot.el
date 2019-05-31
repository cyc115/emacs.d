;; author: Mike Chen
;; date 2019-05-20
;;
(defun my/org-screenshot-kali ()
  (interactive)
  (shell-command "~/.emacs.d/mike/_kali-screenshot.sh")
  )

(defun my/org-screenshot-clipboard ()
  (interactive)
  (shell-command "gnome-screenshot -a -c")
  )
