;; need rework as this is not portable
(defun my/org-screenshot-kali ()
  (interactive)
  "take a screenshot with gnome-screenshot"
  (shell-command "~/.emacs.d/mike/_kali-screenshot.sh")
  )

(defun my/org-screenshot-clipboard ()
  (interactive)
  (shell-command "gnome-screenshot -a -c")
  )

;; take screenshot from emacs on kali
(global-set-key (kbd "C-c SPC ") 'my/org-screenshot-kali)
(global-set-key (kbd "C-c ] ") 'my/screenshot-clipboard)
