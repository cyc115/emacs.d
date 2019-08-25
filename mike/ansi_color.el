(defun my/show-ansi-color ()
  "show ansi color in buffer"
  (interactive)
  (load-library  "ansi-color")
  (ansi-color-apply-on-region (point-min) (point-max))
 )
