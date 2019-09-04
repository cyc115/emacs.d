;; use super+i,j,k,l to navigate the opened windows

(defun my/exwm-focus-window-direction (dir)
  (lambda ()
  (if (window-in-direction dir)
      (select-window (window-in-direction dir))
    (other-window 1))
  ))

(defun my/exwm-focus-window-right ()
  (interactive)
  (if (window-in-direction 'right)
      (select-window (window-in-direction 'right))
    (other-window 1))
  )

(defun my/exwm-focus-window-left ()
  (interactive)
  (if (window-in-direction 'left)
      (select-window (window-in-direction 'left))
    (other-window -1))
  )

(define-key (current-global-map) (kbd "s-j") 'my/exwm-focus-window-left)
(define-key (current-global-map) (kbd "s-l") 'my/exwm-focus-window-right)
