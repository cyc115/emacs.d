(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))

;; enable global snippets
;; have a fundamental-mode directory alongside whatever other snippet directories
(add-hook 'yas-minor-mode-hook
          (lambda ()
            (yas-activate-extra-mode 'fundamental-mode)))

(add-hook 'yas-minor-mode-hook
          (lambda ()
            (global-set-key (kbd "TAB") 'yas-expand)))
