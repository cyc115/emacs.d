;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Increase gc-cons-threshold, depending on your system you may set it back to a
;; lower value in your dotfile (function `dotspacemacs/user-config')
(setq gc-cons-threshold 100000000)

(defconst spacemacs-version         "0.200.13" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "24.4" "Minimal version of Emacs.")

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (load-file (concat (file-name-directory load-file-name)
                     "core/core-load-paths.el"))
  (require 'core-spacemacs)
  (spacemacs/init)
  (configuration-layer/sync)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)
  (require 'server)
  (unless (server-running-p) (server-start)))


;; load-user-file
;; load a custom .el lib from ~/.emacs.d/ directory
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; custom setups
(set-default 'truncate-line t)
(set-default 'word-wrap t)
(desktop-save-mode 1)

;; enable mouse scrolling in osx
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))


;; org-mode setups
(setq org-todo-keywords
      '((sequence "IDEA" "TODO" "WIP" "PAUSED" "|" "DONE" "CANCELLED" "NOTE"  )))

(setq org-todo-keyword-faces
      '(
        ("IDEA" . (:foreground "pink" :weight bold))
        ("WIP" . (:foreground "blue" :weight bold))
        ("CANCELLED" . (:foreground "green" :weight bold))
        ("PAUSED" . (:foreground "yellow" :weight bold))
        ))

(setq org-startup-indented t)

;; org-mode archive-done-tasks
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file)
  (org-map-entries 'org-archive-subtree "/CANCELLED" 'file)
  )


;; projectile mode setup
(projectile-mode +1)

;; minapp development setups
(setq auto-mode-alist (append '(("\\.wpy$" . vue))
                              auto-mode-alist))

;; switch window mode
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-qwerty-shortcuts
      '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))
(setq switch-window-shortcut-appearance 'asciiart)

;; load timer
(load-user-file "mike/timer.el")

;; load ruby robocop fmter
(load-user-file "mike/robocopfmt.el")
(add-hook 'ruby-mode-hook #'rubocopfmt-mode)

;;; accomodate dropbox
(setq desktop-base-file-name (concat ".desktop." (system-name)))
