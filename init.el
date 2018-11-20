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

;; ------------------------- from original emacs setup --------------------------------------------------

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

;; ------------------------- tools / libraries / custom setups ------------------------------------------

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

;; auto-reload files from filesystem on git checkout branch
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)

;; projectile mode setup
(projectile-mode +1)

;; tab-nine autocomplete
(use-package company-tabnine :ensure t)
(add-to-list 'company-backends #'company-tabnine)

;; company mode setup
(setq company-idle-delay 0)
;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-number t)
;; Use the tab-and-go frontend.
;; Allows TAB to select and complete at the same time.
(company-tng-configure-default)
(setq company-frontends
      '(company-tng-frontend
        company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))

;; switch window mode
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-qwerty-shortcuts
      '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))
(setq switch-window-shortcut-appearance 'asciiart)

;; load timer
(load-user-file "mike/timer.el")

;; load textmate mode to enhence goto functions see: https://github.com/defunkt/textmate.el
(load-user-file "mike/textmate.el")
(require 'textmate)
(textmate-mode)
(global-set-key (kbd "M-p") 'textmate-goto-file)
(global-set-key (kbd "M-]") 'textmate-goto-symbol)


;; accomodate dropbox
(setq desktop-base-file-name (concat ".desktop." (system-name)))


;; create dedicated window with c-c t
(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

;; make buffer sticky
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

;; display time inn the status line
(display-time-mode 1)

;; open links from eamcs in the emacs embedded w3m browser
(setq browse-url-browser-function  'w3m-goto-url-new-session)


;; ---------------------------------- org mode ------------------------------------------------------

;; TODO keywords
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
(defun my/org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file)
  (org-map-entries 'org-archive-subtree "/CANCELLED" 'file)
  )

;; org-mode sort keywords
(setq org-todo-sort-order '( "CANCELLED" "DONE" "NOTE" "PAUSED" "IDEA" "TODO" "WIP"))

(defun my/user-todo-sort (a b)
  "Sort todo based on which I want to see first"
  (when-let ((state-a (get-text-property 14 'todo-state a))
             (state-b (get-text-property 14 'todo-state b))
             (cmp (--map (cl-position-if (lambda (x)
                                           (equal x it))
                                         org-todo-sort-order)
                         (list state-a state-b))))
    (cond ((apply '> cmp) 1)
          ((apply '< cmp) -1)
          (t nil))))
(setq org-agenda-cmp-user-defined 'my/user-todo-sort)
(setq org-agenda-cmp-user-defined 'my/user-todo-sort)



;; ------------------------- language specific sections below ---------------------------------------

;; ----- ruby -----

;; load ruby robocop fmter
(load-user-file "mike/robocopfmt.el")
;; (add-hook 'ruby-mode-hook #'rubocopfmt-mode)

;; ----- wepy -----

;; minapp development setups
(setq auto-mode-alist (append '(("\\.wpy$" . vue))
                              auto-mode-alist))
