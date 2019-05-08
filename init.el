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
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; ------------------------- emacs windows manager related setups  --------------------------------------------------
(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(1 "eDP-1" 2 "DP-2"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output eDP-1 --output DP-2 --above eDP-1 --auto")))
(exwm-randr-enable)
(exwm-config-default)
(define-key exwm-mode-map [?\C-q] 'exwm-input-release-keyboard)


(symon-mode)
(add-hook 'after-init-hook #'fancy-battery-mode)



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

;; load-user-file loads a custom .el file from the provided directory
;; eg. (load-user-file "mike/timer.el")
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
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line) ;
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; allow neotree resize
(setq neo-window-fixed-size nil)


;; config yas snippet
(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))

;; enable global snippets
;; have a fundamental-mode directory alongside whatever other snippet directories
(add-hook 'yas-minor-mode-hook
          (lambda ()
            (yas-activate-extra-mode 'fundamental-mode)))

(add-hook 'yas-minor-mode-hook
          (lambda ()
            (global-set-key (kbd "TAB") 'yas-expand))
          )




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

;; restclient
(load-user-file "mike/restclient.el")
(load-user-file "mike/ob-restclient.el")
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (restclient . t)
   (ruby . t)
   (shell . t)
   ))

;; load textmate mode to enhence goto functions see: https://github.com/defunkt/textmate.el
(load-user-file "mike/textmate.el")
(require 'textmate)
(textmate-mode)
(global-set-key (kbd "M-p") 'textmate-goto-file)
(global-set-key (kbd "M-]") 'textmate-goto-symbol)

;; load ace-jump
(load-user-file "mike/ace-jump.el")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "emacs quick move minor mode"
  t)
(global-unset-key "\M-c")
(define-key global-map (kbd "M-c") 'ace-jump-mode)

;; global linum
(if (version< "26" emacs-version)
    ;; only in emacs26
    (global-display-line-numbers-mode)
  ;; emacs 23 and higher
  (global-linum-mode 1))

;; Accomodate dropbox
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


;; ------- eww -----------

;; eww browser open links in separate buffer by default
;; Auto-rename new eww buffers, so when running `M-x eww` a new buffer is created
(defun xah-rename-eww-hook ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
(add-hook 'eww-mode-hook #'xah-rename-eww-hook)

;; C-u M-x eww will force a new eww buffer
(defun modi/force-new-eww-buffer (orig-fun &rest args)
  "When prefix argument is used, a new eww buffer will be created,
regardless of whether the current buffer is in `eww-mode'."
  (if current-prefix-arg
      (with-temp-buffer
        (apply orig-fun args))
    (apply orig-fun args)))
(advice-add 'eww :around #'modi/force-new-eww-buffer)

;; set google as default search engine
(setq eww-search-prefix "https://www.google.com/search?q=")

;; ---------------------------------- org mode ------------------------------------------------------

;; set TODO keyword highlights
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
  (org-map-entries 'org-archive-subtree "/CANCELLED" 'file))


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

;; capture template
(setq org-capture-templates '(("w" "Work [inbox]" entry
                               (file+headline "~/org/inbox.org" "Work")
                               "* TODO %i%? \n %U")
                              ("p" "Personal [inbox]" entry
                               (file+headline "~/org/inbox.org" "Personal")
                               "* TODO %i%? \n %U")
                              ("k" "PWK lab todo" entry
                               (file+headline "~/org/pwk_course/exploit.org" "5")
                               "* TODO %i%? \n %U")
                              ("n" "PWK lab notes" entry
                               (file+headline "~/org/pwk_course/exploit.org" "5")
                               "* NOTE %i%? \n %U")
                              ("f" "PWK lab flags [inbox]" entry
                               (file+headline "~/org/pwk_course/exploit.org" "5")
                               "* NOTE %i%? :FLAG:\n %U")
                              ) )
(global-set-key (kbd "M-q ") 'org-capture)


;; org custom agenda views
(setq org-agenda-custom-commands
      '(
        ("k" "pwk lab relate tasks"
         (
          (tags-todo "pwk")
          ))
        ("w" "Agenda and work related"
         ((agenda "")
          (tags-todo "work")
          ))
        ("p" "Agenda and personal related tasks"
         ((agenda "")
          (tags-todo "personal")
          ))
        ("d" "daily and review"
         (
          (tags-todo "inbox")
          ;; TODO overdue
          ;; TODO inbox
          ;; TODO scheduled today for work
          ;; TODO scheduled today for personal
          ))
        ("t" "test"
         ((agenda "")
          (tags-todo "work")
          (todo "IDEA")
          ))
        ))

;; helm-org-rifle : quickly search through org files
;; https://github.com/alphapapa/helm-org-rifle
;; Helm commands: show results in a Helm buffer
;; helm-org-rifle: Results from all open Org buffers
;; helm-org-rifle-current-buffer: Show results from current buffer
;; helm-org-rifle-directories: Show results from selected directories; with prefix, recursively
;; helm-org-rifle-org-directory: Show results from Org files in org-directory

;; Occur commands: show results in an occur-like, persistent buffer
;; helm-org-rifle-occur: Show results from all open Org buffers
;; helm-org-rifle-occur-directories: Show results from selected directories; with prefix, recursively
;; helm-org-rifle-occur-org-directory: Show results from Org files in org-directory
(defun rifle-in-orgs ()
  "search in predefined $HOME/org"
  (interactive)
  (helm-org-rifle-directories "~/org")
  )

(global-set-key (kbd "M-s M-s") 'rifle-in-orgs)

;; display inline image size
(setq org-image-actual-width nil)

;; ------------------------- language specific sections below ---------------------------------------

;; ----- ruby -----

;; load ruby robocop fmter
(load-user-file "mike/robocopfmt.el")
;; (add-hook 'ruby-mode-hook #'rubocopfmt-mode)

;; run test at cursor
(load-user-file "mike/test_rails.el")
;; (global-set-key (kbd "C-c C-c") 'my/rails-test-line-at-cursor)

;; send line to tmux pane 1
(load-user-file "mike/send_to_pane1.el")

(global-set-key (kbd "C-c b") 'my/tmux-send-to-pane-1)
(global-set-key (kbd "C-c k") 'my/tmux-send-C-c-to-pane-1)
(global-set-key (kbd "C-c d") 'my/tmux-send-C-d-to-pane-1)
(global-set-key (kbd "C-c s") 'my/tmux-swap-pane-1-with-0)

;; ---- web mode -----

;; does not warn about missing semicolon
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)
;; ----- elisp ------
;; auto complete
(add-hook 'emacs-lisp-mode-hook 'ielm-auto-complete)


;; ------- predefined split window ratio 30/70 --------
(defun my/split-window-right (&optional arg)
  "Split the current window 30/70
A single-digit prefix argument gives the left window size arg*10%."
  (interactive "P")
  (let ((proportion (* (or arg 12) 0.1)))
    (split-window-right (round (* proportion (window-height))))))

(global-set-key (kbd "C-c x 4") 'my/split-window-right)
