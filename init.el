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
             ;; top-bottom
             ;; "xrandr" nil "xrandr --output eDP-1 --output DP-2 --above eDP-1 --auto"

             ;; left-right
             "xrandr" nil "xrandr --output eDP-1 --output DP-2 --right-of eDP-1 --primary --auto"
             )))

(exwm-randr-enable)
(exwm-config-default)
(define-key exwm-mode-map [?\C-q] 'exwm-input-release-keyboard)

(defun exwm-logout ()
  (interactive)
  (recentf-save-list)
  (save-some-buffers)
  (start-process-shell-command "logout" nil "lxsession-logout"))

(symon-mode)
(add-hook 'after-init-hook #'fancy-battery-mode)

;; ------------------------- original spacemacs setup --------------------------------------------------

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

(defun frame-bck()
(interactive)
(other-window -1))

(define-key (current-global-map) (kbd "C-x ]") 'other-window)
(define-key (current-global-map) (kbd "C-x [") 'frame-bck)
;; (define-key (current-global-map) (kbd "C-x p") 'frame-bck)

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
;; (desktop-save-mode 1)

;; enable mouse scrolling in osx
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line) ;
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; allow neotree resize
(setq neo-window-fixed-size nil)

;; global linum
(if (version< "26" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode 1))

;; display time inn the status line
(display-time-mode 1)

;; --------------------- yas snippet ----------------------------
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

;; --------------------------- global revert-mode / projectile ---------------------------------
;; auto-reload files from filesystem on git checkout branch
;; (global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)
;; Accomodate dropbox(each machine has its own desktop file)
;; (setq desktop-base-file-name (concat ".desktop." (system-name)))


;; projectile mode setup
(projectile-mode +1)

;; ------------------------ auto-completion -----------------------

;; tab-nine autocomplete
;; (use-package company-tabnine :ensure t)
;; (add-to-list 'company-backends #'company-tabnine)

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


;; ------------------- load custom libraries ----------------------
(load-user-file "mike/timer.el")
(load-user-file "mike/ansi_color.el")
(load-user-file "mike/restclient.el")
(load-user-file "mike/ob-restclient.el")
(load-user-file "mike/custom-org-functions.el")
(load-user-file "mike/custom-eww-functions.el")
(load-user-file "mike/kali-screenshot.el")
(load-user-file "mike/toggle-window-dedicated.el")

;; take screenshot from emacs on kali
(global-set-key (kbd "C-c SPC ") 'my/org-screenshot-kali)
(global-set-key (kbd "C-c ] ") 'my/screenshot-clipboard)

;; make buffer sticky -- prevent another window open in the same buffer
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

;; load textmate mode to enhence goto functions see: https://github.com/defunkt/textmate.el
(load-user-file "mike/textmate.el")
(require 'textmate)
(textmate-mode)
(global-set-key (kbd "M-p") 'textmate-goto-file)
(global-set-key (kbd "M-]") 'textmate-goto-symbol)

;; load ace-jump
;; use [M-c + leading character] to jump to character
(load-user-file "mike/ace-jump.el")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "emacs quick move minor mode"
  t)
(define-key global-map (kbd "M-c") 'ace-jump-mode)

;; ---------------------------------- org capture templates ------------------------------------------------------
(setq org-capture-templates '(("w" "Work [inbox]" entry
                               (file+headline "~/org/inbox.org" "Work")
                               "* TODO %i%? \n %U")
                              ("p" "Personal [inbox]" entry
                               (file+headline "~/org/inbox.org" "Personal")
                               "* TODO %i%? \n %U")
                              ("k" "PWK / htb lab todo" entry
                               (file+headline "~/org/hackthebox/friendzone/friendzone.org" "TODOs")
                               "* TODO %i%? \n %U")
                              ("n" "PWK lab notes" entry
                               (file+headline "~/org/pwk_course/exploit.org" "5")
                               "* NOTE %i%? \n %U")
                              ("f" "PWK lab flags [inbox]" entry
                               (file+headline "~/org/pwk_course/exploit.org" "5")
                               "* NOTE %i%? :FLAG:\n %U")
                              ) )
(global-set-key (kbd "M-q ") 'org-capture)

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
(put 'narrow-to-page 'disabled nil)
