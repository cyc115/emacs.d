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
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

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


;; load-user-file
;; load a custom .el lib from ~/.emacs.d/ directory
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

;; ------------------------- tools / libraries / custom setups ------------------------------------------


;; load-user-file loads a custom .el file from the provided directory
;; eg. (load-user-file "mike/timer.el")
(defun load-user-file (file)
  (interactive "f")
 "Load a file in current user's configuration directory"
 (load-file (expand-file-name file user-init-dir)))


;; enable mouse scrolling in osx
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line) ;
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))


;; ------------------- load custom libraries ----------------------
(load-user-file "mike/timer.el")
(load-user-file "mike/ansi_color.el")
(load-user-file "mike/restclient.el")
(load-user-file "mike/ob-restclient.el")
(load-user-file "mike/custom-org-functions.el")
(load-user-file "mike/org-capture.el")
(load-user-file "mike/custom-eww-functions.el")
(load-user-file "mike/toggle-window-dedicated.el")
(load-user-file "mike/yas-setup.el")
(load-user-file "mike/textmate.el")  ;; load textmate mode to enhence goto functions see: https://github.com/defunkt/textmate.el
;; (load-user-file "mike/kali-screenshot.el")
(load-user-file "mike/ace-jump.el")
(load-user-file "mike/send_to_pane1.el")
(load-user-file "mike/pwk-tools.el")
(load-user-file "mike/custom-functions.el")

;; load ruby robocop fmter
;; (load-user-file "mike/robocopfmt.el")
;; (add-hook 'ruby-mode-hook #'rubocopfmt-mode)

;; run test at cursor
;; (load-user-file "mike/test_rails.el")
;; (global-set-key (kbd "C-c C-c") 'my/rails-test-line-at-cursor)


(require 'ansi-color)
(defun my/display-ansi-colors ()
  "decode ansi chars into colored buffer"
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))


;; quick find files
(require 'textmate)
(textmate-mode)
(global-set-key (kbd "M-p") 'textmate-goto-file)
(global-set-key (kbd "M-]") 'textmate-goto-symbol)
