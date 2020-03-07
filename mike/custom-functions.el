;; ------------------------ general setup  -----------------------
(symon-mode)
(add-hook 'after-init-hook #'fancy-battery-mode)


(defun frame-bck()
  (interactive)
  (other-window -1))

(define-key (current-global-map) (kbd "C-x ]") 'other-window)
(define-key (current-global-map) (kbd "C-x [") 'frame-bck)

;; custom setups
(set-default 'truncate-line t)
(set-default 'word-wrap t)

;; global linum
(if (version< "26" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode 1))

;; display time inn the status line
(display-time-mode 1)

;; allow neotree resize
(setq neo-window-fixed-size nil)


;; auto-reload files from filesystem on git checkout branch
;; (global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)

;; Accomodate dropbox(each machine has its own desktop file)
;; (setq desktop-base-file-name (concat ".desktop." (system-name)))

;; projectile mode setup
(projectile-mode +1)

;; make buffer sticky -- prevent another window open in the same buffer
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)


;; load ace-jump
;; use [M-c + leading character] to jump to character
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "emacs quick move minor mode"
  t)
(define-key global-map (kbd "M-c") 'ace-jump-mode)

;; prevent taking too much cpu cycle
(savehist-mode 0)



(defun my/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


(defun my/split-window-right (&optional arg)
  "Split the current window 30/70. A single-digit prefix argument gives the left window size arg*10%."
  (interactive "P")
  (let ((proportion (* (or arg 12) 0.1)))
    (split-window-right (round (* proportion (window-height))))))
(global-set-key (kbd "C-c x 4") 'my/split-window-right)



;; ------------------------ text editing  -----------------------
(defun uniquify-all-lines-region (start end)
  "Remove duplicate lines in region START to END, keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

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

;; elisp auto complete
(add-hook 'emacs-lisp-mode-hook 'ielm-auto-complete)

;; ------------------------ language configurations -----------------------
;; does not warn about missing semicolon
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)


(require 'org-crypt)
; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; GPG key to use for encryption
(setq org-crypt-key "F0B66B40")


;; Remove this initial input in all commands:
(setq ivy-initial-inputs-alist nil)
