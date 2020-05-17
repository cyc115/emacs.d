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

;; org babel execute languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (restclient . t)
   (ruby . t)
   (shell . t)
   ))

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
(setq org-agenda-skip-unavailable-files t)
(setq org-agenda-skip-scheduled-if-done t)

;; org custom agenda views
(setq org-agenda-custom-commands
      (quote
       (("w" "Daily stuff:  Agenda + :thisweek:work: + :thisweek: "
         (
          ;; show agenda view
          (agenda "" nil)
          ;; show additional lists
          (tags-todo ":thisweek:work:" ((org-agenda-overriding-header "This week @ work")))
          (tags-todo "thisweek-work" ((org-agenda-overriding-header "This week")))
          (tags-todo "Q1" ((org-agenda-overriding-header "This Quarter")))
          (tags-todo "inbox" ((org-agenda-overriding-header "All inbox"))))

         ((org-agenda-start-with-log-mode '(closed clock state))
          (org-agenda-start-with-clockreport-mode t)
          )
         )
        ("p" "Agenda and personal related tasks"
         ((agenda "" nil)
          (tags-todo "personal" nil))
         nil)
        ("d" "daily and review"
         ((tags-todo "inbox" nil))
         nil))))


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

(defun my/search-today-i-learned ()
  "search TIL notes in learning.org"
  (interactive)
  (helm-org-rifle-files "~/org/learning.org"))

(global-set-key (kbd "C-c &") 'my/search-today-i-learned)
(global-set-key (kbd "C-c f") 'helm-org-rifle-agenda-files)

;; display inline image size
(setq org-image-actual-width 400)

(define-key org-mode-map (kbd "C-c l") 'org-store-link)

;; generate agenda every 5 minutes idle
(run-with-idle-timer 5 nil (lambda () (org-agenda-list) (delete-window)))
