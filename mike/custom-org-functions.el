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

(define-key (current-global-map) (kbd "C-c &") 'helm-org-rifle-occur-org-directory)

;; display inline image size
(setq org-image-actual-width nil)
