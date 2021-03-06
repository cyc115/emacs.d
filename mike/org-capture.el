(defun my/org-add-property-with-date-captured ()
  "Add DATE_CAPTURED property to the current item."
  (interactive)
  (org-set-property "DATE_CAPTURED" (format-time-string "[%F]")))

(add-hook 'org-capture-before-finalize-hook 'my/org-add-property-with-date-captured)

(setq org-capture-templates
      '(
        ("b" "bookmark" entry
         (file+headline "~/org/bookmark.org" "bookmark :bookmark:")
         "* NOTE %i%?
 %U
")
        ("1" "1-1 inbox [work_related]" checkitem
         (file+olp "~/org/work_related.org" "1-1s")
         "- [ ] ")
				("o" "1-1 notes [work_related]" entry
         (file+headline "~/org/work_related.org" "1-1s")
         "** TODO %t 1-1 with %?")
        ("p" "Personal [inbox]" entry
         (file+headline "~/org/inbox.org" "Personal")
         "* IDEA %i%?
 %U")
        ("w" "Work [work]" entry
         (file+headline "~/org/work.org" "Inbox")
         "* TODO %i%?
 %U")
        ("L" "locksmith [work]" entry
         (file+headline "~/org/work.org" "locksmith :locksmith:")
         "* TODO %i%?
 %U")
        ("I" "Work Interrupt [work]" checkitem
         (file+olp "~/org/work.org" "Interrupt")
         "- [ ] %t %?")
        ("t" "Tech-experiments [inbox]" entry
         (file+headline "~/org/inbox.org" "Tech-experiments")
         "* IDEA %i%?
 %U")
        ("i" "Self improvements [inbox]" entry
         (file+headline "~/org/inbox.org" "Self improvements")
         "* TODO %i%?
 %U")
        ("d" "dream [ bucketlist - things I want to do eventually] " entry
         (file "~/org/bucketlist.org")
         "* TODO %i%?
 %U")
        ("T" "Today I learned [learning]" entry
         (file+headline "~/org/learning.org" "TIL :til:")
         "* NOTE %i%?
 %U")
        ("H" "daily highlight [personal]" checkitem
         (file+olp "~/org/personal.org" "daily highlight")
         "- [ ] %t %?")
        ("n" "Currently clocked-in" item (clock)
         "Note taken on %U \\\\ \n%?"
         :prepend t)
        ))
