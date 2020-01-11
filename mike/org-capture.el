'(org-capture-templates
  (quote
   (
    ("p" "Personal [inbox]" entry
     (file+headline "~/org/inbox.org" "Personal")
     "* IDEA %i%?
 %U")
    ("u" "urgent this week [inbox]" entry
     (file+headline "~/org/inbox.org" "Personal-this-week")
     "* TODO %i%?
 %U")
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
    ("w" "Work [inbox]" entry
     (file+headline "~/org/work.org" "Work")
     "* TODO %i%? 
 %U")
)))
