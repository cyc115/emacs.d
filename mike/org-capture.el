'(org-capture-templates
  (quote
   (
    ("p" "Personal [inbox]" entry
     (file+headline "~/org/inbox.org" "Personal")
     "* IDEA %i%?
 %U")
    ("d" "dream [ bucketlist - things I want to do eventually] " entry
     (file "~/org/bucketlist.org")
     "* TODO %i%? 
 %U")
    ("w" "Work [inbox]" entry
     (file+headline "~/org/work.org" "Work")
     "* TODO %i%? 
 %U")
    ("n" "Notes" entry
     (file "~/org/notes.org")
     "* NOTE %i%? 
 %U")
    ("m" "Movies to watch [inbox]" entry
     (file+headline "~/org/inbox.org" "Personal")
     "* IDEA %i%? :MOVIES:
 %U")
    ("k" "PWK lab todo" entry
     (file+headline "~/org/pwk_course/exploit.org" "10")
     "* TODO %i%? 
 %U")
    ("n" "PWK lab notes" entry
     (file+headline "~/org/pwk_course/exploit.org" "10")
     "* NOTE %i%? 
 %U")
    ("f" "PWK lab flags [inbox]" entry
     (file+headline "~/org/pwk_course/exploit.org" "10")
     "* NOTE %i%? :FLAG:
 %U"))))
