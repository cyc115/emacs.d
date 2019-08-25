(setq elfeed-feeds
      '(
        "http://possiblywrong.wordpress.com/feed/"
        "http://summitroute.com/blog/feed.ml"
        "https://hnrss.org/newest?points=100"

        ;; aws
        "http://feeds.feedburner.com/HighScalability?format=xml"
        "https://www.lastweekinaws.com/feed/"

        "https://infosec-handbook.eu/blog/index.xml"
        ))

;; tagging
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(video youtube)))

;; tag old entries as read
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread))

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "HighScalability\\.com"
                              :add '(aws)))


(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "lastweekinaws\\.com"
                              :add '(aws)))

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "https://infosec-handbook\\.eu/blog/index\\.xml"
                              :add '(security)))

