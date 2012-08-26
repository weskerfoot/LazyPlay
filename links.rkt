#lang racket
(require (planet clements/sxml2:1:3))
(require (planet dherman/json:4:0))
(require (planet neil/htmlprag:1:5))
(require net/url)
(require net/uri-codec)

;; Blip.tv search and download server

(define *MAXPAGES* 60)

(define categories
  #hash(("Animation/Comics" . 43)
        ("Comedy" . 44)
        ("Drama" . 45)
        ("Entertainment" . 45)
        ("Fashion/Beauty" . 47)
        ("Food/Drink" . 48)
        ("Health/Fitness" . 50)
        ("Home/Family" . 51)
        ("Howto" . 52)
        ("Learning" . 53)
        ("Music" . 55)
        ("News/Politics" . 56)
        ("Sports/Cars" . 58)
        ("Talk/Interview" . 59)
        ("Tech/Gadgets" . 60)
        ("Videogames" . 61)))

;; Category Stuff Starts...
(define (make-category-url category page)
  (let* ([id (hash-ref categories category #f)]
         [url (λ () (format
                     "http://blip.tv/pr/channel_get_directory_listing?channels_id=~a&section=all&page=~a"
                     id
                     page))])
    (if (false? id)
        #f
   (values
    (λ ()
     (set! page (+ 1 page))
      (url))
    url
    category
    id))))

(define add-breaks
  (sxml:modify (list "a" 'insert-following `(br ""))))

(define (parse-category-chunk chunk)
  (let* ([links (sxpath "//li/div/h3/a")]
         [descriptions (sxpath "//p")]
         [html (port->string (get-pure-port (string->url chunk)))]
         [result (links (html->shtml html))])
  (match result
    ['() '()]
    [_ 
     (let* ([new-result (shtml->html result)]
            [new-new-result (html->sxml new-result)])
       (srl:sxml->html (add-breaks new-new-result)))])))

(define (get-category-list next url category)
  (let ([pages '()])
    (letrec ([acc-pages (λ (n)
                (match (eq? n *MAXPAGES*)
                    [#t pages]
                    [#f (let ([res (parse-category-chunk (next))])
                          (match (empty? res) 
                           [#f (set! pages (cons res pages))
                               (acc-pages (+ 1 n))]
                           [#t pages]))]))])
      (acc-pages 1)
      pages)))

(define (get-category category)
  (define-values (drama.next drama.url drama.category drama.id) (make-category-url category 1))
        (string-join
         (reverse (get-category-list drama.next (drama.url) drama.category)) ""))

;; Category Stuff ends...

(define (string->json data)
  (json->jsexpr (regexp-replace* #px"\\s" (port->string data) "")))
;; Turns a normal blip video url into the direct link
(define (blipurl->direct-url link)
  (let* ([data (string->json (get-pure-port (string->url (format "~a?skin=json&version=2&no_wrap=1" link))))]
         [new-url (string->url (hash-ref (hash-ref (hash-ref data 'Post) 'media) 'url))])
    (url->string new-url)))

;; Searches blip.tv
(define (search-blip keywords)
  (let ([data (get-pure-port 
                (string->url (format "http://blip.tv/posts/?pagelen=650&skin=json&search=~a&version=2&no_wrap=1" keywords)))])
    (string->json data)))

;;Gets all of a user's videos
(define (retrieve-videos username page-n)
  (let* ([user-url (string->url (format "http://blip.tv/~a?pagelen=5&skin=json&version=2&no_wrap=1&page=~a" username page-n))]
         [data (string->json (get-pure-port user-url))])
    (format
     "<html><body>~a <p></p><a href=\"http://localhost:8080/~a?p=~a\">Previous</a><a href=\"http://localhost:8080/~a?p=~a\">Next</a></body></html>"
     (format "~a" (string-join
                           (map (λ (x) 
                                 (format "<a href=\"http://localhost:8080/add?name=~a\">~a</a>"
                                         (form-urlencoded-encode
                                          (blipurl->direct-url (hash-ref x 'url)))
                                         (hash-ref x 'title))) data)
                           "<br />"))
     username
     (- page-n 1)
     username
     (+ 1 page-n))))

;(retrieve-videos "slowbeef")

(provide get-category retrieve-videos)