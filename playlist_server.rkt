#lang racket
(require web-server/servlet
         web-server/servlet-env)
(require web-server/templates)
(require web-server/dispatch)
(require "links.rkt")
(require "command_parser.rkt")
(require "cacher.rkt")
(require xml)

(define (make-server player-thread parse-command)
  (define (response/404 req) 
  (response 404 #"Not Found" 
            (current-seconds) 
            TEXT/HTML-MIME-TYPE 
            (list) 
            (λ (op) (write-bytes #"Not Found" op))))

(define (list->hash xs)
  (let ([ht (make-hash)])
    (map
     (λ (item)
       (if (pair? item)
           (hash-set! ht (car item) (cdr item))
           (error "not a pair"))) xs)
    ht))

(define-values (lazyplay-dispatch lazyplay-url)
  (dispatch-rules
   [("getcategory") category-response]
   [("categories") category-list-response]
   [("favicon.ico") response/404]
   [("add") add-name]
   [else user-list]))

;(check-cache 
;       username 
;       'user 
;       (λ () (retrieve-videos username)) 
;       identity)
  
;; Returns all of the videos for a user
(define (user-list req)
  (response/xexpr
   (string->xexpr
    (let [(username (path/param-path (car (url-path (request-uri req)))))
          (page-n (hash-ref (list->hash (url-query (request-uri req))) 'p))]
      (retrieve-videos username (string->number page-n))))))

;; Adds a new resource to the lazyplay queue
(define (add-name req)
  (let ([name (hash-ref
               (list->hash (url-query (request-uri req)))
               'name)])
    (thread-send player-thread (add-resources "add" (list name)))
    (response/xexpr (string->xexpr (include-template "./sent.html")))))

;; Lists all the available categories
(define (category-list-response req)
  (response/xexpr (string->xexpr (include-template "./categories.html"))))

;; Gets a category listing
(define (category-response req) 
  (let* ([query-data (url-query (request-uri req))]
         [query-hash (list->hash query-data)]
         [categoryname (hash-ref query-hash 'category)]
         [categories (get-category categoryname)])
    (response/xexpr (string->xexpr (include-template "./main.html")))))

(serve/servlet 
 lazyplay-dispatch
 #:servlet-regexp #px""
 #:launch-browser? #f
 #:banner? #f
 #:port 8080))

(provide make-server)