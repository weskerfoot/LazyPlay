#lang racket

(require (planet mordae/couchdb:1:11))
;(require (planet dherman/json:4:0))

(define (database-connection database-name)
  (couchdb-db
   (couchdb-connect
   #:host "localhost"
   #:port 5984
   #:user "admin"
   #:password "password")
   database-name))

(define number->symbol (compose string->symbol number->string))

(define conn (database-connection "blipcache"))

(define (cached? id)
  (with-handlers ([exn:couchdb:not-found?
                   (lambda (_) 
                     #f)])
    ;(hash-ref (couchdb-get conn id) 'content)
    (couchdb-get conn id)))

(define (user-cached? username page-num)
  ((compose
   (λ (object)
     (match object
       [#f #f]
       [(? (λ (obj) (not (hash-has-key? obj (number->symbol page-num))))) 'update]
       [_ (hash-ref object (number->symbol page-num))]))
     cached?) username))

(define category-cached?
  (compose
   (λ (obj)
     (match obj
       [#f #f]
       [_ (hash-ref obj 'content)]))
   cached?))
  

;; type is the type of data to cache
;; info is the actual name of the data to be cached
;; get-data is the proc that gets the data from the database



(define (cache info type get-data)
  (match type
    ['user 
     (let* ([message (make-hash)]
            [data (get-data)])
       (hash-set! message '_id info)
       (hash-set! message 'content data)
       (hash-set! message 'last_updated (current-inexact-milliseconds))
     (couchdb-put conn message)
       data)]))

(define (cache-user message username page-num get-data)
  (let ([data (get-data)])
    (hash-set! message '_id username)
    (hash-set! message (number->symbol page-num) data)
    (hash-set! message 'last_updated (current-inexact-milliseconds))
    (couchdb-put conn message)
    data))

;; Checks if a document needs updating
;; Info -> String
;; Data -> Hash
;; Updater -> (Hash -> Hash)
(define (update? key data updater)
  (let* ([last-time (hash-ref data 'last_updated)]
         [revision-id (hash-ref data '_rev)]
         [current-time (current-inexact-milliseconds)]
         [new-data (updater data)])
    (match (> (- current-time last-time) 300000)
    [#t (λ ()
          (let ([message (hasheq '_rev revision-id
                                 'last_updated current-time
                                 key new-data
                                 '_id (hash-ref data '_id))])
            (couchdb-put conn message)
            data))]
      [_ #f])))
            
          
;((update? "foobar" (couchdb-get conn "foobar") (λ (h) (string-append "watwat" "foobarbaz"))))          

(struct user-cache-params (username pagenum))

(define (immuthsh->muthsh hsh)
  (make-hash (hash-map hsh (λ (a b) (cons a b)))))

;; 
(define (check-cache info type get-data updater)
  (match type
    ['user (let* [(username (user-cache-params-username info))
                  (pagenum (user-cache-params-pagenum info))]
           (match (user-cached? username pagenum)
             ['update (let ([message (immuthsh->muthsh (couchdb-get conn username))])
                        (cache-user message username pagenum get-data))]
             [#f (cache-user (make-hash) username pagenum get-data)]
             [result result]))]))
     
       
;(define (check-user-cache username page-number get-data)
;  (match (



(provide check-cache user-cache-params)