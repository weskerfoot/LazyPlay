#lang racket

(require (planet mordae/couchdb:1:11))
;(require (planet dherman/json:4:0))

(define (database-connection database-name)
  (couchdb-db
   (couchdb-connect
   #:host "localhost"
   #:port 5984
   #:user "wes"
   #:password "password")
   database-name))

(define conn (database-connection "blipcache"))

(define (cached? id)
  (with-handlers ([exn:couchdb:not-found?
                   (lambda (_) 
                     #f)])
    (couchdb-get conn id)))

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

;; Checks if a document needs updating
;; Info -> String
;; Data -> Hash
;; Updater -> (Hash -> Hash)
(define (update? info data updater)
  (let* ([last-time (hash-ref data 'last_updated)]
         [revision-id (hash-ref data '_rev)]
         [current-time (current-inexact-milliseconds)]
         [new-data (updater data)])
    (match (> (- current-time last-time) 300000)
    [#t (λ ()
          (let ([message (hasheq '_rev revision-id
                                 'last_updated current-time
                                 'content new-data
                                 '_id (hash-ref data '_id))])
            (couchdb-put conn message)))]
      [_ #f])))
            
          
;((update? "foobar" (couchdb-get conn "foobar") (λ (h) (string-append "watwat" "foobarbaz"))))          
                                   
;; 
(define (check-cache info type get-data)
  (match (cached? info)
    [#f (cache info type get-data)]
    [result 
     (match (update? info (couchdb-get conn info))
       [#f result]
       [updated (updated)])]))
       
(provide check-cache)