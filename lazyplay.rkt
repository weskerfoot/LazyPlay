#! /usr/bin/env racket
#lang racket
;; New features
;; Better filetype handling
;; Ability to skip forward and backwards in the playlist


(require racket/system)
(require racket/pretty)
(require "helpers.rkt")
(require "config_parser.rkt")

(define args (vector->list (current-command-line-arguments)))

(define media-player (string->path (hash-ref settings "player" "/usr/bin/mplayer")))
(define player-args (hash-ref settings "args" '()))
(define file-types
    (update (make-hash) (hash-ref settings "filetypes" '(("avi" #t)))))

(define (file-list) ; list of files in the current working directory
    (map path->string ; get the strings from the list of paths
        (directory-list (current-directory))))

; check if a filename has a desired suffix
(define (check-suffix file-types)
    (compose
        ((curry hash-has-key?) file-types)
        last
        ((curry regexp-split) #px"\\.")))

; filter out filenames without the desired suffix
(define (filter-paths file-types paths)
    (filter (check-suffix file-types) paths))

(define (play-list) 
    (sort-paths (filter-paths file-types (file-list)))) ; first commandline argument is the filename

; sort the paths
(define (sort-paths paths)
  (sort paths string<?))

; abusing hash tables to be sets
(define played 
    (let* ((table (make-hash)))
                (map (lambda (fname) 
                    (hash-set! table fname #t)) (play-list))
                table))

; list of new files that have been seen
(define (new-files sett files)
    (filter 
        (compose not ((curry hash-has-key?) played))
        files))

; (define (controller pid out)

(define (play-files filenames args)
    (let* ((nullport 
        (open-output-file (string->path "/dev/null") #:exists 'append))) ; we don't want any output from the process
            (call-with-values
            (lambda () (apply subprocess (append
                                          (list nullport #f nullport media-player (car filenames))
                                          args)))
            list))) ; convert the 4 return values into a list

;; gets all of the available data from the mailbox
(define (parse-new-data new-data next)
  (match new-data
    [#f #f]
    [ht
     (letrec ([inner (λ (acc)
                 (match (next)
                   [#f acc]
                   [new-hash
                    (match (hash-ref new-hash 'arguments)
                      ; the case that there is no arguments...
                      [#f (hash-set! acc 'new-files (append (hash-ref acc 'new-files) (hash-ref new-hash 'new-files)))
                          (inner acc)]
                           ; if there are arguments...
                      [_ (hash-set! acc 'arguments (hash-ref new-hash 'arguments))
                         (inner acc)])]))])
       (inner ht))]))

(define (get-args message args)
    (cond ((false? message) args)
    (else message)))

(define (play fnames played args)
    (cond ((null? fnames) '())
    (else 
        (let* ((results (play-files fnames args))) ; get the pid and the 3 i/o ports
            (subprocess-wait (first results)) ; block until a new file is started
            (close-output-port (third results))) 
        (let* ([new-dir-files (new-files played (play-list))]
               [new-data (parse-new-data (thread-try-receive) thread-try-receive)]) ; get new list of files
            (match new-data
              [#f (play (append (cdr fnames) new-dir-files) 
                        (update played (map (compose reverse ((curry list) #f)) new-dir-files))
                        args)]
              [_ (let ([newfs (append new-dir-files (hash-ref new-data 'new-files))])
               (play 
                (append (cdr fnames) newfs) ; append new files to the tail of the list of old files 
                (update played (map (compose reverse ((curry list) #f)) newfs)) ; update the set of played files
                (get-args (hash-ref new-data 'arguments) args)))]))))) ; check for new arguments from controller

(define (parse-command cmd)
  (match (regexp-split #px"\\s" cmd)
    [(list-rest "cmds" xs) 
     (let ([result (make-hash)])
     (hash-set! result 'arguments xs)
     (hash-set! result 'new-files '())
       result)]
    [(list-rest "add" xs)
     (let ([result (make-hash)])
     (hash-set! result 'arguments #f)
     (hash-set! result 'new-files (list (string-join xs "")))
       result)]
    [_ (let ([result (make-hash)])
         (hash-set! result 'arguments #f)
         (hash-set! result 'new-files '())
         result)]))


(define (controller player-thread)
    (cond 
        ((compose not thread-running?) player-thread ; if the thread is not running then return
        '()))
    (display "> ") ; TODO; add gnu readline support
    (let* [(input (read-line (current-input-port) 'linefeed))]
    input
    (cond ((eof-object? input) (kill-thread player-thread)) ; check if received EOF, and kill player-thread
    (else
        (thread-send player-thread (parse-command input))
        (controller player-thread)))))

(define player-thread (thread (lambda () (play (play-list) played player-args))))
(define controller-thread (thread (lambda () (controller player-thread))))

; check to see if the player is running, and if not then kill the controller
(define (check)
    ; (sleep 20)
    (cond (((compose not thread-running?) player-thread) 
        (kill-thread controller-thread))
    (else (check))))

(check)