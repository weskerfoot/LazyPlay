#! /usr/bin/env racket
#lang racket

(require racket/system)
(require racket/pretty)
(require "helpers.rkt")
(require "config_parser.rkt")

(define args (vector->list (current-command-line-arguments)))

(define media-player (string->path (hash-ref settings "player" "/usr/bin/mplayer")))
(define player-args (hash-ref settings "args" ""))
(define file-types
    (update (make-hash) (hash-ref settings "filetypes" '(("avi" #t)))))

(define (file-list) ; list of files in the current working directory
    (map path->string ; get the strings from the list of paths
        (directory-list (current-directory))))

; check if a filename has a desired suffix
(define (check-suffix file-types)
    (compose
        (partial hash-has-key? file-types)
        (lambda (x) 
            (let* 
                ((len (string-length x) ))
                (substring x (- len 3) len)))))

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
        (compose not (partial hash-has-key? played))
        files))

; (define (controller pid out)

(define (play-files filenames args)
    (let* ((nullport 
        (open-output-file (string->path "/dev/null") #:exists 'append))) ; we don't want any output from the process
            (call-with-values
            (lambda () (subprocess nullport #f nullport media-player (car filenames) args))
            list))) ; convert the 4 return values into a list

(define (get-args message args)
    (cond ((false? message) args)
    (else message)))

(define (play fnames played args)
    (cond ((null? fnames) '())
    (else 
        (let* ((results (play-files fnames args))) ; get the pid and the 3 i/o ports
            (subprocess-wait (first results)) ; block until a new file is started
            (close-output-port (third results))) 
        (let* ((newfs (new-files played (play-list)))) ; get new list of files
            (play 
                (append (cdr fnames) newfs) ; append new files to the tail of the list of old files 
                (update played newfs) ; update the set of played files
                (get-args (thread-try-receive) args)))))) ; check for new arguments from controller

(define (controller player-thread)
    (cond 
        ((compose not thread-running?) player-thread ; if the thread is not running then return
        '()))
    (display "> ") ; TODO; add gnu readline support
    (let* ((input (read-line (current-input-port) 'linefeed)))
    input
    (cond ((eof-object? input) (kill-thread player-thread)) ; check if received EOF, and kill player-thread
    (else
        (thread-send player-thread input)
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