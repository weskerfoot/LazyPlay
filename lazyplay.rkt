#! /usr/bin/env racket
#lang racket

(require racket/system)
(require "helpers.rkt")
(require "config_parser.rkt")

(define args (vector->list (current-command-line-arguments)))
; args: filename, mplayer-args
; optional: directory
(hash-ref settings "args" "")
(define file-types (make-hash))
(define media-player (string->path (hash-ref settings "player" "/usr/bin/mplayer")))
(define player-args (hash-ref settings "args" ""))
(update file-types (hash-ref settings "file-types" '(("avi" #t))))

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

; sortgetter, gets the episode number and converts it to an integer
(define (sortgetter file-name)
    (string->number 
        (second (regexp-match #rx"E([0-9])*" file-name))))

; sort the paths
(define (sort-paths paths)
  (sort paths <
        #:key sortgetter))

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
                (lambda () (subprocess  nullport #f nullport media-player (car filenames) args))
                list))) ; convert the 4 return values into a list

(define (get-args message)
    (cond ((false? message) player-args)
    (else message)))

(define (play fnames played args)
    (cond ((null? fnames) '())
    (else 
        (let* ((results (play-files fnames args))) ; get the pid and the 3 i/o ports
            ; send the other thread the new pid here
            ; and the output port for piping input to the process
            ; (thread-send controller-thread ((third results) (first results)))
            (subprocess-wait (first results))
            (close-output-port (third results)))
        (let* ((newfs (new-files played (play-list))))
            (play (append (cdr fnames) newfs) (update played newfs) (get-args (thread-try-receive)))))))

(define (controller player-thread)
    (cond 
        ((compose not thread-running?) player-thread ; if the thread is not running then return
        '()))
    (let* ((input (read-line (current-input-port) 'linefeed)))
    input
    (cond ((eof-object? input) '())
    (else
        (thread-send player-thread input)
        (controller player-thread)))))

(define player-thread (thread (lambda () (play (play-list) played player-args))))
(define controller-thread (thread (lambda () (controller player-thread))))

; check to see if the player is running, and if not then kill the controller
(define (check)
    (sleep 20)
    (cond (((compose not thread-running?) player-thread) (kill-thread controller-thread))
    (else (check))))

(check)