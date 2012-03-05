#! /usr/bin/env racket
#lang racket

(require racket/system)
(require "helpers.rkt")

(define args (vector->list (current-command-line-arguments)))
; args: filename, mplayer-args
; optional: directory

(define mplcmd (string->path "/usr/bin/mplayer")) ; command used to play files
 ; output file to /dev/null/

(define (file-list) ; list of files in the current working directory
    (map path->string ; get the strings from the list of paths
        (directory-list (current-directory))))

; check if a filename has a desired suffix
(define (check-suffix suffix)
    (compose
        (partial string=? suffix)
        (lambda (x) 
            (let* 
                ((len (string-length x) ))
                (substring x (- len 3) len)))))

; filter out filenames without the desired suffix
(define (filter-paths suffix paths)
    (filter (check-suffix suffix) paths))

(define (play-list) 
    (sort-paths (filter-paths (first args) (file-list)))) ; first commandline argument is the filename

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

(define (play-files filenames)
        (let* ((nullport 
            (open-output-file (string->path "/dev/null") #:exists 'append))) ; we don't want any output from the process
                (call-with-values
                (lambda () (subprocess  nullport #f nullport mplcmd (car filenames)))
                list))) ; convert the 4 return values into a list

(define (play fnames played)
    (cond ((null? fnames) '())
    (else 
        (let* ((results (play-files fnames))) ; get the pid and the 3 i/o ports
            ; send the other thread the new pid here
            ; and the output port for piping input to the process
            ; (thread-send thd (first results))
            (subprocess-wait (first results))
            (close-output-port (third results)))
        (let* ((newfs (new-files played (play-list))))
            (play (append (cdr fnames) newfs) (update played newfs))))))

(play (play-list) played)
(define (controller)
    (let* ((input (read (current-input-port))))
    (cond ((eof-object? input))
    (else
        (printf "output: ~A\n" input)
        (controller)))))


; (define (main)
  ; (printf "Beginning (main)\n")
  ; (thread controller))
  ; (sleep 1000)


; (main)
