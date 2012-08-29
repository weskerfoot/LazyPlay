#! /usr/bin/env racket
#lang racket
(require racket/system)
(require racket/pretty)
(require "helpers.rkt")
(require "config_parser.rkt")
(require "playlist_server.rkt")
(require "command_parser.rkt")

(define args (vector->list (current-command-line-arguments)))

(define media-player (string->path (hash-ref settings "player" "/usr/bin/mplayer")))
(define player-args (hash-ref settings "args" '()))
(define file-types
    (update (make-hash) (hash-ref settings "filetypes" '(("avi" #t)))))

(define (update! newfs)
  (update played (map (compose reverse ((curry list) #f)) newfs)))

(define (file-list directory) ; list of files in the current working directory
    (map path->string ; get the strings from the list of paths
        (directory-list directory)))

; check if a filename has a desired suffix
(define (check-suffix file-types)
    (compose
        ((curry hash-has-key?) file-types)
        last
        ((curry regexp-split) #px"\\.")))

; filter out filenames without the desired suffix
(define (filter-paths file-types paths)
    (filter (check-suffix file-types) paths))

(define (play-list directory) 
    (sort-paths (filter-paths file-types (file-list directory)))) ; first commandline argument is the filename

; sort the paths
(define (sort-paths paths)
  (sort paths string<?))

; abusing hash tables to be sets
(define played 
    (let* ((table (make-hash)))
                (map (lambda (fname) 
                    (hash-set! table fname #t)) (play-list (current-directory)))
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

;; function to check if a file should be removed from the playlist
(define playlist-remove?
  (λ (a b)
    (regexp-match? (regexp (format "^~a$" a)) b)))

;;function to run when a chdir command is received
(define (chdir-command dir args)
  ;; if the command fails to run with the updated directory
  ;; then say so, and continue running
  (with-handlers ([exn:fail? 
                   (λ (_) 
                     (display 
                      (format "No such directory ~a"
                              (car dir)))
                     (play '() played args))])
  (current-directory (car dir)) 
    (update! (play-list (current-directory)))
    (play (play-list (current-directory)) played args)))

; the reactor procedure for commands
(define (play-react previous-files played args cmd next)
  (define (continue-playing new-prev args res)
      (play-react (append new-prev args res) 
              played args 
              (parse-command (next))
              next))
  
  (let ([inner (λ (new-previous)
                 (match cmd
                   [(add-resources s resources) (update! resources) 
                                                (continue-playing new-previous args resources)]
                   
                   [(add-commands s commands) (continue-playing new-previous commands '())]
                   
                   [(remove-command s resources) (remove* resources previous-files playlist-remove?) 
                                                 (continue-playing new-previous args resources)]
                   
                   [(chdir s directory) (chdir-command directory args)]
                   
                   [(modify s name new-content) s]
                   
                   [_ (play new-previous played args)]))])
               (match previous-files ; check if there are previous files
                 ['() (inner '())]
                 [_ (inner (cdr previous-files))])))

;; the main play procedure
(define (play fnames played args)
    (cond ((null? fnames)
           (let* ([new-data (thread-receive)])
             (play-react fnames played args new-data thread-try-receive)))
    (else
        (let* ([results (play-files fnames args)]) ; get the pid and the 3 i/o ports
            (subprocess-wait (first results)) ; block until a new file is started
            (close-output-port (third results)))
        
        (let* ([new-dir-files (new-files played (play-list (current-directory)))]
               [new-data (thread-try-receive)]) ; get new command information
          (update! new-dir-files)
            (match new-data
              [#f (play (append (cdr fnames) new-dir-files) 
                        played
                        args)]
              [_ (play-react (append (cdr fnames ) new-dir-files)
                             played
                             args
                             new-data
                             thread-try-receive)])))))

;; "ls" command used to add files selectively
(define (cmd-ls dir)
  (let* ([playlist (map ((compose
                          (curry string-append)
                          ((curry format) "~a/"))
                         (car dir)) (play-list (car dir)))]
        [number-list (for/list ([i (length playlist)]) i)]
        [output-list (map cons playlist number-list)])
    (display output-list)
    (let ([input (read-line (current-input-port))])
      (match (regexp-split #px"\\s" input)
        [(list-rest "add" xs)
         (let* ([xs (apply set (map string->number xs))]
                [added (filter (λ (x) (set-member? xs (cdr x))) output-list)])
         (add-resources "add" (map car added)))]
        [_ (add-resources "add" (list))]))))
 
;; the procedure to control the command line thingy
(define (controller player-thread)
    (cond 
        ((compose not thread-running?) player-thread ; if the thread is not running then return
        '()))
    (display "> ") ; TODO; add gnu readline support
    (let* [(input (read-line (current-input-port) 'linefeed))]
    (match input
      [(? eof-object?) (kill-thread player-thread)]
      [(? ((curry regexp-match-exact?) #px"ls.*")) (thread-send player-thread (cmd-ls (cdr (regexp-split #px"\\s" input))))
                                                   (controller player-thread)]
      [_ (thread-send player-thread (parse-command input))
         (controller player-thread)])))

(define player-thread (thread (lambda () 
                                (play (play-list (current-directory)) played player-args))))

(define controller-thread (thread (lambda () (controller player-thread))))

(define server-thread (thread (λ () (make-server player-thread parse-command))))

; check to see if the player is running, and if not then kill the controller
(define (check)
    ; (sleep 20)
    (cond (((compose not thread-running?) player-thread) 
        (kill-thread controller-thread))
    (else (check))))

(check)