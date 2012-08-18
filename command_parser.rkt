#lang racket

;; guard procedure for the command type
(define/contract (valid? identifier argument-strings name)
  (-> string? (or/c string? (listof string?)) (or/c string? symbol?) (values string? (listof string?)))
  (values identifier (list argument-strings)))

;; command type
(struct command (identifier))

(struct add-resources command (resources))
(struct remove-command command (resources))
(struct add-commands command (commands))
(struct chdir command (directory))
(struct modify command (command-name new-content))

(struct commands (add-resources
                  remove-command
                  add-commands
                  chdir
                  modify))

;; parses a command to be sent to the player thread
(define (parse-command cmd)
  (match cmd
    [(? string?) 
     (match (regexp-split #px"\\s" cmd)
       [(list-rest "cmds" xs) (add-commands "cmds" xs)]
       [(list-rest "add" xs) (add-resources "add" (list (string-join xs "")))]
       [(list-rest "chdir" xs) (chdir "chdir" xs)]
       [(list-rest "remove" xs) (remove-command "rem" xs)]
       [_ (command #f)])]
    [_ (command #f)]))

(provide (all-defined-out))