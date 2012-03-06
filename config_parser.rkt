#! /usr/bin/env racket
#lang racket
(require "helpers.rkt")

(define ip (open-input-file "/home/wes/lisp/lazyplay/lazyplay.sexp"))
(define parsed
    (read ip))
(close-input-port ip)

(define settings
    (update (make-hash) parsed))

(provide settings)