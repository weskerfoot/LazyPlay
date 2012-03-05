#! /usr/bin/env racket
#lang racket
(require "helpers.rkt")

(define ip (open-input-file "/home/wes/lisp/lazyplay/lazyplay.sexp"))
(define parsed
    (read ip))
(close-input-port ip)

(define settings (make-hash))
(update settings parsed)

(provide settings)