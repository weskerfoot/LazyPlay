#! /usr/bin/env racket
#lang racket

(define ip (open-input-file "lazyplay.sexp"))
(read-syntax "~/lisp/lazyplay/lazyplay.sexp" ip)
(close-input-port ip)