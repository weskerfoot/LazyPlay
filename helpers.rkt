#! /usr/bin/env racket
#lang racket

(define (update htable settings)
    (cond ((null? settings) htable)
    (else (map (lambda (setting)
        (hash-set! htable (first setting) (second setting))) settings)
        htable)))
        
(define (partial f x) 
    (lambda (y) (f x y)))

(define (flip f)
    (lambda (y x) (f x y)))
    
(define nowhere (open-output-nowhere 'nowhere #t))

(provide (all-defined-out))
