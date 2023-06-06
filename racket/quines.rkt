#lang racket
(require benchmark plot/pict racket/vector racket/list)
(require pretty-format)
(require macro-debugger/expand)

(require "mk.rkt")
(require "debug_stuff.rkt")

(include "q.scm")

(define === (lambda (a b)
  (lambda (st)
    (begin
      (incr_counter)
      (pretty-printf "~a  ~a\n" (pp a) (pp b))
      ((== a b) st)))
))

(define quineso (lambda (q)
  (eval-expo q '() `(val_ q))
))

(run 5 (q) (quineso q))
(report_counters)
