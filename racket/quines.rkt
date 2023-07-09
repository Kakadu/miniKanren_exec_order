#lang racket
(require benchmark plot/pict racket/vector racket/list)
(require pretty-format)
(require macro-debugger/expand)

(require "mk.rkt")
(require "debug_stuff.rkt")

(define (list-display lis)
  (cond ((null? lis)
          #f)
        (else
          (display (car lis))
          (newline)
          (list-display (cdr lis)))))

(define === (lambda (a b [msg ""])
  ; (pretty-printf "partially applied unification ~a\n" msg)
  (lambda (st)
    (begin
      (incr_counter)
      (unless (getenv "SILENT_UNIFICATIONS")
        (pretty-printf "~a ~a, ~a\n" (trace_after_reify a st) (trace_after_reify b st) msg))
      ((== a b) st)))
))

(define not-in-envo (lambda (x env)
   (conde
     ((fresh (y v rest)
        (=== env `((,y . ,v) . ,rest) )
        (=/= y x)
        (not-in-envo x rest)
         ))
     ((=== '() env)) )))


(define proper-listo
 (lambda (exp env rs)
   (conde
     ((=== '() exp) (=== '() rs))
     ((fresh (e d t-e t-d)
        (=== exp `(,e . ,d) )
        (=== rs `(,t-e . ,t-d) )
        (eval-expo e env `(val_ ,t-e) )
        (proper-listo d env t-d) )) )))

(define eval-expo
  (lambda (exp env r)
    (conde
      ((fresh (t)
         (=== exp `(seq ((symb 'quote) ,t)) )
         (=== r `(val_ ,t))
         (not-in-envo 'quote env)
           ))
      ((fresh (es rs)
         (=== exp `(seq ((symb 'list) . ,es)) )
         (=== r `(val_ (seq ,rs)) )
         (not-in-envo 'list env)
         (proper-listo es env rs)
      ))
      ;
      ((fresh (s)
         (=== exp `(symb ,s))
         (lookupo s env r) ))
      ((fresh (rator rand x body env^ a)
         (=== exp `(seq (,rator ,rand)) )
         (eval-expo rand env a)
         (eval-expo rator env `(closure ,x ,body ,env^))
         (eval-expo body `((,x . ,a) . ,env^) r)
      ))

      ((fresh (x body)
         (=== exp `(seq ( (symb 'lambda)
                         (seq ((symb ,x)))
                         ,body) ) )
         (not-in-envo 'lambda env)
         (=== r `(closure ,x ,body ,env) ) ))
    )) )

(define lookupo
  (lambda (x env t)
    (fresh (rest y v)
      (=== `((,y . ,v) . ,rest) env)
      (conde
        ((=== y x) (=== v t))
        ((=/= y x) (lookupo x rest t))))))

;
(command-line
  #:program "compiler"
  #:once-each
  [("--app1") ""
      (begin
        (run 1 (q) (eval-expo '('asdf) '() q))
        (report_counters))]
   [("--simpleQ") ""
      (let ((answers
          (run 1 (q) (fresh (tmp)
            (=== tmp
              `(seq (
                (seq ((symb lambda) (seq (_.0)) (seq ((symb list) _.0 (seq ((symb list) (seq ((symb quote) (symb quote) )) _.0))))))
                (seq ((symb lambda) (seq (_.0)) (seq ((symb list) _.0 (seq ((symb list) (seq ((symb quote) (symb quote) )) _.0))))))
                )) )
            (eval-expo tmp '() `(val_ q))
          )) ))
        (pretty-printf " ===== \n")
        (list-display answers)
        (report_counters))]

  [("--firstQ") ""
      (begin
        (list-display
          (run 1 (q) (eval-expo q '() `(val_ q))))
        (report_counters))]
        #|
  [("--rev1") ""
      (begin
        (pretty-printf "~a\n"
          (run 1 (q) (reverso '(1 2) q))
        )
        (report_counters))]
  [("--rev2") ""
      (begin
        (pretty-printf "~a\n"
          (run 1 (q) (reverso q '(1 2)))
        )
        (report_counters))] |#
        )