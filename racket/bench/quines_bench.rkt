#lang racket
(require math/statistics pretty-format benchmark plot/pict)

(require "../mk.rkt")

(define lookupo
  (lambda (x env t)
    (fresh (rest y v)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== y x) (== v t))
        ((=/= y x) (lookupo x rest t))))))

(define not-in-envo (lambda (x env)
   (conde
     ((fresh (y v rest)
        (== env `((,y . ,v) . ,rest) )
        (=/= y x)
        (not-in-envo x rest)
         ))
     ((== '() env)) )))


(define proper-listo
 (lambda (exp env rs)
   (conde
     ((== '() exp) (== '() rs))
     ((fresh (e d t-e t-d)
        (== exp `(,e . ,d) )
        (== rs `(,t-e . ,t-d) )
        (eval-expo e env `(val_ ,t-e) )
        (proper-listo d env t-d) )) )))

(define eval-expo
  (lambda (exp env r)
    (conde
      ((fresh (t)
         (== exp `(seq ((symb 'quote) ,t)) )
         (== r `(val_ ,t))
         (not-in-envo 'quote env)
           ))
      ((fresh (es rs)
         (== exp `(seq ((symb 'list) . ,es)) )
         (== r `(val_ (seq ,rs)) )
         (not-in-envo 'list env)
         (proper-listo es env rs)
      ))
      ;
      ((fresh (s)
         (== exp `(symb ,s))
         (lookupo s env r) ))
      ((fresh (rator rand x body env^ a)
         (== exp `(seq (,rator ,rand)) )
         (eval-expo rand env a)
         (eval-expo rator env `(closure ,x ,body ,env^))
         (eval-expo body `((,x . ,a) . ,env^) r)
      ))

      ((fresh (x body)
         (== exp `(seq ( (symb 'lambda)
                          (seq ((symb ,x)))
                          ,body) ) )
         (not-in-envo 'lambda env)
         (== r `(closure ,x ,body ,env) ) ))
    )) )

(define myrun (lambda (n rel)
    (let ((st empty-state))
    (let ((scope (subst-scope (state-S st))))
    (let ((q (var scope)))
    (map
        (lambda (st0)
        (let ((st (state-with-scope st0 nonlocal-scope)))
            ((reify q) st))
        )
        (takeMK n ((rel q) st))
    )))))
)
(define run1 (lambda (rel)
  (map
    (lambda (p) p)
      (let ((st empty-state))
        (let ((scope (subst-scope (state-S st))))
          (let ((q (var scope)))
            (map
              (lambda (st0)
                (let ((st (state-with-scope st0 nonlocal-scope)))
                  ((reify q) st))
              )
              (takeMK 1 ((rel q) st))
            )))))
))
; default 755,704,976 
; 755,833,664
;(pretty-printf "~a\n" (myrun 1 (lambda (q) (eval-expo q '() `(val_ ,q)))))
; 100 quines: 1,142,906,896
;(pretty-printf "~a\n" (myrun 100 (lambda (q) (eval-expo q '() `(val_ ,q)))))
; 15 twines : 1,041,180,816
; (pretty-printf "~a\n" (myrun 15 (lambda (p)
;                             (fresh (q r)
;                                 (== p `(,q ,r))
;                                 (eval-expo q '() `(val_ ,r))
;                                 (eval-expo r '() `(val_ ,q)))
;                         )))
; 2 thrines: 1,093,984,848 bytes 
; (pretty-printf "~a\n" (myrun 2 (lambda (p)
;                             (fresh (q r s)
;                                 (== p `(,q ,r s))
;                                 (eval-expo q '() `(val_ ,r))
;                                 (eval-expo r '() `(val_ ,s))
;                                 (eval-expo s '() `(val_ ,q)))
;                         )))


; (display (myrun 1 (lambda (q) (eval-expo q '() `(val ,q)))))
(display "Benchmarking...\n")
(define results
    (run-benchmarks
        ; operations (whats)
        (list
            ;'mul255x255
            ;'mul127x127
            ;'quines1
            'quines100
            'twines15
            'thrines2
            ;'log243base3
            ;'exp3x5
            ;'exp7x2
            ; 'sleepHalf
        )
        ; list of options (hows)
        (list)
        ; to run each benchmark
        (lambda (op)
            (match op
            ['quines1
                (void (myrun 1 (lambda (q) (eval-expo q '() `(val_ ,q)))))
            ]
            ['quines100 (myrun 100 (lambda (q)
                            (eval-expo q '() `(val_ ,q))))]
            ['twines15 (myrun 15 (lambda (p)
                            (fresh (q r)
                                (== p `(,q ,r))
                                (eval-expo q '() `(val_ ,r))
                                (eval-expo r '() `(val_ ,q)))
                        ))]
            ['thrines2 (myrun 2 (lambda (p)
                            (fresh (q r s)
                                (== p `(,q ,r s))
                                (eval-expo q '() `(val_ ,r))
                                (eval-expo r '() `(val_ ,s))
                                (eval-expo s '() `(val_ ,q)))
                        ))]
            ; ['mul255x255 (run 1 (p) (*o (build-num 255) (build-num 255) p))]
            ; ['mul127x127 (run 1 (p) (*o (build-num 127) (build-num 127) p))]
            ; ['log243base3 (run 1 (p) (logo (build-num 243) (build-num 3) p (build-num 0)))]
            ; ['exp3x5 (run 1 (p) (expo (build-num 3) (build-num 5) p))]
            ; ['exp7x2 (run 1 (p) (expo (build-num 7) (build-num 2) p))]
            ['sleepHalf (sleep 0.5)]
            ))
        ; don't extract time, instead time (run ...)
        #:extract-time 'delta-time
        #:num-trials 40 ; TODO: 40 is better
        #:results-file "quines_bench_racket.sexp"
    ))

; TODO: plot
(for ([i results])
  (pretty-printf "~a: ~a\n"
    (benchmark-result-name i)
    (mean (benchmark-result-trial-times i)))
)