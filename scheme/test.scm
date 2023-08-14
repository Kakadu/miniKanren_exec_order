(eval-when (compile) (optimize-level 3))

(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")


(define-syntax defrel
  (syntax-rules ()
   ;; No suspend given single goal; search order for relations designed with define
   ((_ (name arg ...) g)
    (define (name arg ...) g))
   ;; Use of fresh suspends when forming a conjunction to avoid nontermination
   ((_ (name arg ...) g ...)
    (define (name arg ...) (fresh () g ...)))))

(include "../faster-miniKanren/numbers.scm")
#|
(define appendo (lambda (l s out)
  (conde
    [(== '() l) (== s out)]
    [(fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res))])))

(printf "build-num\n")
(define build-num
  (lambda (n)
    (cond
      ((odd? n)
       (cons 1
         (build-num (quotient (- n 1) 2))))
      ((and (not (zero? n)) (even? n))
       (cons 0
         (build-num (quotient n 2))))
      ((zero? n) '()))))

(printf "zeroo\n")
(defrel (zeroo n)
  (== '() n))

(defrel (poso n)
  (fresh (a d)
    (== `(,a . ,d) n)))

(defrel (>1o n)
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))

(defrel (full-addero b x y r c)
  (conde
    ((== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c))))

(defrel (addero d n m r)
  (conde
    ((== 0 d) (== '() m) (== n r))
    ((== 0 d) (== '() n) (== m r)
     (poso m))
    ((== 1 d) (== '() m)
     (addero 0 n '(1) r))
    ((== 1 d) (== '() n) (poso m)
     (addero 0 '(1) m r))
    ((== '(1) n) (== '(1) m)
     (fresh (a c)
       (== `(,a ,c) r)
       (full-addero d 1 1 a c)))
    ((== '(1) n) (gen-addero d n m r))
    ((== '(1) m) (>1o n) (>1o r)
     (addero d '(1) n r))
    ((>1o n) (gen-addero d n m r))))

(defrel (gen-addero d n m r)
  (fresh (a b c e x y z)
    (== `(,a . ,x) n)
    (== `(,b . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (full-addero d a b c e)
    (addero e x y z)))

(defrel (pluso n m k)
  (addero 0 n m k))

(defrel (minuso n m k)
  (pluso m k n))

(defrel (*o n m p)
  (conde
    ((== '() n) (== '() p))
    ((poso n) (== '() m) (== '() p))
    ((== '(1) n) (poso m) (== m p))
    ((>1o n) (== '(1) m) (== n p))
    ((fresh (x z)
       (== `(0 . ,x) n) (poso x)
       (== `(0 . ,z) p) (poso z)
       (>1o m)
       (*o x m z)))
    ((fresh (x y)
       (== `(1 . ,x) n) (poso x)
       (== `(0 . ,y) m) (poso y)
       (*o m n p)))
    ((fresh (x y)
       (== `(1 . ,x) n) (poso x)
       (== `(1 . ,y) m) (poso y)
       (odd-*o x n m p)))))

(defrel (odd-*o x n m p)
  (fresh (q)
    (bound-*o q p n m)
    (*o x m q)
    (pluso `(0 . ,q) m p)))

(defrel (bound-*o q p n m)
  (conde
    ((== '() q) (poso p))
    ((fresh (a0 a1 a2 a3 x y z)
       (== `(,a0 . ,x) q)
       (== `(,a1 . ,y) p)
       (conde
         ((== '() n)
          (== `(,a2 . ,z) m)
          (bound-*o x y z '()))
         ((== `(,a3 . ,z) n)
          (bound-*o x y z m)))))))

(defrel (=lo n m)
  (conde
    ((== '() n) (== '() m))
    ((== '(1) n) (== '(1) m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (=lo x y)))))

(defrel (<lo n m)
  (conde
    ((== '() n) (poso m))
    ((== '(1) n) (>1o m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (<lo x y)))))

(defrel (<=lo n m)
  (conde
    ((=lo n m))
    ((<lo n m))))

(defrel (<o n m)
  (conde
    ((<lo n m))
    ((=lo n m)
     (fresh (x)
       (poso x)
       (pluso n x m)))))

(defrel (<=o n m)
  (conde
    ((== n m))
    ((<o n m))))

(defrel (/o n m q r)
  (conde
    ((== r n) (== '() q) (<o n m))
    ((== '(1) q) (=lo n m) (pluso r m n)
     (<o r m))
    ((<lo m n)
     (<o r m)
     (poso q)
     (fresh (nh nl qh ql qlm qlmr rr rh)
       (splito n r nl nh)
       (splito q r ql qh)
       (conde
         ((== '() nh)
          (== '() qh)
          (minuso nl r qlm)
          (*o ql m qlm))
         ((poso nh)
          (*o ql m qlm)
          (pluso qlm r qlmr)
          (minuso qlmr nl rr)
          (splito rr r '() rh)
          (/o nh m qh rh)))))))

(defrel (splito n r l h)
  (conde
    ((== '() n) (== '() h) (== '() l))
    ((fresh (b n^)
       (== `(0 ,b . ,n^) n)
       (== '() r)
       (== `(,b . ,n^) h)
       (== '() l)))
    ((fresh (n^)
       (== `(1 . ,n^) n)
       (== '() r)
       (== n^ h)
       (== '(1) l)))
    ((fresh (b n^ a r^)
       (== `(0 ,b . ,n^) n)
       (== `(,a . ,r^) r)
       (== '() l)
       (splito `(,b . ,n^) r^ '() h)))
    ((fresh (n^ a r^)
       (== `(1 . ,n^) n)
       (== `(,a . ,r^) r)
       (== '(1) l)
       (splito n^ r^ '() h)))
    ((fresh (b n^ a r^ l^)
       (== `(,b . ,n^) n)
       (== `(,a . ,r^) r)
       (== `(,b . ,l^) l)
       (poso l^)
       (splito n^ r^ l^ h)))))

(defrel (logo n b q r)
  (conde
    ((== '(1) n) (poso b) (== '() q) (== '() r))
    ((== '() q) (<o n b) (pluso r '(1) n))
    ((== '(1) q) (>1o b) (=lo n b) (pluso r b n))
    ((== '(1) b) (poso q) (pluso r '(1) n))
    ((== '() b) (poso q) (== r n))
    ((== '(0 1) b)
     (fresh (a ad dd)
       (poso dd)
       (== `(,a ,ad . ,dd) n)
       (exp2 n '() q)
       (fresh (s)
         (splito n dd r s))))
    ((fresh (a ad add ddd)
       (conde
         ((== '(1 1) b))
         ((== `(,a ,ad ,add . ,ddd) b))))
     (<lo b n)
     (fresh (bw1 bw nw nw1 ql1 ql s)
       (exp2 b '() bw1)
       (pluso bw1 '(1) bw)
       (<lo q n)
       (fresh (q1 bwq1)
         (pluso q '(1) q1)
         (*o bw q1 bwq1)
         (<o nw1 bwq1))
       (exp2 n '() nw1)
       (pluso nw1 '(1) nw)
       (/o nw bw ql1 s)
       (pluso ql '(1) ql1)
       (<=lo ql q)
       (fresh (bql qh s qdh qd)
         (repeated-mul b ql bql)
         (/o nw bw1 qh s)
         (pluso ql qdh qh)
         (pluso ql qd q)
         (<=o qd qdh)
         (fresh (bqd bq1 bq)
           (repeated-mul b qd bqd)
           (*o bql bqd bq)
           (*o b bq bq1)
           (pluso bq r n)
           (<o n bq1)))))))

(defrel (exp2 n b q)
  (conde
    ((== '(1) n) (== '() q))
    ((>1o n) (== '(1) q)
     (fresh (s)
       (splito n b s '(1))))
    ((fresh (q1 b2)
       (== `(0 . ,q1) q)
       (poso q1)
       (<lo b n)
       (appendo b `(1 . ,b) b2)
       (exp2 n b2 q1)))
    ((fresh (q1 nh b2 s)
       (== `(1 . ,q1) q)
       (poso q1)
       (poso nh)
       (splito n b s nh)
       (appendo b `(1 . ,b) b2)
       (exp2 nh b2 q1)))))

(defrel (repeated-mul n q nq)
  (conde
    ((poso n) (== '() q) (== '(1) nq))
    ((== '(1) q) (== n nq))
    ((>1o q)
     (fresh (q1 nq1)
       (pluso q1 '(1) q)
       (repeated-mul n q1 nq1)
       (*o nq1 n nq)))))

(defrel (expo b q n)
  (logo n b q '()))
|#

(printf "HERE\n")
(display
  (run 1 (q) (== q '(123))))
(printf "Warmup\n ~a\n"
    (run 1 (p) (*o (build-num 255) (build-num 255) p)))
(printf "HERE\n")

(define time_acc 0.0)
(define iter (lambda (n f)
  (cond
    ((= n 0) (void) )
    (else
      (let ((tbegin (real-time)))
      (let ( (ans (f)) )
        ;(printf "~a\n" ans)
      (let ((tend (real-time)))
        (set! time_acc (+ time_acc (- tend tbegin)))
        (iter (- n 1) f)
      )))))))

(define avg (lambda (name n f)
    (set! time_acc 0.0)
    (collect)
    (iter n f)
    (printf "~a: ~a(s)\n" name (/ time_acc (* n 1000)))
))

(avg "mul255x255" 10
    (lambda ()
        (run 1 (p) (*o (build-num 255) (build-num 255) p))))

(avg "mul127x127" 10
    (lambda ()
        (run 1 (p) (*o (build-num 127) (build-num 127) p))))

(avg "log243base3" 10
    (lambda ()
        (run 1 (p) (logo (build-num 243) (build-num 3) p (build-num 0)))))

(avg "exp3^5" 10
    (lambda ()
        (run 1 (p) (expo (build-num 3) (build-num 5) p)) ))

(avg "exp7^2" 10
    (lambda ()
        (run 1 (p) (expo (build-num 7) (build-num 2) p)) ))
