(define-module (fibonacci)
  #:use-module (ice-9 hash-table)
  #:export (fib fib-iter fib-list fib-memo fib-matrix))

(define (fib n)
  "Naive recursive Fibonacci - exponential time complexity O(2^n)."
  (cond
   ((< n 0) (throw 'out-of-range "fib" "Negative input: ~A" (list n) (list n)))
   ((= n 0) 0)
   ((= n 1) 1)
   (else (+ (fib (- n 1))
            (fib (- n 2))))))

(define (fib-iter n)
  "Iterative Fibonacci - linear time complexity O(n), constant space O(1)."
  (define (fib-helper a b count)
    (if (= count 0)
        a
        (fib-helper b (+ a b) (- count 1))))
  (if (< n 0)
      (throw 'out-of-range "fib-iter" "Negative input: ~A" (list n) (list n))
      (fib-helper 0 1 n)))

(define (fib-list n)
  (define (build-list i acc)
    (if (< i 0)
        (reverse acc)
        (build-list (- i 1) (cons (fib-iter i) acc))))
  (build-list n '()))

;; Memoized version - demonstrates higher-order functional programming
(define fib-memo
  (let ((memo-table (make-hash-table)))
    (lambda (n)
      "Memoized Fibonacci - O(n) time, O(n) space with caching."
      (or (hash-ref memo-table n)
          (let ((result 
                 (cond
                  ((< n 0) (throw 'out-of-range "fib-memo" "Negative input: ~A" (list n) (list n)))
                  ((= n 0) 0)
                  ((= n 1) 1)
                  (else (+ (fib-memo (- n 1)) (fib-memo (- n 2)))))))
            (hash-set! memo-table n result)
            result))))

;; Matrix exponentiation method - O(log n)
(define (fib-matrix n)
  "Matrix exponentiation Fibonacci - logarithmic time complexity O(log n)."
  (define (matrix-mult a b)
    (let ((a11 (car a)) (a12 (cadr a)) (a21 (caddr a)) (a22 (cadddr a))
          (b11 (car b)) (b12 (cadr b)) (b21 (caddr b)) (b22 (cadddr b)))
      (list (+ (* a11 b11) (* a12 b21))
            (+ (* a11 b12) (* a12 b22))
            (+ (* a21 b11) (* a22 b21))
            (+ (* a21 b12) (* a22 b22)))))
  
  (define (matrix-power base exp)
    (cond
     ((= exp 0) '(1 0 0 1))  ; identity matrix
     ((even? exp) 
      (let ((half (matrix-power base (/ exp 2))))
        (matrix-mult half half)))
     (else (matrix-mult base (matrix-power base (- exp 1))))))
  
  (if (< n 0)
      (throw 'out-of-range "fib-matrix" "Negative input: ~A" (list n) (list n))
      (if (= n 0) 
          0
          (cadr (matrix-power '(1 1 1 0) n)))))

(define (main)
  (display "Fibonacci Algorithm Comparison\n")
  (display "===============================\n")
  (display "First 10 Fibonacci numbers: ")
  (display (fib-list 9))
  (newline)
  (display "\nImplementation comparison for F(20):\n")
  (display "Recursive: ") (display (fib 20)) (newline)
  (display "Iterative: ") (display (fib-iter 20)) (newline)  
  (display "Memoized:  ") (display (fib-memo 20)) (newline)
  (display "Matrix:    ") (display (fib-matrix 20)) (newline)
  (display "\nLarge number test F(50):\n")
  (display "Iterative: ") (display (fib-iter 50)) (newline)
  (display "Memoized:  ") (display (fib-memo 50)) (newline)
  (display "Matrix:    ") (display (fib-matrix 50)) (newline))

(when (eq? (current-module) (resolve-module '(fibonacci)))
  (main))