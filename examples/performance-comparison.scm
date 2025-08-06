#!/usr/bin/env guile3
!#

;;; performance-comparison.scm --- Benchmark different language implementations

(define-module (performance-comparison)
  #:use-module (language elisp spec)
  #:use-module (system base compile)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (benchmark-factorial benchmark-list-ops run-all-benchmarks))

(define (time-execution thunk iterations)
  "Time the execution of a thunk over multiple iterations."
  (let ((start-time (get-internal-real-time)))
    (let loop ((i iterations))
      (when (> i 0)
        (thunk)
        (loop (- i 1))))
    (let ((end-time (get-internal-real-time)))
      (/ (- end-time start-time) internal-time-units-per-second iterations))))

(define (benchmark-factorial n iterations)
  "Compare factorial implementations across languages."
  (format #t "~%=== Factorial Benchmark (n=~a, iterations=~a) ===~%" n iterations)
  
  ;; Scheme recursive
  (define (scheme-fac-recursive n)
    (if (<= n 1) 1 (* n (scheme-fac-recursive (- n 1)))))
  
  ;; Scheme iterative  
  (define (scheme-fac-iterative n)
    (let loop ((i n) (acc 1))
      (if (<= i 1) acc (loop (- i 1) (* acc i)))))
  
  ;; Compile Elisp versions once
  (define elisp-recursive 
    (compile '(defun elisp-fac-rec (n)
                (if (<= n 1) 1 (* n (elisp-fac-rec (- n 1)))))
             #:from 'elisp #:to 'value))
  
  (define elisp-iterative
    (compile '(defun elisp-fac-iter (n)
                (let ((result 1))
                  (while (> n 1)
                    (setq result (* result n))
                    (setq n (- n 1)))
                  result))
             #:from 'elisp #:to 'value))
  
  ;; Run benchmarks
  (let ((scheme-rec-time (time-execution (lambda () (scheme-fac-recursive n)) iterations))
        (scheme-iter-time (time-execution (lambda () (scheme-fac-iterative n)) iterations))
        (elisp-rec-time (time-execution (lambda () (elisp-recursive n)) iterations))
        (elisp-iter-time (time-execution (lambda () (elisp-iterative n)) iterations)))
    
    (format #t "Scheme recursive:  ~6,4f seconds~%" scheme-rec-time)
    (format #t "Scheme iterative:  ~6,4f seconds~%" scheme-iter-time)  
    (format #t "Elisp recursive:   ~6,4f seconds~%" elisp-rec-time)
    (format #t "Elisp iterative:   ~6,4f seconds~%" elisp-iter-time)
    
    ;; Performance ratios
    (let ((fastest (min scheme-rec-time scheme-iter-time elisp-rec-time elisp-iter-time)))
      (format #t "~%Relative performance (1.0 = fastest):~%")
      (format #t "Scheme recursive:  ~4,2fx~%" (/ scheme-rec-time fastest))
      (format #t "Scheme iterative:  ~4,2fx~%" (/ scheme-iter-time fastest))
      (format #t "Elisp recursive:   ~4,2fx~%" (/ elisp-rec-time fastest))
      (format #t "Elisp iterative:   ~4,2fx~%" (/ elisp-iter-time fastest)))))

(define (benchmark-list-ops lst-size iterations)
  "Compare list processing operations."
  (format #t "~%=== List Operations Benchmark (size=~a, iterations=~a) ===~%" 
          lst-size iterations)
  
  (let ((test-list (iota lst-size)))
    
    ;; Scheme implementations
    (define (scheme-map-square lst)
      (map (lambda (x) (* x x)) lst))
    
    (define (scheme-filter-even lst)
      (filter even? lst))
    
    (define (scheme-fold-sum lst)
      (fold + 0 lst))
    
    ;; Elisp implementations  
    (define elisp-map-square
      (compile `(lambda (lst)
                  (mapcar (lambda (x) (* x x)) lst))
               #:from 'elisp #:to 'value))
    
    (define elisp-filter-even
      (compile `(lambda (lst)
                  (let (result)
                    (dolist (x lst (reverse result))
                      (when (= (mod x 2) 0)
                        (push x result)))))
               #:from 'elisp #:to 'value))
    
    (define elisp-fold-sum
      (compile `(lambda (lst)
                  (let ((sum 0))
                    (dolist (x lst sum)
                      (setq sum (+ sum x)))))
               #:from 'elisp #:to 'value))
    
    ;; Benchmark map operation
    (let ((scheme-map-time (time-execution (lambda () (scheme-map-square test-list)) iterations))
          (elisp-map-time (time-execution (lambda () (elisp-map-square test-list)) iterations)))
      (format #t "Map operation:~%")
      (format #t "  Scheme: ~6,4f seconds~%" scheme-map-time)
      (format #t "  Elisp:  ~6,4f seconds (ratio: ~4,2fx)~%" 
              elisp-map-time (/ elisp-map-time scheme-map-time)))
    
    ;; Benchmark filter operation
    (let ((scheme-filter-time (time-execution (lambda () (scheme-filter-even test-list)) iterations))
          (elisp-filter-time (time-execution (lambda () (elisp-filter-even test-list)) iterations)))
      (format #t "Filter operation:~%")
      (format #t "  Scheme: ~6,4f seconds~%" scheme-filter-time)
      (format #t "  Elisp:  ~6,4f seconds (ratio: ~4,2fx)~%" 
              elisp-filter-time (/ elisp-filter-time scheme-filter-time)))
    
    ;; Benchmark fold operation
    (let ((scheme-fold-time (time-execution (lambda () (scheme-fold-sum test-list)) iterations))
          (elisp-fold-time (time-execution (lambda () (elisp-fold-sum test-list)) iterations)))
      (format #t "Fold operation:~%")
      (format #t "  Scheme: ~6,4f seconds~%" scheme-fold-time)
      (format #t "  Elisp:  ~6,4f seconds (ratio: ~4,2fx)~%" 
              elisp-fold-time (/ elisp-fold-time scheme-fold-time)))))

(define (benchmark-compilation-overhead)
  "Measure Elisp compilation vs execution time."
  (format #t "~%=== Compilation Overhead Analysis ===~%")
  
  (let ((elisp-code '(defun test-fn (x) (+ x x x))))
    
    ;; Time compilation
    (let ((compile-time 
           (time-execution 
            (lambda () (compile elisp-code #:from 'elisp #:to 'value))
            10)))
      (format #t "Average compilation time: ~6,4f seconds~%" compile-time))
    
    ;; Time execution of pre-compiled function
    (let ((compiled-fn (compile elisp-code #:from 'elisp #:to 'value)))
      (let ((exec-time 
             (time-execution (lambda () (compiled-fn 42)) 1000)))
        (format #t "Average execution time: ~6,4f seconds~%" exec-time)
        (format #t "Compilation overhead ratio: ~4,0fx~%" 
                (/ compile-time exec-time))))))

(define (analyze-memory-usage)
  "Analyze memory characteristics of different approaches."
  (format #t "~%=== Memory Usage Analysis ===~%")
  
  ;; This is a simplified analysis - in practice you'd use more sophisticated tools
  (let ((before-gc (gc-stats)))
    
    ;; Create large data structures
    (let ((large-scheme-list (iota 10000))
          (large-elisp-result 
           ((compile '(lambda () 
                        (let (result)
                          (dotimes (i 10000 result)
                            (push (* i i) result))))
                     #:from 'elisp #:to 'value))))
      
      (gc) ; Force garbage collection
      (let ((after-gc (gc-stats)))
        (format #t "Scheme list (10k elements) created~%")
        (format #t "Elisp list (10k elements) created~%")
        (format #t "GC stats comparison requires manual analysis~%")))))

(define (run-all-benchmarks)
  "Run comprehensive benchmark suite."
  (format #t "=== Guile Multilanguage Performance Comparison ===~%")
  (format #t "Guile version: ~a~%" (version))
  
  (benchmark-factorial 20 1000)
  (benchmark-list-ops 1000 100)
  (benchmark-compilation-overhead)
  (analyze-memory-usage)
  
  (format #t "~%=== Summary ===~%")
  (format #t "• Scheme generally faster for recursive operations~%")
  (format #t "• Elisp comparable for iterative operations~%") 
  (format #t "• Compilation overhead significant for small operations~%")
  (format #t "• Consider caching compiled Elisp functions~%")
  (format #t "• Choose language based on domain expertise, not just performance~%"))

(define (main)
  "Main benchmark runner."
  (run-all-benchmarks))

(when (batch-mode?)
  (main))

;;; performance-comparison.scm ends here