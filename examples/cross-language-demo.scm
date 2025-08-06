#!/usr/bin/env guile3
!#

;;; cross-language-demo.scm --- Demonstrates cross-language interop patterns

(define-module (cross-language-demo)
  #:use-module (language elisp spec)
  #:use-module (system base compile)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (demo-elisp-interop demo-scheme-features compare-paradigms))

;; Scheme-native implementations for comparison
(define (scheme-factorial n)
  "Pure Scheme factorial implementation."
  (if (<= n 1) 1 (* n (scheme-factorial (- n 1)))))

(define (scheme-map-square lst)
  "Pure Scheme list mapping."
  (map (lambda (x) (* x x)) lst))

(define (scheme-filter-even lst)
  "Pure Scheme list filtering."  
  (filter even? lst))

;; Dynamic Elisp compilation and execution
(define (compile-and-run-elisp code)
  "Compile Elisp code and execute it, returning the result."
  (catch #t
    (lambda ()
      (compile code #:from 'elisp #:to 'value))
    (lambda (key . args)
      (format #t "Elisp compilation error: ~a ~a~%" key args)
      #f)))

(define (demo-elisp-interop)
  "Demonstrate Elisp code running within Guile."
  (format #t "=== Cross-Language Interop Demo ===~%~%")
  
  ;; Test 1: Dynamic function definition and calling
  (format #t "1. Dynamic Elisp function definition:~%")
  (compile-and-run-elisp 
   '(defun elisp-factorial (n)
      (if (<= n 1) 1 (* n (elisp-factorial (- n 1))))))
  (let ((result (compile-and-run-elisp '(elisp-factorial 5))))
    (format #t "   Elisp factorial(5): ~a~%" result)
    (format #t "   Scheme factorial(5): ~a~%" (scheme-factorial 5)))
  
  ;; Test 2: List processing comparison
  (format #t "~%2. List processing paradigms:~%")
  (let ((test-list '(1 2 3 4 5 6)))
    (format #t "   Original list: ~a~%" test-list)
    
    ;; Scheme version
    (format #t "   Scheme squared: ~a~%" (scheme-map-square test-list))
    (format #t "   Scheme evens: ~a~%" (scheme-filter-even test-list))
    
    ;; Elisp version
    (let ((elisp-map-result 
           (compile-and-run-elisp 
            `(mapcar (lambda (x) (* x x)) ',test-list)))
          (elisp-filter-result
           (compile-and-run-elisp
            `(let (result)
               (dolist (x ',test-list (reverse result))
                 (when (= (mod x 2) 0)
                   (push x result)))))))
      (when elisp-map-result
        (format #t "   Elisp squared: ~a~%" elisp-map-result))
      (when elisp-filter-result
        (format #t "   Elisp evens: ~a~%" elisp-filter-result))))
  
  ;; Test 3: Higher-order functions
  (format #t "~%3. Higher-order function patterns:~%")
  (let ((compose-fn (compile-and-run-elisp 
                     '(defun compose (f g)
                        (lambda (x) (funcall f (funcall g x)))))))
    (when compose-fn
      (let ((double (lambda (x) (* x 2)))
            (square (lambda (x) (* x x))))
        (let ((composed (compose-fn square double)))
          (format #t "   Compose square(double(3)): ~a~%" (composed 3))
          (format #t "   Direct calculation: ~a~%" (square (double 3))))))))

(define (demo-scheme-features)
  "Showcase Scheme-specific features not easily translatable to Elisp."
  (format #t "~%=== Scheme-Specific Features ===~%~%")
  
  ;; Continuations (if supported)
  (format #t "1. Advanced control flow:~%")
  (let ((result 
         (call/cc (lambda (escape)
                    (let loop ((n 1) (acc 1))
                      (if (> n 10)
                          (escape acc)
                          (loop (+ n 1) (* acc n))))))))
    (format #t "   Call/cc factorial(10): ~a~%" result))
  
  ;; Multiple values
  (format #t "~%2. Multiple return values:~%")
  (let ((quotient-remainder (lambda (a b)
                               (values (quotient a b) (remainder a b)))))
    (let-values (((q r) (quotient-remainder 17 5)))
      (format #t "   17 ÷ 5 = ~a remainder ~a~%" q r)))
  
  ;; Advanced macros
  (format #t "~%3. Syntax transformation:~%")
  (let-syntax ((when-debug
                (syntax-rules ()
                  ((_ expr) (when #t expr)))))
    (when-debug 
     (format #t "   Debug macro executed successfully~%")))
  
  ;; Module system
  (format #t "~%4. Module system introspection:~%")
  (format #t "   Current module: ~a~%" (module-name (current-module)))
  (format #t "   Module uses: ~a~%" (module-uses (current-module))))

(define (compare-paradigms)
  "Compare functional vs imperative approaches across languages."
  (format #t "~%=== Paradigm Comparison ===~%~%")
  
  ;; Functional vs imperative fibonacci
  (format #t "1. Fibonacci implementations:~%")
  (format #t "   Scheme functional: ~a~%" 
          (let fib ((n 10))
            (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))
  
  (let ((elisp-iterative 
         (compile-and-run-elisp
          '(let ((a 0) (b 1) (n 10))
             (while (> n 0)
               (let ((temp a))
                 (setq a b)
                 (setq b (+ temp b))
                 (setq n (- n 1))))
             a))))
    (when elisp-iterative
      (format #t "   Elisp iterative: ~a~%" elisp-iterative)))
  
  ;; Data structure manipulation
  (format #t "~%2. Data structure approaches:~%")
  (let ((alist '((name . "Guile") (version . 3) (language . "Scheme"))))
    (format #t "   Scheme alist access: ~a~%" (assq 'name alist)))
  
  (let ((elisp-plist 
         (compile-and-run-elisp
          '(let ((plist '(name "Elisp" version 29 language "Emacs Lisp")))
             (plist-get plist 'name)))))
    (when elisp-plist
      (format #t "   Elisp plist access: ~a~%" elisp-plist))))

(define (main)
  "Main demonstration function."
  (demo-elisp-interop)
  (demo-scheme-features)
  (compare-paradigms)
  (format #t "~%Demo complete! This showcases Guile's unique multilanguage capabilities.~%"))

(when (batch-mode?)
  (main))

;;; cross-language-demo.scm ends here