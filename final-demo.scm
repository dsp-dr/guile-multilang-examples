#!/usr/bin/env guile
!#

;;; final-demo.scm --- Final multilanguage demo for Guile on FreeBSD 14.3

(use-modules (system base compile)
             (ice-9 format)
             (rnrs bytevectors))

;; Define functions at top level
(define (scheme-factorial n)
  (if (<= n 1) 1 (* n (scheme-factorial (- n 1)))))

(define (print-separator)
  (format #t "~a~%" (make-string 60 #\-)))

(define (print-header title)
  "Print a formatted header for demo sections."
  (format #t "~%")
  (print-separator)
  (format #t "~a~%" title)
  (print-separator))

(define (main)
  "Main comprehensive demo function."
  
  (print-header "GUILE MULTILANGUAGE DEMONSTRATION")
  (format #t "System: FreeBSD 14.3-RELEASE amd64~%")
  (format #t "Guile Version: ~a~%" (version))
  (format #t "Date: ~a~%" (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time))))
  
  (print-header "✓ SCHEME (NATIVE) CAPABILITIES")
  
  (format #t "1. Arithmetic and basic operations:~%")
  (format #t "   (+ 15 25 35) = ~a~%" (+ 15 25 35))
  (format #t "   (* 7 8 9) = ~a~%" (* 7 8 9))
  (format #t "   (expt 2 10) = ~a~%~%" (expt 2 10))
  
  (format #t "2. Higher-order functions:~%")
  (let* ((data '(1 2 3 4 5 6 7 8 9 10))
         (evens (filter even? data))
         (cubes (map (lambda (x) (* x x x)) evens))
         (sum (apply + cubes)))
    (format #t "   Original: ~a~%" data)
    (format #t "   Even numbers: ~a~%" evens)
    (format #t "   Cubed: ~a~%" cubes)
    (format #t "   Sum of cubes: ~a~%~%" sum))
  
  (format #t "3. String manipulation:~%")
  (let ((words '("Guile" "is" "a" "powerful" "Scheme" "implementation")))
    (format #t "   Words: ~a~%" words)
    (format #t "   Joined: \"~a\"~%~%" (string-join words " ")))
  
  (format #t "4. Recursive function (factorial):~%")
  (format #t "   7! = ~a~%~%" (scheme-factorial 7))
  
  (print-header "✓ ELISP COMPILATION AND EXECUTION")
  
  (format #t "1. Basic arithmetic in Elisp:~%")
  (let ((result1 (compile '(+ 15 25 35) #:from 'elisp #:to 'value))
        (result2 (compile '(* 7 8 9) #:from 'elisp #:to 'value)))
    (format #t "   (+ 15 25 35) = ~a~%" result1)
    (format #t "   (* 7 8 9) = ~a~%~%" result2))
  
  (format #t "2. Function definition and execution:~%")
  (compile '(defun elisp-power (base exp)
              (if (<= exp 0) 
                  1 
                  (* base (elisp-power base (- exp 1)))))
           #:from 'elisp #:to 'value)
  (let ((result (compile '(elisp-power 3 4) #:from 'elisp #:to 'value)))
    (format #t "   (elisp-power 3 4) = ~a~%~%" result))
  
  (format #t "3. List processing with mapcar:~%")
  (let ((result (compile '(mapcar (lambda (x) (+ x 10)) '(1 2 3 4 5))
                         #:from 'elisp #:to 'value)))
    (format #t "   Add 10 to each: ~a~%~%" result))
  
  (format #t "4. String operations:~%")
  (let ((result (compile '(concat "FreeBSD" " + " "Guile" " = " "Awesome!")
                         #:from 'elisp #:to 'value)))
    (format #t "   Concatenated: \"~a\"~%~%" result))
  
  (format #t "5. Conditional logic:~%")
  (compile '(defun elisp-max (a b) (if (> a b) a b)) #:from 'elisp #:to 'value)
  (let ((result1 (compile '(elisp-max 42 37) #:from 'elisp #:to 'value))
        (result2 (compile '(elisp-max 15 28) #:from 'elisp #:to 'value)))
    (format #t "   (elisp-max 42 37) = ~a~%" result1)
    (format #t "   (elisp-max 15 28) = ~a~%~%" result2))
  
  (print-header "✓ CROSS-LANGUAGE COMPARISON")
  
  (format #t "Identical operations in different language contexts:~%~%")
  
  (format #t "Mathematical operations:~%")
  (let ((scheme-result (+ (* 2 5) (* 3 7)))
        (elisp-result (compile '(+ (* 2 5) (* 3 7)) #:from 'elisp #:to 'value)))
    (format #t "  Scheme: (+ (* 2 5) (* 3 7)) = ~a~%" scheme-result)
    (format #t "  Elisp:  (+ (* 2 5) (* 3 7)) = ~a~%~%" elisp-result))
  
  (format #t "List transformations:~%")
  (let ((data '(2 4 6 8))
        (elisp-data '(2 4 6 8)))
    (format #t "  Scheme map: ~a~%" (map (lambda (x) (/ x 2)) data))
    (format #t "  Elisp mapcar: ~a~%~%" 
            (compile `(mapcar (lambda (x) (/ x 2)) ',elisp-data) #:from 'elisp #:to 'value)))
  
  (print-header "✓ COMPILATION PIPELINE DEMONSTRATION")
  
  (format #t "Demonstrating Guile's compilation stages:~%~%")
  
  (let ((elisp-code '(mapcar (lambda (x) (* x x)) '(1 2 3))))
    (format #t "Original Elisp code: ~s~%~%" elisp-code)
    
    (format #t "Stage 1 - Parse to Tree-IL:~%")
    (let ((tree-il (compile elisp-code #:from 'elisp #:to 'tree-il)))
      (format #t "~a~%~%" tree-il))
    
    (format #t "Stage 2 - Compile to bytecode:~%")
    (let ((bytecode (compile elisp-code #:from 'elisp #:to 'bytecode)))
      (format #t "Bytecode: ~a bytes~%~%" (bytevector-length bytecode)))
    
    (format #t "Stage 3 - Execute:~%")
    (let ((result (compile elisp-code #:from 'elisp #:to 'value)))
      (format #t "Final result: ~a~%~%" result)))
  
  (print-header "✓ PERFORMANCE COMPARISON")
  
  (format #t "Computing same recursive function in both languages:~%~%")
  
  ;; Scheme version timing
  (let ((start-time (get-internal-real-time)))
    (let ((result (scheme-factorial 15)))
      (let ((scheme-time (- (get-internal-real-time) start-time)))
        (format #t "Scheme factorial(15):~%")
        (format #t "  Result: ~a~%" result)
        (format #t "  Time: ~a internal units~%~%" scheme-time))))
  
  ;; Elisp version timing  
  (compile '(defun elisp-factorial (n) 
              (if (<= n 1) 1 (* n (elisp-factorial (- n 1)))))
           #:from 'elisp #:to 'value)
  
  (let ((start-time (get-internal-real-time)))
    (let ((result (compile '(elisp-factorial 15) #:from 'elisp #:to 'value)))
      (let ((elisp-time (- (get-internal-real-time) start-time)))
        (format #t "Elisp factorial(15):~%")
        (format #t "  Result: ~a~%" result)
        (format #t "  Time: ~a internal units~%~%" elisp-time))))
  
  (print-header "✗ LIMITATIONS DISCOVERED")
  
  (format #t "Languages NOT working in Guile 2.2.7:~%")
  (format #t "  ✗ ECMAScript/JavaScript - Parser errors~%")
  (format #t "  ✗ Brainfuck - Language not available~%")
  (format #t "  ✗ Advanced Elisp features (message function, etc.)~%~%")
  
  (format #t "Working alternatives:~%")
  (format #t "  ✓ Full Scheme (native) support~%")
  (format #t "  ✓ Core Elisp compilation and execution~%") 
  (format #t "  ✓ Cross-language function compilation~%")
  (format #t "  ✓ Complete compilation pipeline access~%")
  
  (print-header "🎉 DEMONSTRATION COMPLETE")
  
  (format #t "SUMMARY OF ACHIEVEMENTS:~%~%")
  (format #t "✅ Native Scheme execution with full feature set~%")
  (format #t "✅ Elisp compilation from source to bytecode~%")
  (format #t "✅ Cross-language arithmetic and list operations~%")
  (format #t "✅ Function definitions and recursive calls~%")
  (format #t "✅ String manipulation in both languages~%")
  (format #t "✅ Performance comparison capabilities~%")
  (format #t "✅ Complete compilation pipeline exposure~%")
  (format #t "✅ Tree-IL intermediate representation access~%")
  (format #t "✅ Bytecode generation and inspection~%")
  (format #t "✅ Runtime execution measurement~%~%")
  
  (format #t "This demonstrates Guile's powerful multilanguage~%")
  (format #t "compilation architecture running on FreeBSD 14.3!~%")
  (print-separator))

;; Run main function
(main)