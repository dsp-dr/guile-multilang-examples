#!/usr/bin/env guile
!#

;;; working-demo.scm --- Working multilanguage demo for Guile

(use-modules (system base compile)
             (ice-9 format))

;; Define functions at top level
(define (fact n)
  (if (<= n 1) 1 (* n (fact (- n 1)))))

(define (print-header title)
  "Print a formatted header for demo sections."
  (let ((line (make-string (+ (string-length title) 8) #\=)))
    (format #t "~%~a~%" line)
    (format #t "=== ~a ===~%" title)
    (format #t "~a~%~%" line)))

(define (main)
  "Main demo function."
  (format #t "~%")
  (print-header "GUILE MULTILANGUAGE DEMONSTRATION")
  (format #t "System: FreeBSD 14.3-RELEASE amd64~%")
  (format #t "Guile Version: ~a~%~%" (version))
  
  ;; Scheme Demo
  (print-header "SCHEME DEMONSTRATION")
  (format #t "1. Basic arithmetic: (+ 10 20 30) = ~a~%" (+ 10 20 30))
  
  (format #t "2. List operations:~%")
  (let* ((numbers '(1 2 3 4 5))
         (doubled (map (lambda (x) (* x 2)) numbers)))
    (format #t "   Original: ~a~%" numbers)
    (format #t "   Doubled: ~a~%~%" doubled))
  
  (format #t "3. Factorial function: (fact 6) = ~a~%~%" (fact 6))
  
  ;; Elisp Demo  
  (print-header "ELISP DEMONSTRATION")
  
  (format #t "1. Basic arithmetic in Elisp:~%")
  (let ((result (compile '(+ 10 20 30) #:from 'elisp #:to 'value)))
    (format #t "   (+ 10 20 30) = ~a~%~%" result))
  
  (format #t "2. Defining and calling Elisp function:~%")
  (compile '(defun elisp-square (x) (* x x)) #:from 'elisp #:to 'value)
  (let ((result (compile '(elisp-square 8) #:from 'elisp #:to 'value)))
    (format #t "   (elisp-square 8) = ~a~%~%" result))
  
  (format #t "3. List operations in Elisp:~%")
  (let ((result (compile '(mapcar (lambda (x) (* x x)) '(1 2 3 4 5))
                         #:from 'elisp #:to 'value)))
    (format #t "   Squares: ~a~%~%" result))
  
  (format #t "4. String operations:~%")
  (let ((result (compile '(concat "Hello" " " "World" " " "from" " " "Elisp!")
                         #:from 'elisp #:to 'value)))
    (format #t "   Concatenated: \"~a\"~%~%" result))
  
  ;; Cross-language comparison
  (print-header "CROSS-LANGUAGE COMPARISON")
  (format #t "Same operations in different languages:~%~%")
  
  (format #t "Addition:~%")
  (format #t "  Scheme: ~a~%" (+ 5 10 15))
  (format #t "  Elisp:  ~a~%~%" (compile '(+ 5 10 15) #:from 'elisp #:to 'value))
  
  (format #t "List doubling:~%")
  (format #t "  Scheme: ~a~%" (map (lambda (x) (* x 2)) '(1 2 3)))
  (format #t "  Elisp:  ~a~%~%" 
          (compile '(mapcar (lambda (x) (* x 2)) '(1 2 3)) #:from 'elisp #:to 'value))
  
  ;; Compilation internals demo
  (print-header "COMPILATION INTERNALS")
  (format #t "Elisp code: (+ 1 2 3 4 5)~%")
  (format #t "Tree-IL: ~a~%~%" 
          (compile '(+ 1 2 3 4 5) #:from 'elisp #:to 'tree-il))
  
  (let ((bytecode (compile '(+ 1 2 3 4 5) #:from 'elisp #:to 'bytecode)))
    (format #t "Bytecode: ~a bytes generated~%" (bytevector-length bytecode)))
  
  (format #t "Result: ~a~%~%" 
          (compile '(+ 1 2 3 4 5) #:from 'elisp #:to 'value))
  
  (print-header "DEMO COMPLETE")
  (format #t "Successfully demonstrated:~%")
  (format #t "✓ Native Scheme execution~%")
  (format #t "✓ Elisp compilation and execution~%") 
  (format #t "✓ Cross-language comparisons~%")
  (format #t "✓ Compilation stages (Tree-IL, bytecode, execution)~%")
  (format #t "✓ String, numeric, and list operations~%")
  (format #t "✓ Function definitions and calls~%")
  (newline))

;; Run main function
(main)