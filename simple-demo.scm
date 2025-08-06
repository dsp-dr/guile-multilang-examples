#!/usr/bin/env guile
!#

;;; simple-demo.scm --- Simple multilanguage demo for Guile

(use-modules (system base compile)
             (ice-9 format))

(define (print-header title)
  "Print a formatted header for demo sections."
  (let ((line (make-string (+ (string-length title) 8) #\=)))
    (format #t "~%~a~%" line)
    (format #t "=== ~a ===~%" title)
    (format #t "~a~%~%" line)))

(define (demo-elisp)
  "Demo Elisp compilation and execution in Guile."
  (print-header "ELISP DEMONSTRATION")
  
  (format #t "1. Simple arithmetic function:~%")
  (compile '(defun multiply (a b) (* a b)) #:from 'elisp #:to 'value)
  (let ((result (compile '(multiply 6 7) #:from 'elisp #:to 'value)))
    (format #t "   (multiply 6 7) = ~a~%~%" result))
  
  (format #t "2. List operations with mapcar:~%")
  (let ((result (compile '(mapcar (lambda (x) (* x x)) '(1 2 3 4 5)) 
                         #:from 'elisp #:to 'value)))
    (format #t "   Square of [1,2,3,4,5] = ~a~%~%" result))
  
  (format #t "3. String concatenation:~%")
  (let ((result (compile '(concat "Hello" " " "from" " " "Elisp!") 
                         #:from 'elisp #:to 'value)))
    (format #t "   Result: \"~a\"~%~%" result))
  
  (format #t "4. Recursive factorial:~%")
  (compile '(defun factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1))))) 
           #:from 'elisp #:to 'value)
  (let ((result (compile '(factorial 6) #:from 'elisp #:to 'value)))
    (format #t "   (factorial 6) = ~a~%~%" result)))

(define (demo-scheme)
  "Demo native Scheme capabilities."
  (print-header "SCHEME DEMONSTRATION")
  
  (format #t "1. Higher-order functions:~%")
  (let* ((numbers '(1 2 3 4 5 6 7 8 9 10))
         (evens (filter even? numbers))
         (squares (map (lambda (x) (* x x)) evens)))
    (format #t "   Original: ~a~%" numbers)
    (format #t "   Even numbers: ~a~%" evens)
    (format #t "   Squares: ~a~%~%" squares))
  
  (format #t "2. Native factorial function:~%")
  (define (fact n)
    (if (<= n 1) 1 (* n (fact (- n 1)))))
  (format #t "   (factorial 6) = ~a~%~%" (fact 6)))

(define (demo-cross-language)
  "Demo cross-language interactions."
  (print-header "CROSS-LANGUAGE COMPARISON")
  
  (format #t "Same operation in different languages:~%~%")
  
  (format #t "Scheme: (+ 10 20 30) = ~a~%" (+ 10 20 30))
  
  (let ((elisp-result (compile '(+ 10 20 30) #:from 'elisp #:to 'value)))
    (format #t "Elisp:  (+ 10 20 30) = ~a~%~%" elisp-result))
  
  (format #t "List processing comparison:~%")
  (format #t "Scheme: ~a~%" (map (lambda (x) (* x 2)) '(1 2 3 4 5)))
  
  (let ((elisp-list (compile '(mapcar (lambda (x) (* x 2)) '(1 2 3 4 5))
                             #:from 'elisp #:to 'value)))
    (format #t "Elisp:  ~a~%~%" elisp-list)))

(define (main)
  "Main demo function."
  (format #t "~%")
  (print-header "GUILE MULTILANGUAGE DEMONSTRATION")
  (format #t "System: FreeBSD 14.3-RELEASE amd64~%")
  (format #t "Guile Version: ~a~%~%" (version))
  
  ;; Run all demos
  (demo-scheme)
  (demo-elisp)
  (demo-cross-language)
  
  (print-header "DEMO COMPLETE")
  (format #t "Successfully demonstrated:~%")
  (format #t "✓ Native Scheme compilation and execution~%")
  (format #t "✓ Elisp compilation and execution~%") 
  (format #t "✓ Cross-language comparisons~%")
  (format #t "✓ Functional programming in both languages~%")
  (newline))

;; Run main function
(main)