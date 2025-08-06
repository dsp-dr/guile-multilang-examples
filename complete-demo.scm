#!/usr/bin/env guile
!#

;;; complete-demo.scm --- Complete working multilanguage demo for FreeBSD

(use-modules (system base compile)
             (ice-9 format)
             (rnrs bytevectors))

(define (scheme-factorial n)
  (if (<= n 1) 1 (* n (scheme-factorial (- n 1)))))

(define (print-header title)
  (format #t "~%~a~%" (make-string 60 #\=))
  (format #t "~a~%" title)
  (format #t "~a~%~%" (make-string 60 #\=)))

(define (main)
  (print-header "🚀 GUILE MULTILANGUAGE DEMO - FreeBSD 14.3")
  (format #t "Guile Version: ~a~%" (version))
  (format #t "Architecture: amd64~%")
  
  (print-header "✅ SCHEME (NATIVE) DEMONSTRATION")
  
  (format #t "1. Mathematics: (+ 10 20 (* 3 5)) = ~a~%" (+ 10 20 (* 3 5)))
  (format #t "2. List operations: ~a~%" (map (lambda (x) (* x 3)) '(1 2 3 4 5)))
  (format #t "3. Factorial: 6! = ~a~%~%" (scheme-factorial 6))
  
  (print-header "✅ ELISP COMPILATION & EXECUTION")
  
  (format #t "1. Basic arithmetic:~%")
  (let ((result (compile '(+ 5 10 15 20) #:from 'elisp #:to 'value)))
    (format #t "   Elisp: (+ 5 10 15 20) = ~a~%~%" result))
  
  (format #t "2. Function definition:~%")
  (compile '(defun triple (x) (* x 3)) #:from 'elisp #:to 'value)
  (let ((result (compile '(triple 12) #:from 'elisp #:to 'value)))
    (format #t "   (triple 12) = ~a~%~%" result))
  
  (format #t "3. List processing:~%")
  (let ((result (compile '(mapcar (lambda (x) (* x x)) '(2 3 4 5))
                         #:from 'elisp #:to 'value)))
    (format #t "   Squares: ~a~%~%" result))
  
  (format #t "4. String concatenation:~%")
  (let ((result (compile '(concat "Hello" " " "World" " " "2025!")
                         #:from 'elisp #:to 'value)))
    (format #t "   Result: \"~a\"~%~%" result))
  
  (print-header "🔄 CROSS-LANGUAGE COMPARISON")
  
  (format #t "Same computation in both languages:~%~%")
  
  (let ((scheme-sum (+ 1 2 3 4 5 6 7 8 9 10))
        (elisp-sum (compile '(+ 1 2 3 4 5 6 7 8 9 10) #:from 'elisp #:to 'value)))
    (format #t "Sum 1-10:  Scheme=~a, Elisp=~a~%~%" scheme-sum elisp-sum))
  
  (let ((scheme-list (map (lambda (x) (+ x 100)) '(1 2 3)))
        (elisp-list (compile '(mapcar (lambda (x) (+ x 100)) '(1 2 3)) 
                            #:from 'elisp #:to 'value)))
    (format #t "Add 100:   Scheme=~a, Elisp=~a~%~%" scheme-list elisp-list))
  
  (print-header "⚙️ COMPILATION PIPELINE")
  
  (let ((code '(+ (* 2 3) (* 4 5))))
    (format #t "Elisp source: ~s~%~%" code)
    
    (format #t "Tree-IL: ~a~%~%" 
            (compile code #:from 'elisp #:to 'tree-il))
    
    (let ((bytecode (compile code #:from 'elisp #:to 'bytecode)))
      (format #t "Bytecode: ~a bytes generated~%~%" (bytevector-length bytecode)))
    
    (format #t "Result: ~a~%~%" 
            (compile code #:from 'elisp #:to 'value)))
  
  (print-header "📊 LANGUAGE SUPPORT STATUS")
  
  (format #t "✅ WORKING LANGUAGES:~%")
  (format #t "  • Scheme (native) - Full support~%")
  (format #t "  • Elisp - Core compilation working~%")
  (format #t "    - Arithmetic: + - * operators~%") 
  (format #t "    - Functions: defun, lambda~%")
  (format #t "    - Lists: mapcar, list operations~%")
  (format #t "    - Strings: concat~%")
  (format #t "    - Control: if, conditionals~%~%")
  
  (format #t "❌ LIMITED/UNAVAILABLE:~%")
  (format #t "  • ECMAScript - Parser issues~%")
  (format #t "  • Brainfuck - Not available~%")
  (format #t "  • Some Elisp functions (message, /)~%~%")
  
  (print-header "🎯 DEMONSTRATION SUMMARY")
  
  (format #t "SUCCESSFULLY DEMONSTRATED:~%~%")
  (format #t "🔸 Multi-language compilation pipeline~%")
  (format #t "🔸 Native Scheme execution~%")
  (format #t "🔸 Elisp-to-bytecode compilation~%")
  (format #t "🔸 Cross-language result consistency~%")
  (format #t "🔸 Tree-IL intermediate representation~%")
  (format #t "🔸 Bytecode generation~%")
  (format #t "🔸 Runtime function definition~%")
  (format #t "🔸 Higher-order functions~%")
  (format #t "🔸 String and list manipulation~%~%")
  
  (format #t "This showcases Guile's powerful multilanguage~%")
  (format #t "architecture running successfully on FreeBSD 14.3!~%"))

;; Execute demonstration
(main)