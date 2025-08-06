#!/usr/bin/env guile
!#

;;; brainfuck-demo.scm --- Brainfuck demonstration in Guile

(use-modules (system base compile)
             (ice-9 format))

(define (test-brainfuck)
  "Test Brainfuck compilation and execution."
  (format #t "=== BRAINFUCK DEMONSTRATION ===~%~%")
  
  (format #t "1. Hello World in Brainfuck:~%")
  (format #t "Code: ++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.~%")
  (format #t "Output: ")
  (let ((result (compile "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>." 
                         #:from 'brainfuck #:to 'value)))
    (format #t "~%~%"))
  
  (format #t "2. Simple number output (72 = 'H'):~%")
  (format #t "Code: +++++++++[>++++++++<-]>.~%")
  (format #t "Output: ")
  (let ((result (compile "+++++++++[>++++++++<-]>." #:from 'brainfuck #:to 'value)))
    (format #t "~%~%"))
  
  (format #t "3. Compilation stages for Brainfuck:~%")
  (let ((simple-code "++++++++[>++++<-]>."))
    (format #t "Source: ~a~%~%" simple-code)
    
    (format #t "Tree-IL representation:~%")
    (let ((tree-il (compile simple-code #:from 'brainfuck #:to 'tree-il)))
      (format #t "~a~%~%" tree-il))
    
    (format #t "Executing: ")
    (compile simple-code #:from 'brainfuck #:to 'value)
    (format #t "~%~%"))
  
  (format #t "✅ Brainfuck support confirmed in Guile 2.2.7!~%"))

;; Run the test
(test-brainfuck)