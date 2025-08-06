#!/usr/bin/env guile3
!#

;;; compile-elisp.scm --- Elisp compilation script for Guile

(use-modules (language elisp spec)
             (system base compile)
             (ice-9 format)
             (ice-9 pretty-print))

(define (compile-elisp-file filename)
  "Compile an Elisp file to Guile bytecode."
  (let ((output-file (string-append filename ".go")))
    (format #t "Compiling ~a to ~a...~%" filename output-file)
    (compile-file filename 
                  #:from 'elisp 
                  #:to 'bytecode
                  #:output-file output-file)
    (format #t "Compilation complete!~%")))

(define (compile-elisp-string code)
  "Compile Elisp code string and return the result."
  (compile code #:from 'elisp #:to 'value))

(define (test-elisp-compilation)
  "Test various Elisp compilation scenarios."
  (format #t "~%Testing Elisp compilation in Guile:~%")
  (format #t "=====================================~%~%")
  
  ;; Test 1: Simple function
  (format #t "Test 1: Simple addition function~%")
  (catch #t
    (lambda ()
      (compile-elisp-string '(defun add (a b) (+ a b)))
      (let ((result (compile-elisp-string '(add 3 4))))
        (format #t "Result: add(3, 4) = ~a~%~%" result)))
    (lambda (key . args)
      (format #t "ERROR in test 1: ~a ~a~%~%" key args)))
  
  ;; Test 2: List operations
  (format #t "Test 2: List operations~%")
  (let ((result (compile-elisp-string 
                 '(mapcar (lambda (x) (* x 2)) '(1 2 3 4 5)))))
    (format #t "Doubled list: ~a~%~%" result))
  
  ;; Test 3: Conditional logic
  (format #t "Test 3: Conditional logic~%")
  (compile-elisp-string '(defun my-max (a b) (if (> a b) a b)))
  (let ((result (compile-elisp-string '(my-max 10 25))))
    (format #t "max(10, 25) = ~a~%~%" result))
  
  ;; Test 4: String manipulation
  (format #t "Test 4: String manipulation~%")
  (let ((result (compile-elisp-string '(concat "Hello" " " "World"))))
    (format #t "Concatenated string: ~a~%~%" result)))

(define (show-compilation-stages code)
  "Show different compilation stages for Elisp code."
  (format #t "~%Compilation stages for: ~a~%" code)
  (format #t "=========================================~%~%")
  
  ;; Stage 1: Parse to syntax
  (format #t "Stage 1: Parsed syntax~%")
  (let ((syntax (compile code #:from 'elisp #:to 'tree-il)))
    (pretty-print syntax)
    (newline))
  
  ;; Stage 2: Compile to bytecode
  (format #t "Stage 2: Bytecode compilation~%")
  (let ((bytecode (compile code #:from 'elisp #:to 'bytecode)))
    (format #t "Bytecode generated: ~a~%~%" bytecode))
  
  ;; Stage 3: Execute
  (format #t "Stage 3: Execution result~%")
  (let ((result (compile code #:from 'elisp #:to 'value)))
    (format #t "Result: ~a~%~%" result)))

(define (main args)
  "Main entry point."
  (cond
   ((null? (cdr args))
    (format #t "Usage: ~a <elisp-file> | --test | --stages~%" (car args))
    (format #t "  <elisp-file>  Compile an Elisp file to bytecode~%")
    (format #t "  --test        Run compilation tests~%")
    (format #t "  --stages      Show compilation stages demo~%"))
   ((string=? (cadr args) "--test")
    (test-elisp-compilation))
   ((string=? (cadr args) "--stages")
    (show-compilation-stages '(defun square (x) (* x x)))
    (show-compilation-stages '(+ 1 2 3 4 5)))
   (else
    (compile-elisp-file (cadr args)))))

;; Run main if executed as script
(when (batch-mode?)
  (main (command-line)))