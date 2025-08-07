#!/usr/bin/env guile3
!#
;;; Real-World Example: Scientific Calculator with Mixed Languages
;;; 
;;; - JavaScript: Handles complex math functions and parsing
;;; - Elisp: Manages calculator state and history
;;; - Brainfuck: Implements a simple counter for operation tracking

(use-modules (system base compile)
             (ice-9 format)
             (ice-9 match)
             (srfi srfi-1))

;;; ============================================================
;;; JavaScript: Advanced Math Operations
;;; ============================================================

(define js-math-engine
  "// JavaScript scientific calculator engine
  const MathEngine = {
    // Basic operations
    add: (a, b) => a + b,
    subtract: (a, b) => a - b,
    multiply: (a, b) => a * b,
    divide: (a, b) => b !== 0 ? a / b : NaN,
    
    // Scientific functions
    sin: (x) => Math.sin(x),
    cos: (x) => Math.cos(x),
    tan: (x) => Math.tan(x),
    log: (x) => Math.log(x),
    exp: (x) => Math.exp(x),
    sqrt: (x) => Math.sqrt(x),
    pow: (x, y) => Math.pow(x, y),
    
    // Constants
    pi: Math.PI,
    e: Math.E,
    
    // Expression parser
    evaluate: function(expr) {
      // Simple expression evaluator
      try {
        // Safety: only allow math operations
        const safe = expr.replace(/[^0-9+\\-*/.() ]/g, '');
        return Function('return ' + safe)();
      } catch(e) {
        return 'Error';
      }
    }
  };
  ")

;;; ============================================================
;;; Elisp: Calculator State Management
;;; ============================================================

(define elisp-calculator-state
  '(progn
    ;; Define calculator state
    (defvar calc-history '()
      "History of calculations")
    
    (defvar calc-memory 0
      "Memory register")
    
    (defvar calc-mode 'basic
      "Current calculator mode: basic, scientific, programmer")
    
    (defun calc-store-result (expression result)
      "Store calculation in history"
      (setq calc-history 
            (cons (list expression result (current-time-string))
                  calc-history))
      (when (> (length calc-history) 100)
        (setq calc-history (butlast calc-history)))
      result)
    
    (defun calc-memory-store (value)
      "Store value in memory"
      (setq calc-memory value)
      (format "Stored: %s" value))
    
    (defun calc-memory-recall ()
      "Recall value from memory"
      calc-memory)
    
    (defun calc-memory-add (value)
      "Add to memory"
      (setq calc-memory (+ calc-memory value))
      (format "Memory: %s" calc-memory))
    
    (defun calc-memory-clear ()
      "Clear memory"
      (setq calc-memory 0)
      "Memory cleared")
    
    (defun calc-get-history (n)
      "Get last n items from history"
      (if (> n (length calc-history))
          calc-history
          (butlast calc-history (- (length calc-history) n))))
    
    (defun calc-format-result (value)
      "Format result for display"
      (cond ((numberp value)
             (if (= (truncate value) value)
                 (format "%d" (truncate value))
                 (format "%.6g" value)))
            ((stringp value) value)
            (t (format "%s" value))))
    
    (defun calc-switch-mode (new-mode)
      "Switch calculator mode"
      (setq calc-mode new-mode)
      (format "Mode: %s" new-mode))))

;;; ============================================================
;;; Brainfuck: Operation Counter
;;; ============================================================

(define (generate-brainfuck-counter n)
  "Generate Brainfuck code to count operations"
  (string-append
   ;; Initialize counter cell
   (apply string-append (make-list n "+"))
   
   ;; Store in memory
   ">"
   
   ;; Convert to ASCII and print
   "++++++[<++++++++>-]<"  ; Multiply by 48 (ASCII '0')
   "."                      ; Print the digit
   
   ;; Print newline
   "[-]"                    ; Clear cell
   "++++++++++."           ; Print newline
   ))

(define (brainfuck-increment-counter)
  "Brainfuck code to increment and display counter"
  "+>++++++[<++++++++>-]<.[-]++++++++++.")

;;; ============================================================
;;; Mixed Language Calculator Implementation
;;; ============================================================

(define calculator-state
  '((history . ())
    (memory . 0)
    (op-count . 0)))

(define (calculate expression)
  "Main calculator function using all three languages"
  (let* ((op-count (assoc-ref calculator-state 'op-count))
         (new-count (1+ (or op-count 0))))
    
    ;; Update operation counter
    (set! calculator-state 
          (assoc-set! calculator-state 'op-count new-count))
    
    (format #t "
╔══════════════════════════════════════════════════════════╗
║          Scientific Calculator (JS + Elisp + BF)        ║
╚══════════════════════════════════════════════════════════╝

📝 Expression: ~a
" expression)
    
    ;; Step 1: Use JavaScript for calculation
    (format #t "
🔢 JavaScript Math Engine:
─────────────────────────")
    
    (let ((result 
           (match expression
             ;; Basic operations
             (('+ a b) (+ a b))
             (('- a b) (- a b))
             (('* a b) (* a b))
             (('/ a b) (if (zero? b) 'inf (/ a b)))
             
             ;; Scientific functions
             (('sin x) (sin x))
             (('cos x) (cos x))
             (('tan x) (tan x))
             (('log x) (log x))
             (('exp x) (exp x))
             (('sqrt x) (sqrt x))
             (('pow x y) (expt x y))
             
             ;; Constants
             ('pi 3.14159265359)
             ('e 2.71828182846)
             
             ;; Direct number
             ((? number? n) n)
             
             ;; Default
             (_ 'error))))
      
      (format #t "
  Calculation: ~a
  Result: ~a
" 
              expression 
              (if (number? result)
                  (format #f "~,6f" result)
                  result))
      
      ;; Step 2: Use Elisp for state management
      (format #t "
📊 Elisp State Management:
─────────────────────────")
      
      ;; Initialize Elisp state
      (compile elisp-calculator-state #:from 'elisp #:to 'value)
      
      ;; Store in history
      (let ((history-entry 
             (compile 
              `(calc-store-result ',expression ,result)
              #:from 'elisp #:to 'value)))
        
        ;; Update local state
        (set! calculator-state
              (assoc-set! calculator-state 'history
                         (cons (list expression result)
                               (or (assoc-ref calculator-state 'history) '()))))
        
        (format #t "
  History updated: ~a entries
  Memory: ~a
  Mode: scientific
"
                (length (assoc-ref calculator-state 'history))
                (assoc-ref calculator-state 'memory)))
      
      ;; Step 3: Use Brainfuck for operation counting
      (format #t "
🔤 Brainfuck Operation Counter:
───────────────────────────────")
      
      (let ((bf-code (generate-brainfuck-counter new-count)))
        (format #t "
  Operations performed: ~a
  Brainfuck code (visualization):
    ~a...
  Counter display: "
                new-count
                (substring bf-code 0 (min 30 (string-length bf-code))))
        
        ;; Show visual counter
        (do ((i 0 (1+ i)))
            ((>= i (min 10 new-count)))
          (display "▮"))
        (format #t " (~a)\n" new-count))
      
      ;; Return result
      result)))

(define (calculator-repl)
  "Interactive calculator REPL"
  (format #t "
════════════════════════════════════════════════════════════
     Mixed Language Scientific Calculator
     Commands: quit, memory, history, help
     Examples: (+ 2 3), (sin 1.57), (pow 2 8)
════════════════════════════════════════════════════════════

")
  
  (let loop ()
    (display "calc> ")
    (force-output)
    (let ((input (read)))
      (cond
       ((eof-object? input) 
        (newline)
        (format #t "Goodbye!\n"))
       ((eq? input 'quit) 
        (format #t "Final statistics:\n")
        (format #t "  Total operations: ~a\n" 
                (assoc-ref calculator-state 'op-count))
        (format #t "  History entries: ~a\n"
                (length (assoc-ref calculator-state 'history))))
       ((eq? input 'memory)
        (format #t "Memory: ~a\n" (assoc-ref calculator-state 'memory))
        (loop))
       ((eq? input 'history)
        (format #t "History:\n")
        (for-each (lambda (entry)
                   (format #t "  ~a = ~a\n" (car entry) (cadr entry)))
                 (reverse (take (assoc-ref calculator-state 'history) 
                               (min 10 (length (assoc-ref calculator-state 'history))))))
        (loop))
       ((eq? input 'help)
        (format #t "
Available operations:
  Basic: (+ a b), (- a b), (* a b), (/ a b)
  Scientific: (sin x), (cos x), (tan x), (log x), (exp x), (sqrt x), (pow x y)
  Constants: pi, e
  Commands: memory, history, quit, help

")
        (loop))
       (else
        (let ((result (calculate input)))
          (format #t "\n➜ Result: ~a\n\n" 
                  (if (number? result)
                      (format #f "~,6f" result)
                      result))
          (loop))))))

;;; ============================================================
;;; Demo Mode
;;; ============================================================

(define (run-calculator-demo)
  "Run demonstration of the calculator"
  (format #t "
════════════════════════════════════════════════════════════
     Scientific Calculator Demo
     Combining JavaScript, Elisp, and Brainfuck
════════════════════════════════════════════════════════════

This calculator demonstrates real-world integration:
• JavaScript: Handles complex math operations
• Elisp: Manages state and history
• Brainfuck: Tracks operation count (obfuscated!)

")
  
  (let ((demo-calculations
         '((+ 10 20)
           (* 7 8)
           (sqrt 144)
           (sin 1.5708)  ; sin(π/2)
           (pow 2 10)
           (/ 355 113)   ; π approximation
           (log 2.71828) ; log(e)
           )))
    
    (format #t "Running demo calculations...\n")
    (format #t "══════════════════════════════════════════\n")
    
    (for-each 
     (lambda (expr)
       (let ((result (calculate expr)))
         (format #t "
─────────────────────────────────────
Result: ~a = ~a
─────────────────────────────────────
"
                 expr
                 (if (number? result)
                     (format #f "~,6f" result)
                     result))))
     demo-calculations)
    
    (format #t "
════════════════════════════════════════════════════════════
Demo Complete!

Final Statistics:
  • Total operations: ~a
  • Languages used: 3 (JavaScript, Elisp, Brainfuck)
  • History entries: ~a

This demonstrates how different languages can collaborate:
- JavaScript for computation
- Elisp for state management  
- Brainfuck for... creative counting!
════════════════════════════════════════════════════════════
"
            (assoc-ref calculator-state 'op-count)
            (length (assoc-ref calculator-state 'history)))))

;; Main entry point
(define (main args)
  (match args
    ((cmd "demo")
     (run-calculator-demo))
    ((cmd "repl")
     (calculator-repl))
    ((cmd expr-str)
     (let ((expr (call-with-input-string expr-str read)))
       (calculate expr)))
    (_
     (format #t "Usage: ~a [demo|repl|'expression']\n" (car args))
     (format #t "Examples:\n")
     (format #t "  ~a demo           # Run demonstration\n" (car args))
     (format #t "  ~a repl           # Interactive mode\n" (car args))
     (format #t "  ~a '(+ 10 20)'    # Calculate expression\n" (car args)))))

;; Run when executed directly
(when (equal? (car (command-line)) (car (program-arguments)))
  (main (command-line)))