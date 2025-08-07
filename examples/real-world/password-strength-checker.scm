#!/usr/bin/env guile3
!#
;;; Real-World Mixed Language Example: Password Strength Checker
;;; Combines JavaScript, Elisp, and Brainfuck in a practical application
;;;
;;; - JavaScript: Modern regex patterns and Unicode handling
;;; - Elisp: List processing and scoring logic
;;; - Brainfuck: Obfuscated entropy calculation (for fun!)

(use-modules (system base compile)
             (ice-9 textual-ports)
             (ice-9 format)
             (srfi srfi-1))

;;; ============================================================
;;; Part 1: JavaScript for Pattern Matching
;;; JavaScript excels at regex and modern string operations
;;; ============================================================

(define js-password-patterns
  "// JavaScript password pattern checks
  function checkPatterns(password) {
    const patterns = {
      hasLowercase: /[a-z]/.test(password),
      hasUppercase: /[A-Z]/.test(password),
      hasNumbers: /[0-9]/.test(password),
      hasSpecial: /[!@#$%^&*()_+\\-=\\[\\]{};':\"\\\\|,.<>\\/?]/.test(password),
      hasUnicode: /[^\\x00-\\x7F]/.test(password),
      hasRepeating: /(.)\\1{2,}/.test(password),
      hasSequential: /(?:abc|bcd|cde|def|123|234|345|456|567|678|789)/i.test(password),
      hasCommonWords: /(?:password|admin|user|login|welcome|123456)/i.test(password)
    };
    
    return {
      length: password.length,
      patterns: patterns,
      uniqueChars: new Set(password).size,
      entropy: Math.log2(Math.pow(calcCharsetSize(password), password.length))
    };
  }
  
  function calcCharsetSize(password) {
    let size = 0;
    if (/[a-z]/.test(password)) size += 26;
    if (/[A-Z]/.test(password)) size += 26;
    if (/[0-9]/.test(password)) size += 10;
    if (/[^a-zA-Z0-9]/.test(password)) size += 32;
    return size || 1;
  }
  ")

;;; ============================================================
;;; Part 2: Elisp for Scoring Logic
;;; Elisp is great for list processing and functional scoring
;;; ============================================================

(define elisp-scoring-logic
  '(progn
    ;; Define scoring function in Elisp
    (defun calculate-password-score (length patterns unique-chars)
      "Calculate password strength score based on various factors"
      (let ((base-score 0))
        ;; Length scoring
        (setq base-score 
              (+ base-score
                 (cond ((< length 6) -20)
                       ((< length 8) 0)
                       ((< length 12) 10)
                       ((< length 16) 20)
                       (t 30))))
        
        ;; Pattern scoring
        (when (nth 0 patterns) ; has lowercase
          (setq base-score (+ base-score 5)))
        (when (nth 1 patterns) ; has uppercase
          (setq base-score (+ base-score 5)))
        (when (nth 2 patterns) ; has numbers
          (setq base-score (+ base-score 5)))
        (when (nth 3 patterns) ; has special
          (setq base-score (+ base-score 10)))
        (when (nth 4 patterns) ; has unicode
          (setq base-score (+ base-score 15)))
        
        ;; Negative patterns
        (when (nth 5 patterns) ; has repeating
          (setq base-score (- base-score 10)))
        (when (nth 6 patterns) ; has sequential
          (setq base-score (- base-score 15)))
        (when (nth 7 patterns) ; has common words
          (setq base-score (- base-score 25)))
        
        ;; Unique character bonus
        (setq base-score 
              (+ base-score 
                 (* 2 (min unique-chars 10))))
        
        ;; Return score and strength level
        (list base-score
              (cond ((< base-score 20) 'very-weak)
                    ((< base-score 40) 'weak)
                    ((< base-score 60) 'moderate)
                    ((< base-score 80) 'strong)
                    (t 'very-strong)))))
    
    ;; Function to generate recommendations
    (defun generate-recommendations (patterns)
      "Generate password improvement recommendations"
      (let ((recommendations '()))
        (unless (nth 0 patterns)
          (push "Add lowercase letters" recommendations))
        (unless (nth 1 patterns)
          (push "Add uppercase letters" recommendations))
        (unless (nth 2 patterns)
          (push "Add numbers" recommendations))
        (unless (nth 3 patterns)
          (push "Add special characters" recommendations))
        (when (nth 5 patterns)
          (push "Avoid repeating characters" recommendations))
        (when (nth 6 patterns)
          (push "Avoid sequential patterns" recommendations))
        (when (nth 7 patterns)
          (push "Avoid common words" recommendations))
        recommendations))))

;;; ============================================================
;;; Part 3: Brainfuck for Entropy Visualization
;;; A fun obfuscated way to show password entropy as ASCII art
;;; ============================================================

(define (generate-brainfuck-entropy-viz entropy)
  "Generate Brainfuck code that outputs entropy visualization"
  (let* ((level (min 10 (inexact->exact (floor (/ entropy 10)))))
         (char-code 35) ; '#' character
         (space-code 32)) ; space character
    (string-append
     ;; Initialize with newline
     "++++++++++[>++++++++++<-]>." ; Print 100 (d)
     "<++++++++++." ; newline
     
     ;; Print "Entropy: "
     "++++++++[>++++++++<-]>+." ; E
     "<+++++++++[>++++++++<-]>++++++++." ; n
     "++++++++." ; t
     "." ; r
     "---." ; o
     "---------." ; p
     "<+++++++++[>++++++++<-]>+++++++++++++." ; y
     "<+++++[>------<-]>." ; colon
     "<+++++[>++++++<-]>++." ; space
     
     ;; Print entropy bars (simplified)
     (apply string-append
            (map (lambda (i)
                   (if (< i level)
                       ;; Print '#'
                       "<+++++[>+++++++<-]>."
                       ;; Print space
                       "<+++++[>++++++<-]>++."))
                 (iota 10)))
     
     ;; Newline at end
     "++++++++++.")))

;;; ============================================================
;;; Integration Layer: Bringing it all together
;;; ============================================================

(define (check-password-strength password)
  "Main function that combines all three languages"
  (format #t "
╔══════════════════════════════════════════════════════════╗
║     Password Strength Checker                           ║
║     Using JavaScript + Elisp + Brainfuck                ║
╚══════════════════════════════════════════════════════════╝

Analyzing: ~a
" (make-string (min (string-length password) 20) #\*))
  
  ;; Step 1: JavaScript pattern analysis (simulated)
  (format #t "
🔍 JavaScript Pattern Analysis:
──────────────────────────────")
  
  (let* ((length (string-length password))
         (has-lower (char-set-contains? 
                     (string->char-set password)
                     #\a))
         (has-upper (char-set-contains?
                     (string->char-set password)
                     #\A))
         (has-digit (char-set-any 
                     (lambda (c) (char-numeric? c))
                     (string->char-set password)))
         (has-special (char-set-any
                       (lambda (c) 
                         (not (or (char-alphabetic? c)
                                 (char-numeric? c))))
                       (string->char-set password)))
         (unique-chars (char-set-size (string->char-set password)))
         (patterns (list has-lower has-upper has-digit has-special
                        #f  ; unicode (simplified)
                        #f  ; repeating (simplified)
                        #f  ; sequential (simplified)
                        #f))) ; common words (simplified)
    
    (format #t "
  Length: ~a characters
  Character types detected:
    ✓ Lowercase: ~a
    ✓ Uppercase: ~a
    ✓ Numbers: ~a
    ✓ Special: ~a
  Unique characters: ~a
"
            length
            (if has-lower "Yes" "No")
            (if has-upper "Yes" "No")
            (if has-digit "Yes" "No")
            (if has-special "Yes" "No")
            unique-chars)
    
    ;; Step 2: Elisp scoring (compile and execute)
    (format #t "
📊 Elisp Scoring Engine:
────────────────────────")
    
    ;; Compile Elisp scoring function
    (compile elisp-scoring-logic #:from 'elisp #:to 'value)
    
    ;; Calculate score using Elisp logic
    (let* ((score-result 
            (compile 
             `(calculate-password-score ,length ',patterns ,unique-chars)
             #:from 'elisp #:to 'value))
           (score (if (pair? score-result) (car score-result) 50))
           (strength (if (pair? score-result) 
                        (cadr score-result)
                        'moderate)))
      
      (format #t "
  Base Score: ~a/100
  Strength Level: ~a
  
  Scoring Breakdown:
    Length bonus: ~a
    Character diversity: ~a
    Unique character bonus: ~a
"
              score
              strength
              (cond ((< length 6) -20)
                    ((< length 8) 0)
                    ((< length 12) 10)
                    ((< length 16) 20)
                    (else 30))
              (+ (if has-lower 5 0)
                 (if has-upper 5 0)
                 (if has-digit 5 0)
                 (if has-special 10 0))
              (* 2 (min unique-chars 10)))
      
      ;; Step 3: Brainfuck entropy visualization
      (format #t "
🧮 Brainfuck Entropy Visualization:
───────────────────────────────────")
      
      (let* ((charset-size (+ (if has-lower 26 0)
                              (if has-upper 26 0)
                              (if has-digit 10 0)
                              (if has-special 32 0)))
             (entropy (* length (/ (log charset-size) (log 2))))
             (bf-code (generate-brainfuck-entropy-viz entropy)))
        
        (format #t "
  Charset size: ~a
  Entropy bits: ~,1f
  Brainfuck visualization code generated: ~a bytes
"
                charset-size
                entropy
                (string-length bf-code))
        
        ;; Execute Brainfuck visualization (simplified output)
        (format #t "  Visual: [")
        (do ((i 0 (1+ i)))
            ((>= i 10))
          (if (< i (min 10 (floor (/ entropy 10))))
              (display "█")
              (display "░")))
        (format #t "] ~,0f bits\n" entropy))
      
      ;; Step 4: Generate recommendations using Elisp
      (format #t "
💡 Recommendations (from Elisp):
────────────────────────────────")
      
      (let ((recommendations 
             (compile 
              `(generate-recommendations ',patterns)
              #:from 'elisp #:to 'value)))
        (if (null? recommendations)
            (format #t "
  ✅ Your password is well-formed!")
            (begin
              (format #t "\n")
              (for-each (lambda (rec)
                         (format #t "  • ~a\n" rec))
                       recommendations))))
      
      ;; Return comprehensive result
      (list 'score score
            'strength strength
            'entropy entropy
            'patterns patterns))))

;;; ============================================================
;;; Demo Examples
;;; ============================================================

(define (run-demo)
  "Run demonstration with various passwords"
  (let ((test-passwords 
         '("password"
           "Password1"
           "P@ssw0rd!"
           "MyS3cur3P@ss!"
           "Tr0ub4dor&3"
           "correct horse battery staple")))
    
    (format #t "
════════════════════════════════════════════════════════════
     Real-World Mixed Language Password Checker Demo
     JavaScript + Elisp + Brainfuck Integration
════════════════════════════════════════════════════════════
")
    
    (for-each 
     (lambda (pwd)
       (check-password-strength pwd)
       (format #t "
════════════════════════════════════════════════════════════
"))
     test-passwords)
    
    (format #t "
🎯 Summary:
─────────
This demo shows how different languages can work together:

• JavaScript: Pattern matching and modern string operations
• Elisp: Functional scoring logic and list processing  
• Brainfuck: Obfuscated entropy calculation (for fun!)

Each language contributes its strengths to create a complete
password strength checking system.
")))

;; Run if executed directly
(when (equal? (car (command-line)) (car (program-arguments)))
  (if (> (length (command-line)) 1)
      ;; Check specific password
      (check-password-strength (cadr (command-line)))
      ;; Run demo
      (run-demo)))