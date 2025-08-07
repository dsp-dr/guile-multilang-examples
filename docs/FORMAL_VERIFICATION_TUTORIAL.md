# Formal Verification Tutorial
## Property-Based Testing & TLA+ Specifications for Guile Multilanguage System

### Table of Contents
1. [Introduction](#introduction)
2. [Property-Based Testing Framework](#property-based-testing-framework)
3. [Writing Custom Property Tests](#writing-custom-property-tests)
4. [TLA+ Specifications](#tla-specifications)
5. [Combining Formal Methods](#combining-formal-methods)
6. [Best Practices](#best-practices)
7. [Troubleshooting](#troubleshooting)

---

## Introduction

This tutorial demonstrates how to use formal verification techniques to ensure correctness of the Guile multilanguage compilation system. We combine two powerful approaches:

1. **Property-Based Testing** - Automated testing with randomly generated inputs
2. **TLA+ Specifications** - Mathematical modeling of system behavior

These techniques provide stronger guarantees than traditional unit testing by exploring edge cases and proving invariants.

---

## Property-Based Testing Framework

### Quick Start

Run the existing property tests:

```bash
# Make executable if needed
chmod +x examples/formal-verification/property-tests.scm

# Run all property tests
./examples/formal-verification/property-tests.scm
```

Expected output:
```
╔══════════════════════════════════════════════╗
║  Property-Based Testing for Multilanguage   ║
║         Inspired by dsp.defrecord.com        ║
╚══════════════════════════════════════════════╝

Running property tests...
========================

Property test: 100/100 passed
🎉 All properties verified!
```

### Understanding the Framework

The framework uses SRFI-27 for random generation and tests properties over many inputs:

```scheme
;; Initialize random source
(define *rng* (make-random-source))
(random-source-randomize! *rng*)
(define random-real (random-source-make-reals *rng*))
(define random-int (random-source-make-integers *rng*))
```

### Core Generators

#### Integer Generator
```scheme
(define* (gen-integer #:key (min -1000) (max 1000))
  "Generate a random integer in range"
  (+ min (random-int (- max min))))

;; Usage
(gen-integer)                    ; Random from -1000 to 1000
(gen-integer #:min 0 #:max 100)  ; Random from 0 to 100
```

#### List Generator
```scheme
(define* (gen-list gen-element #:key (length #f))
  "Generate a list of random elements"
  (let ((len (or length (random-int 10))))
    (map (lambda (_) (gen-element)) (iota len))))

;; Usage
(gen-list gen-integer)           ; List of random integers
(gen-list gen-integer #:length 5) ; Exactly 5 integers
```

#### Elisp Expression Generator
```scheme
(define (gen-elisp-arithmetic)
  "Generate random Elisp arithmetic expressions"
  (define ops '(+ - *))
  (define (gen-expr depth)
    (if (or (zero? depth) (< (random-real) 0.3))
        (gen-integer)
        (let ((op (list-ref ops (random-int (length ops))))
              (n-args (+ 2 (random-int 3))))
          (cons op (map (lambda (_) (gen-expr (- depth 1))) 
                       (iota n-args))))))
  (gen-expr 3))

;; Generates expressions like:
;; (+ 42 17)
;; (* (+ 3 5) (- 10 2))
```

---

## Writing Custom Property Tests

### Property Test Structure

A property test verifies that a condition holds for all generated inputs:

```scheme
(define (property-my-invariant)
  "Property: My invariant always holds"
  (let ((iterations 100)
        (passed 0))
    (do ((i 0 (+ i 1)))
        ((= i iterations))
      (let ((input (gen-my-input)))
        (when (test-invariant input)
          (set! passed (+ passed 1)))))
    (format #t "My invariant: ~a/~a passed~%" passed iterations)
    (= passed iterations)))
```

### Example: Testing String Compilation

```scheme
(define (gen-elisp-string)
  "Generate random Elisp string expressions"
  (define chars "abcdefghijklmnopqrstuvwxyz0123456789 ")
  (define (random-string len)
    (list->string
      (map (lambda (_) 
             (string-ref chars (random-int (string-length chars))))
           (iota len))))
  `(concat ,(random-string 10) ,(random-string 10)))

(define (property-string-concatenation)
  "Property: String concatenation preserves content"
  (let ((iterations 50)
        (passed 0))
    (do ((i 0 (+ i 1)))
        ((= i iterations))
      (let* ((expr (gen-elisp-string))
             (result (compile expr #:from 'elisp #:to 'value)))
        (when (string? result)
          (set! passed (+ passed 1)))))
    (format #t "String concatenation: ~a/~a passed~%" passed iterations)
    (= passed iterations)))
```

### Example: Testing List Operations

```scheme
(define (gen-elisp-list-op)
  "Generate Elisp list operations"
  (define ops '(car cdr length))
  (let ((op (list-ref ops (random-int (length ops))))
        (list-expr `(list ,@(gen-list gen-integer #:length 5))))
    (list op list-expr)))

(define (property-list-operations)
  "Property: List operations preserve structure"
  (let ((iterations 75)
        (passed 0))
    (do ((i 0 (+ i 1)))
        ((= i iterations))
      (let* ((expr (gen-elisp-list-op))
             (compiled (with-exception-handler
                        (lambda (e) #f)
                        (lambda () 
                          (compile expr #:from 'elisp #:to 'value))
                        #:unwind? #t)))
        (when compiled
          (set! passed (+ passed 1)))))
    (format #t "List operations: ~a/~a passed~%" passed iterations)
    (>= (/ passed iterations) 0.9))) ; Allow 90% success rate
```

### Adding Properties to Test Suite

Add your property to the main test runner:

```scheme
(define (run-all-properties)
  "Run all property-based tests"
  (let ((results
         (list
          (cons "Arithmetic Preservation" (property-arithmetic-preservation))
          (cons "List Operations" (property-list-operations))
          (cons "String Concatenation" (property-string-concatenation)) ; NEW
          (cons "My Custom Property" (property-my-invariant)))))       ; NEW
    ;; ... rest of runner code
    ))
```

---

## TLA+ Specifications

### What is TLA+?

TLA+ is a formal specification language for describing and verifying concurrent systems. Our specifications model the compilation pipeline mathematically.

### Available Specifications

1. **ElispCompiler.tla** - Models Elisp to bytecode compilation
2. **CompilerInvariants.tla** - Defines system-wide invariants

### Running TLA+ Verification

```bash
# Make verification script executable
chmod +x specifications/verify-specs.sh

# Run TLC model checker
./specifications/verify-specs.sh

# Or manually with TLC
tlc ElispCompiler.tla -config ElispCompiler.cfg
```

### Understanding ElispCompiler.tla

Key components of the specification:

```tla
---- MODULE ElispCompiler ----
EXTENDS Integers, Sequences, TLC

CONSTANTS MaxDepth, MaxListSize

VARIABLES 
    expression,     \* Current expression being compiled
    stage,         \* Compilation stage
    output,        \* Compiled result
    error          \* Error state

\* Type invariant
TypeInvariant == 
    /\ stage \in {"parsing", "analysis", "compilation", "done", "error"}
    /\ output \in [type: {"tree-il", "bytecode", "none"}, value: Any]
    /\ error \in {TRUE, FALSE}

\* Safety property: Compilation always terminates
TerminationProperty ==
    <>[](stage = "done" \/ stage = "error")

\* Liveness property: Valid expressions compile successfully
CompilationSucceeds ==
    []((IsValidExpression(expression) /\ stage = "parsing") => 
       <>(stage = "done" /\ error = FALSE))
====
```

### Writing Custom TLA+ Specifications

Create a new specification for your language feature:

```tla
---- MODULE MyLanguageFeature ----
EXTENDS Integers, Sequences

VARIABLES input, output, state

Init ==
    /\ input \in ValidInputs
    /\ output = <<>>
    /\ state = "ready"

Process ==
    /\ state = "ready"
    /\ state' = "processing"
    /\ output' = Transform(input)
    /\ UNCHANGED input

Complete ==
    /\ state = "processing"
    /\ state' = "done"
    /\ UNCHANGED <<input, output>>

Next == Process \/ Complete

Spec == Init /\ [][Next]_<<input, output, state>>

\* Invariant: Output is always valid
OutputValid == state = "done" => IsValid(output)

\* Property: Processing completes
Eventually == <>(state = "done")
====
```

### Model Configuration (.cfg files)

Configure the model checker parameters:

```cfg
SPECIFICATION Spec
INVARIANT TypeInvariant
INVARIANT OutputValid
PROPERTY TerminationProperty

CONSTANTS
    MaxDepth = 3
    MaxListSize = 5
    ValidInputs = {1, 2, 3}

INIT Init
NEXT Next
```

---

## Combining Formal Methods

### Strategy 1: Specification-Driven Testing

Use TLA+ specifications to generate test cases:

```scheme
(define (test-from-tla-spec)
  "Generate tests from TLA+ state space"
  ;; 1. Parse TLA+ specification
  ;; 2. Extract valid state transitions
  ;; 3. Generate test cases for each transition
  (let ((states (parse-tla-states "ElispCompiler.tla")))
    (for-each 
      (lambda (state)
        (test-state-transition state))
      states)))
```

### Strategy 2: Property Refinement

Start with property tests, then formalize successful properties in TLA+:

```scheme
;; Step 1: Discover property through testing
(define (property-discovered)
  "Property: Discovered through random testing"
  (test-many-inputs ...))

;; Step 2: Formalize in TLA+
;; DiscoveredInvariant == 
;;     [](expression.type = "number" => output.type = "integer")
```

### Strategy 3: Counterexample Analysis

Use TLA+ counterexamples to create regression tests:

```scheme
(define (test-tla-counterexample)
  "Test case from TLA+ counterexample"
  (let ((counterexample '(nested (list (with (deep (structure))))))
    ;; This input caused TLA+ to find a violation
    (test-assert "TLA+ counterexample compiles"
      (compile counterexample #:from 'elisp #:to 'bytecode))))
```

### Strategy 4: Cross-Validation

Verify property test results against TLA+ models:

```scheme
(define (cross-validate-property property-name)
  "Ensure property test matches TLA+ specification"
  (let ((test-result (run-property-test property-name))
        (tla-result (verify-tla-property property-name)))
    (assert (eq? test-result tla-result)
            "Property test and TLA+ disagree!")))
```

---

## Best Practices

### 1. Start Simple
Begin with basic properties before complex invariants:
```scheme
;; Good: Simple, clear property
(property-numbers-are-numbers)

;; Avoid: Complex property with many conditions initially
(property-complex-nested-compilation-with-side-effects)
```

### 2. Use Shrinking
When a property fails, minimize the failing input:
```scheme
(define (shrink-integer n)
  "Shrink integer toward zero"
  (cond
    ((positive? n) (list (- n 1) (quotient n 2)))
    ((negative? n) (list (+ n 1) (quotient n 2)))
    (else '())))

(define (find-minimal-failure property gen-input)
  "Find smallest failing input"
  (let loop ((input (gen-input)))
    (if (not (property input))
        (let ((shrunk (shrink-integer input)))
          (if (null? shrunk)
              input
              (loop (find-if (lambda (x) (not (property x))) shrunk))))
        (loop (gen-input)))))
```

### 3. Document Assumptions
Clearly state what properties assume:
```scheme
(define (property-commutative-addition)
  "Property: Addition is commutative
   Assumes: Integer overflow doesn't occur
   Range: -1000 to 1000"
  ...)
```

### 4. Test Generators Separately
Verify your generators produce expected distributions:
```scheme
(define (test-generator-distribution)
  "Verify generator produces uniform distribution"
  (let ((samples (map (lambda (_) (gen-integer #:min 0 #:max 9))
                      (iota 1000)))
        (frequencies (make-vector 10 0)))
    (for-each (lambda (n)
                (vector-set! frequencies n 
                            (+ 1 (vector-ref frequencies n))))
              samples)
    ;; Each digit should appear ~100 times (±20%)
    (every (lambda (count) 
             (and (> count 80) (< count 120)))
           (vector->list frequencies))))
```

### 5. Combine Multiple Techniques
Use both property testing and TLA+ for critical components:
```scheme
;; Property test for quick feedback
(property-critical-invariant)

;; TLA+ for exhaustive verification
;; CriticalInvariant == [](...) 
```

---

## Troubleshooting

### Common Issues

#### 1. Random Seed Problems
```scheme
;; Fix: Ensure proper initialization
(random-source-randomize! *rng*)
;; Or use fixed seed for reproducibility
(random-source-pseudo-randomize! *rng* 42)
```

#### 2. Generator Infinite Loops
```scheme
;; Bad: Can loop forever
(define (gen-until-condition)
  (let loop ((x (gen-integer)))
    (if (condition? x) x (loop (gen-integer)))))

;; Good: Bounded attempts
(define (gen-until-condition #:max-attempts 1000)
  (let loop ((x (gen-integer)) (attempts 0))
    (cond
      ((condition? x) x)
      ((>= attempts 1000) (error "Generation failed"))
      (else (loop (gen-integer) (+ attempts 1))))))
```

#### 3. TLA+ State Explosion
```cfg
\* Limit state space in .cfg file
CONSTANTS
    MaxDepth = 2      \* Reduce from 5
    MaxListSize = 3   \* Reduce from 10
```

#### 4. Property Test Flakiness
```scheme
;; Use deterministic testing when needed
(parameterize ((current-random-source 
                (let ((src (make-random-source)))
                  (random-source-pseudo-randomize! src 12345)
                  src)))
  (run-property-tests))
```

### Debugging Tips

1. **Add Logging**: Track what's being tested
```scheme
(when *debug*
  (format #t "Testing: ~s => ~s~%" input output))
```

2. **Reduce Iteration Count**: Start with fewer tests
```scheme
(define *quick-test* #t)
(define iterations (if *quick-test* 10 1000))
```

3. **Isolate Properties**: Test one property at a time
```scheme
(define (test-single-property name)
  (case name
    ((arithmetic) (property-arithmetic-preservation))
    ((lists) (property-list-operations))
    (else (error "Unknown property"))))
```

4. **Visualize Distributions**: Check generator output
```scheme
(define (plot-distribution gen n)
  "Simple ASCII histogram"
  (let ((samples (map (lambda (_) (gen)) (iota n))))
    ;; ... generate histogram ...))
```

---

## Advanced Topics

### Custom Shrinking Strategies

Implement domain-specific shrinking:

```scheme
(define (shrink-elisp-expr expr)
  "Shrink Elisp expression toward simpler forms"
  (match expr
    ((op args ...) 
     (append (map (lambda (arg) (cons op (remove arg args)))
                  args)
             (map (lambda (arg) (list op arg))
                  args)))
    (n (when (number? n) (shrink-integer n)))
    (_ '())))
```

### Stateful Property Testing

Test sequences of operations:

```scheme
(define (property-stateful-compilation)
  "Property: Compilation state machine behaves correctly"
  (let ((state (make-compiler-state)))
    (for-each 
      (lambda (_)
        (let ((action (gen-compiler-action)))
          (apply-action! state action)
          (assert (valid-state? state))))
      (iota 20))
    #t))
```

### Performance Properties

Verify performance characteristics:

```scheme
(define (property-compilation-performance)
  "Property: Compilation time is O(n) in expression size"
  (let ((times '()))
    (for-each
      (lambda (size)
        (let* ((expr (gen-elisp-of-size size))
               (start (current-time))
               (_ (compile expr #:from 'elisp #:to 'bytecode))
               (elapsed (- (current-time) start)))
          (set! times (cons (cons size elapsed) times))))
      '(10 20 40 80 160))
    (is-linear? times)))
```

---

## Resources

### Documentation
- [SRFI-27](https://srfi.schemers.org/srfi-27/) - Random number generation
- [SRFI-64](https://srfi.schemers.org/srfi-64/) - Testing framework
- [TLA+ Language Manual](https://lamport.azurewebsites.net/tla/tla.html)
- [Learn TLA+](https://learntla.com/)

### Tools
- [TLC Model Checker](https://lamport.azurewebsites.net/tla/tools.html)
- [TLA+ Toolbox](https://lamport.azurewebsites.net/tla/toolbox.html)
- [QuickCheck](https://hackage.haskell.org/package/QuickCheck) - Inspiration

### Examples
- `examples/formal-verification/property-tests.scm` - Main test suite
- `specifications/*.tla` - TLA+ specifications
- `specifications/verify-specs.sh` - Verification script

---

## Conclusion

Formal verification through property-based testing and TLA+ specifications provides strong correctness guarantees for the Guile multilanguage system. These techniques catch edge cases that traditional testing might miss and prove properties that hold for all possible inputs.

Key takeaways:
1. Property tests explore the input space automatically
2. TLA+ proves properties mathematically
3. Combining both provides defense in depth
4. Start simple and gradually add complexity
5. Document assumptions and invariants clearly

For questions or contributions, see the main [README](../README.md) or open an issue on GitHub.

---

*Last updated: August 2025 | Based on Guile 3.0.10 on FreeBSD 14.3*