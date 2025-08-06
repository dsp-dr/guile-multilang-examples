;;; factorial.el --- Factorial implementation in Elisp for Guile

;; This file demonstrates Elisp code that can be compiled and run on Guile 3

(defun factorial (n)
  "Calculate factorial of N recursively.
Throws error for negative inputs, returns 1 for 0 and 1."
  (cond
   ((not (integerp n)) (error "Input must be an integer"))
   ((< n 0) (error "Factorial not defined for negative numbers"))
   ((<= n 1) 1)
   (t (* n (factorial (- n 1))))))

(defun factorial-iter (n)
  "Calculate factorial of N iteratively."
  (cond
   ((not (integerp n)) (error "Input must be an integer"))
   ((< n 0) (error "Factorial not defined for negative numbers"))
   ((<= n 1) 1)
   (t (let ((result 1)
            (counter n))
        (while (> counter 1)
          (setq result (* result counter))
          (setq counter (- counter 1)))
        result))))

;; List operations
(defvar test-list '(1 2 3 4 5 6 7 8 9 10)
  "A test list for demonstrations.")

(defun sum-list (lst)
  "Sum all elements in LST."
  (apply '+ lst))

(defun map-square (lst)
  "Square all elements in LST."
  (mapcar (lambda (x) (* x x)) lst))

(defun filter-even (lst)
  "Filter even numbers from LST."
  ;; Use a more portable approach since seq-filter may not be available in Guile's elisp
  (let (result)
    (dolist (x lst (reverse result))
      (when (= (mod x 2) 0)
        (push x result)))))

;; String operations
(defun reverse-string (str)
  "Reverse a string STR."
  (apply 'string (reverse (string-to-list str))))

(defun palindrome-p (str)
  "Check if STR is a palindrome."
  (string= str (reverse-string str)))

;; Higher-order functions
(defun compose (f g)
  "Return composition of functions F and G."
  (lambda (x) (funcall f (funcall g x))))

(defun curry (f x)
  "Curry function F with argument X."
  (lambda (y) (funcall f x y)))

;; Test the functions
(defun run-tests ()
  "Run all test functions and display results."
  (message "Factorial of 5: %d" (factorial 5))
  (message "Factorial of 10 (iterative): %d" (factorial-iter 10))
  (message "Sum of list: %d" (sum-list test-list))
  (message "Squared list: %s" (map-square '(1 2 3 4 5)))
  (message "Even numbers: %s" (filter-even test-list))
  (message "Reverse 'hello': %s" (reverse-string "hello"))
  (message "Is 'racecar' a palindrome? %s" (palindrome-p "racecar"))
  (message "Is 'hello' a palindrome? %s" (palindrome-p "hello")))

;; Run tests when loaded
(run-tests)