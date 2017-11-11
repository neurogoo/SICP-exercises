#lang sicp
;;Exercise 2.1.  Define a better version of make-rat that handles both positive and negative arguments. Make-rat should normalize the sign so that if the rational number is positive, both the numerator and denominator are positive, and if the rational number is negative, only the numerator is negative.

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((sign-n (if (< n 0) -1 1))
        (sign-d (if (< d 0) -1 1)))
    (let ((g (gcd (* sign-n n) (* sign-d d))))
      (cons (* (* sign-n sign-d) (/ (* sign-n n) g))
            (/ (* sign-d d) g)))))

;;Exercise 2.2.  Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points: a starting point and an ending point. Define a constructor make-segment and selectors start-segment and end-segment that define the representation of segments in terms of points. Furthermore, a point can be represented as a pair of numbers: the x coordinate and the y coordinate. Accordingly, specify a constructor make-point and selectors x-point and y-point that define this representation. Finally, using your selectors and constructors, define a procedure midpoint-segment that takes a line segment as argument and returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints). To try your procedures, you'll need a way to print points:

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (let ((p1 (start-segment segment))
        (p2 (end-segment segment)))
    (make-point (/ (+ (x-point p1) (x-point p2)) 2)
                (/ (+ (y-point p1) (y-point p2)) 2))))

;;Exercise 2.3.  Implement a representation for rectangles in a plane. (Hint: You may want to make use of exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the perimeter and the area of a given rectangle. Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers, so that the same perimeter and area procedures will work using either representation?

(define (rectangle upper-left lower-right)
  (cons upper-left lower-right))

(define (upper-left rectangle)
  (car rectangle))

(define (upper-right rectangle)
  (make-point (x-point (cdr rectangle)) (y-point (car rectangle))))

(define (lower-right rectangle)
  (cdr rectangle))

(define (lower-left rectangle)
  (make-point (x-point (car rectangle)) (y-point (cdr rectangle))))

(define (top-segment-rectangle rectangle)
  (make-segment (upper-left rectangle) (upper-right rectangle)))

(define (right-segment-rectangle rectangle)
  (make-segment (upper-right rectangle) (lower-right rectangle)))

(define (line-width segment)
  (let ((x1 (x-point (start-segment segment)))
        (x2 (x-point (end-segment segment))))
    (if (> x1 x2)
        (- x1 x2)
        (- x2 x1))))

(define (line-height segment)
  (let ((y1 (y-point (start-segment segment)))
        (y2 (y-point (end-segment segment))))
    (if (> y1 y2)
        (- y1 y2)
        (- y2 y1))))

(define (perimeter rectangle)
  (* 2 (+ (line-width (top-segment-rectangle rectangle)) (line-height (right-segment-rectangle rectangle)))))

(define (area rectangle)
  (* (line-width (top-segment-rectangle rectangle))
     (line-height (right-segment-rectangle rectangle))))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; Exercise 2.7.  Alyssa's program is incomplete because she has not specified the implementation of the interval abstraction. Here is a definition of the interval constructor:

(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;Exercise 2.8.  Using reasoning analogous to Alyssa's, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;Exercise 2.10.  Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa's code to check for this condition and to signal an error if it occurs.


(define (div-interval x y)
  (if (or (= 0 (lower-bound y)) (= 0 (upper-bound y)))
      'error
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;Exercise 2.12.  Define a constructor make-center-percent that takes a center and a percentage tolerance and produces the desired interval. You must also define a selector percent that produces the percentage tolerance for a given interval. The center selector is the same as the one shown above.

;(define (make-center-percent c p)
;  (let((midpoint ()))
;      (make-interval ())))

;(define (percent i))

;Exercise 2.17.  Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

;Exercise 2.18.  Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:

(define (reverse l)
  (define (reverse-list start end)
    (if (null? end)
        start
        (reverse-list (cons (car end) start) (cdr end))))
  (reverse-list '() l))

;Exercise 2.19.  Consider the change-counting program of section 1.2.2. It would be nice to be able to easily change the currency used by the program, so that we could compute the number of ways to change a British pound, for example. As the program is written, the knowledge of the currency is distributed partly into the procedure first-denomination and partly into the procedure count-change (which knows that there are five kinds of U.S. coins). It would be nicer to be able to supply a list of coins to be used for making change.

(define (first-denomination l)
  (car l))
(define (except-first-denomination l)
  (cdr l))
(define (no-more? l)
  (null? l))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
;Exercise 2.20.  The procedures +, *, and list take arbitrary numbers of arguments. One way to define such procedures is to use define with dotted-tail notation. In a procedure definition, a parameter list that has a dot before the last parameter name indicates that, when the procedure is called, the initial parameters (if any) will have as values the initial arguments, as usual, but the final parameter's value will be a list of any remaining arguments. For instance, given the definition

(define (same-parity x . l)
  (let ((parity (remainder x 2)))
    (define (filter-parity result l)
      (if (null? l)
          result
          (filter-parity (if (= parity (remainder (car l) 2))
                             (cons (car l) result)
                             result)
                         (cdr l))))
    (reverse (filter-parity (list x) l))))
;Exercise 2.21.  The procedure square-list takes a list of numbers as argument and returns a list of the squares of those numbers.

;(define (square-list l)
;  (map (lambda (x) (* x x)) l))
;(define (square-list items)
;  (if (null? items)
;      nil
;      (cons (* (car items) (car items)) (square-list (cdr items)))))

;Exercise 2.22.  Louis Reasoner tries to rewrite the first square-list procedure of exercise 2.21 so that it evolves an iterative process:

;Exercise 2.23.  The procedure for-each is similar to map. It takes as arguments a procedure and a list of elements. However, rather than forming a list of the results, for-each just applies the procedure to each of the elements in turn, from left to right. The values returned by applying the procedure to the elements are not used at all -- for-each is used with procedures that perform an action, such as printing. For example,

(define (my-for-each func l)
  (cond ((null? l) )
        (else
         (func (car l))
         (my-for-each func (cdr l)))))
;Exercise 2.25.  Give combinations of cars and cdrs that will pick 7 from each of the following lists:

;;(car (cdaddr '(1 3 (5 7) 9)))
;;(car (car '((7))))
;;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;;Exercise 2.27.  Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. 

(define (deep-reverse m)
  (define (iter start end)
    (cond ((null? start) end)
          ((not (pair? (car start))) (iter (cdr start) (cons (car start) end)))
          (else (iter (cdr start) (cons (iter (car start) '()) end)))))
  (iter m '()))

;; Exercise 2.28.  Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order.

(define (fringe tree)
  (define (iter node)
    (cond ((pair? node) (append (iter (car node)) (iter (cdr node))))
          ((null? node) nil)
          (else (list node))))
  (iter tree))

;; Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;a

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (define (weight branch)
    (let ((structure (branch-structure branch)))
      (if (pair? structure)
          (+ (weight (left-branch structure))
             (weight (right-branch structure)))
          structure)))
  (+ (weight (left-branch mobile))
     (weight (right-branch mobile))))
