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
