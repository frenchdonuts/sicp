; "The general technique of isolating the parts of a program that deal w/ how data objects
; are REPRESENTED from the parts of a program that deal w/ how data objects are USED is a
; powerful design methodology called DATA ABSTRACTION."
; Representation vs Manipulation of data objects
; 2.1
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (print-rat x)
        (newline)
        (display (numer x))
        (display "/")
        (display (denom x))
    )
    (define (make-rat n d)
        (if (> 0 d)
            (make-rat (* -1 n) (* -1 d))
            (let ((g (gcd n d)))
                (cons (/ n g) (/ d g))
            )
        )
    )
; 2.2
    (define (make-point x y)
        (cons x y)
    )
    (define (x-point p)
        (car p)
    )
    (define (y-point p)
        (cdr p)
    )
    (define (print-point p)
        (newline)
        (display "(")
        (display (x-point p))
        (display ",")
        (display (y-point p))
        (display ")")
    )

    (define (make-segment start end)
        (cons start end)
    )
    (define (start-segment segment)
        (car segment)
    )
    (define (end-segment segment)
        (cdr segment)
    )
    (define (midpoint-segment segment)
        ((lambda (start end)
                    (make-point 
                        (average (x-point start) (x-point end))
                        (average (y-point start) (y-point end))
                    )
        ) (start-segment segment)
          (end-segment segment)
        )
    )
; 2.3
    ; For one representation, we can use 2 points. Note however that these rectangles'
    ; sides will always be parallel to the x and y axis
    (define (make-rect p1 p2)
        (cons p1 p2)
    )
    (define (point1 rect) (car rect))
    (define (point2 rect) (cdr rect))
    (define (length1 rect)
        (abs (- (x-point (point1 rect)) (x-point (point2 rect))))
    )
    (define (length2 rect)
        (abs (- (y-point (point1 rect)) (y-point (point2 rect))))
    )
    (define (perimeter rect)
        (* 2 (+ (length1 rect) (length2 rect)))
    )
    (define (area rect)
        (* (length1 rect) (length2 rect))
    )

    ; To allow for "rotated" rectangles, we need 3 points. (4 would allow trapezoids)
    ; We also need to rewrite some functions. However, notice that the perimeter and
    ; area functions remain the same.
    (define (make-rect p1 p2 p3)
        (cons p1 p2 p3)
    )
    (define (seglength p1 p2)
        (+ (square (- (x-point p1) (x-point p2))) (square (- (y-point p1) (y-point p2))))
    )
    (define (length1 rect)
        (seglength p1 p2)
    )
    (define (length2 rect)
        (seglength p2 p3)
    )
; 2.4
    ; Notice that this is some crazy shit: a pair is being represented by a PROCEDURE;
    ; the pair IS the lambda expression.
    ; Time to whip out the substitution model
    (define (cons x y)
        (lambda (m) (m x y))
    )
    (define (car z)
        (z (lambda (p q) p))
    )

    (car (cons x y))
    -> (car (lambda (m) (m x y)))
    -> ((lambda (m) (m x y)) (lambda (p q) p))
    -> (((lambda (p q) p) x y))
    -> x
    ; Thus, the corresponding code for cdr would be:
    (define (cdr z)
        (z (lambda (p q) q))
    )
; 2.5
    ; Note that you can do this w/ any prime/coprime number
    (define (ex:cons a b)
        (* (expt 2 a) (expt 3 b))
    )
    (define (ex:car c)
        (define (divideBy2 count remaining)
            (if (< 0 (- remaining (floor remaining)))
                (- count 1)
                (divideBy2 (+ count 1) (/ remaining 2))
            )
        )
        (divideBy2 0 c)
    )
    (define (ex:cdr c)
        (define (divideBy3 count remaining)
            (if (< 0 (- remaining (floor remaining)))
                (- count 1)
                (divideBy3 (+ count 1) (/ remaining 3))
            )           
        )
        (divideBy3 0 c)
    )
    ; tests
    (ex:car (ex:cons 4 5))
    (ex:cdr (ex:cons 4 5))
    ; I'm sure all this is somehow connected in some deep way to representation theory
; 2.6
    ; Here, we are representing numbers as procedures
    (define zero 
        (lambda (f) (lambda (x) x))
    )
    (define (add-1 n)
        (lambda (f) (lambda (x) (f ((n f) x))))
    )
    ; So 1 would be the evaluation of (add-1 zero)
    (add-1 zero)
    -> (add-1 (lambda (f) (lambda (x) x)))
    -> (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
    ; ((lambda (f) (lambda (x) x)) f) -> (lambda (x) x)
    -> (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
    -> (lambda (f) (lambda (x) (f x)))
    ; 2 would be the evaluation of (add-1 1)
    (add-1 (lambda (f) (lambda (x) (f x))))
    -> (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
    -> (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
    -> (lambda (f) (lambda (x) (f (f x))))
    ; (+ 1 2) would evaluate to
    (lambda (f) (lambda (x) (f (f (f x)))))
    ; How do I take a function that composes f once and a function that composes f 
    ; twice, and produce a function that composes f three times?
    ; This is a modified version of add-1 - just replace f w/ (a f)
    (define (add a n)
        (lambda (f) (lambda (x) ((a f) ((n f) x))))
    )
    ; Check:
    (add 1 2)
    -> (add (lambda (f) (lambda (x) (f x))) (lambda (f) (lambda (x) (f (f x)))))
    -> (lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f x))) f) (((lambda (f) (lambda (x) (f (f x)))) f) x))))
    -> (lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f x))) f) ((lambda (x) (f (f x))) x))))
    -> (lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f x))) f) (f (f x)))))
    -> (lambda (f) (lambda (x) ((lambda (x) (f x)) (f (f x)))))
    -> (lambda (f) (lambda (x) ((f (f (f x)))))) ; Correct - yay
    ; Note that we can "unchurch" and convert the function back to an integer by using
    ; ((n inc) 0) where n is the church numeral
    (define (unchurch n)
        ; inc->f and 0->x
        ((n inc) 0)
    )
; 2.1.4
    (define (add-interval x y)
        (make-interval (+ (lower-bound x) (lower-bound y))
                       (+ (upper-bound x) (upper-bound y))
        )
    )
    ; Multiplication becomes more complicated because of negative numbers
    (define (mul-interval x y)
        (let ((p1 (* (lower-bound x) (lower-bound y)))
              (p2 (* (lower-bound x) (upper-bound y)))
              (p3 (* (upper-bound x) (lower-bound y)))
              (p4 (* (upper-bound x) (upper-bound y)))
             )
            (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
        )
    )
    (define (div-interval x y)
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))
                      )
        )
    )
; 2.7
    (define (make-interval a b) (cons a b))
    (define (lower-bound interval) (car interval))
    (define (upper-bound interval) (cdr interval))
; 2.8
    (define (sub-interval x y)
        (make-interval (- (lower-bound x) (upper-bound y))
                       (- (upper-bound x) (lower-bound y))
        )
    )
; 2.9
    ; Let width(x) = size(x)/2 where x is an interval and size = upper_bound(x)-lower_bound(x)
    ; From here, it is not hard to show that:
    ; width(x+y) = width(x) + width(y) AND width(x-y) = width(x) + width(y).
    ; For multiplication and division, consider [-3 8] and [2 10]
    (define (width x)
        (/ (- (upper-bound x) (lower-bound x)) 2)
    )
    (define some-interval (make-interval -3 8))
    (define some-interval2 (make-interval 2 10))
    (width (mul-interval some-interval some-interval2)) -> 55
    (* (width some-interval) (width some-interval2)) -> 22
    (width (div-interval some-interval some-interval2)) -> 2.75
    (/ (width some-interval) (width some-interval2)) -> 1.375
; 2.10
    (define (div-interval x y)
        (let ((spans-zero? (or (= (upper-bound y) 0) (= (lower-bound y) 0))))
            (if spans-zero?
                (display "ERROR: Interval spans zero")
                (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))
                  )
                )
            )
        )
    )
    ; test
    (define some-interval (make-interval -3 8))
    (define some-interval2 (make-interval 0 10))
    (div-interval some-interval some-interval2)
; 2.11

; 2.12
    (define (make-center-width c w)
        (make-interval (- c w) (+ c w))
    )
    (define (center i)
        (/ (+ (lower-bound i) (upper-bound i)) 2.)
    )
    (define (width i)
        (/ (- (upper-bound i) (lower-bound i)) 2.)
    )
    (define (make-center-percent c p)
        (make-center-width c (* c (/ p 100.)))
    )
    (define (percent i)
        (* (/ (width i) (center i)) 100.)
    )
; 2.13
    (define some-interval (make-center-percent 3 3))
    (define some-interval2 (make-center-percent 7 5))
    (percent (mul-interval some-interval some-interval2))
    ; It looks like you just add the tolerances of the factors
; 2.14
    (define A (make-center-percent 75 1))
    (define B (make-center-percent 32 1))
    (define (format-interval-percent i)
        (newline)
        (display (center i))
        (display " +/- ")
        (display (percent i))
    )
    (format-interval-percent (div-interval A A))
    (format-interval-percent (div-interval A B))
    ; My answer is 
; 2.15
    ; This is a hand-wavy expalnation, but here goes:
    ; Because the variable carries with it the noise AND the signal,
    ; a formulation in which it is used redundantly would contribute more
    ; noise, but no more signal.
; 2.16
    ; To put it succinctly, "the more interval arithmetic we do during the calculation,
    ; the wider the resulting interval will be." - JoT's Jottings
    ; We also have to remember that the algebra of intervals is not the same as the 
    ; algebra of real numbers
; 2.17
    (define (last-pair lst)
        (if (null? (cdr lst))
            (list (car lst))
            (last-pair (cdr lst))
        )   
    )
; 2.18
    (define (reverse lst)
        (if (null? (cdr lst))
            (list (car lst))
            (append (reverse (cdr lst)) (list (car lst)))
        )
    )
; 2.19
    (define (cc amount coin-values)
        (cond ((= amount 0) 1)
              ((or (< amount 0) (no-more? coin-values)) 0)
              (else
                (+ (cc amount
                    (except-first-denomination coin-values))
                   (cc (- amount (first-denomination coin-values))
                     coin-values)
                )
              )
        )
    )
    (define (first-denomination coin-values)
        (car coin-values)
    )
    (define (except-first-denomination coin-values)
        (cdr coin-values)
    )
    (define (no-more? coin-values)
        (null? coin-values)
    )
    ; The order of the list of coin-values do not affect the result
; 2.20
    (define (same-parity x . lst)
        (display lst)(newline)
        (display (cdr lst))(newline)(newline)
        (let ((xparity (remainder x 2))
              (parity (remainder (car lst) 2)))
             (if (null? (cdr lst))
                 (if (= xparity parity) (car lst))
                 (if (= xparity parity)
                     (cons (car lst) (same-parity x (cdr lst)))
                     (same-parity x (cdr lst))
                 )
             )
        )
    )
    ; So (cdr lst) is being passed in as ONE parameter, not as a list of parameters.
    ; This is why it is wrapped by another list
    ; How to get around this?
    ; In general, how can we pass a list of parameters on to the next recursive call?
; 2.21
    (define (square-list items)
        (map (lambda (x) (* x x)) items)
    )
; 2.22
    ; Louis Reasoner's 1st try
    (define (square-list items)
        (define (iter things answer)
            (if (null? things)
                answer
                (iter (cdr things) (cons (square (car things)) answer))
            )
        )
        (iter items nil)
    )
    (square-list (list 1 2 3))
    -> (iter (list 1 2 3) ())
    -> (if (null? (1 2 3))
            ()
            (iter (cdr (1 2 3)) (cons (square (car (1 2 3))) ()))
       )
    -> (iter (2 3) (cons (square 1) ()))
    -> (iter (2 3) (cons 1 ()))
    -> (iter (2 3) (1))
    -> (if (null? (2 3))
            (1)
            (iter (cdr (2 3)) (cons (square (car (2 3))) (1)))
       )
    -> (iter (cdr (2 3)) (cons (square 2) (1)))
    -> (iter (3) (cons 4 (1)))
    -> (iter (3) (4 1))
    -> (if (null? (3))
            (4 1)
            (iter (cdr (3)) (cons (square (car (3))) (4 1)))
       )
    -> (iter (cdr (3)) (cons (square (car (3))) (4 1)))
    -> (iter () (cons 9 (4 1)))
    -> (iter () (9 4 1))
    -> (if (null? ())
            (9 4 1)
            (iter (cdr ()) (cons (square (car ())) (9 4 1)))
       )
    -> (9 4 1)
    ; It looks like the reason the answer is reversed is because
    ; (cons current_number_squared accumulated_list)
    ; Louis Reasoner's 2nd try - he switches the arguments to cons ("fixing" it according
    ; to the explanation we gave above)
    (define (square-list-2 items)
        (define (iter things answer)
            (if (null? things)
                answer
                (iter (cdr things) (cons answer (square (car things))))
            )
        )
        (iter items ())
    )
    (square-list (list 1 2 3))
    -> (iter (list 1 2 3) ())
    -> (if (null? (1 2 3))
            ()
            (iter (cdr (1 2 3)) (cons () (square (car (1 2 3)))))
       )    
    -> (iter (cdr (1 2 3)) (cons () (square (car (1 2 3)))))
    -> (iter (2 3) (cons () 1))
    -> (iter (2 3) (() 1))
    -> (if (null? (2 3))
            (() 1)
            (iter (cdr (2 3)) (cons (() 1) (square (car (2 3)))))
       )
    -> (iter (cdr (2 3)) (cons (() 1) (square (car (2 3)))))
    -> (iter (3) (cons (() 1) 4))
    -> (iter (3) (() 1 4))

    -> (if (null? (things))
            answer
            (iter (cdr things) (cons answer (square (car things))))
       )
    ; Short answer as to why the 2nd is not correct is...that while the ordering
    ; is correct, the result is not a list. Remember that a list is something like
    ; this: (1. (4 . (9 . (16 . (25 . ()))))). The 2nd procedure produces something
    ; like this: (((((() . 1) . 4) . 9) . 16) . 25). It's sort of like a "reverse" list.
; 2.23
    ; Use the fact that the parameters of a function have to be evaluated 
    ; before the function itself can be evaluated.
    (define (for-each f items)
        (define (iter items dummy)
            (if (null? items)
                #t
                (iter (cdr items) (f (car items)))
            )
        )
        (iter (cdr items) (f (car items)))
    )
;
    (list 1 2) -> (cons 1 (cons 2 nil))
    (list 3 4) -> (cons 3 (cons 4 nil))
    (cons (list 1 2) (list 3 4)) 
    -> (cons (cons 1 (cons 2 nil)) (cons 3 (cons 4 nil)))
    -> ((1 2) 3 4)
    ; Remember, the two parameters to cons:
    ; 1. The first elementm (car)
    ; 2. A list containing the rest of the elements (cdr)
;
    ; Another way to think of sequences whose elements are sequences is as TREES
;
    ; Compare length to count-leaves
    (define (length items)
        (if (null? items)
            0
            (+ 1 (length (cdr items)))
        )
    )
    (define (count-leaves tree)
        (cond ((null? tree) 0)
              ((not (pair? tree)) 1)
              (else
                (+ (count-leaves (car tree)) (count-leaves (cdr tree)))
              )
        )
    )
; 2.24
    ; Have to do this one on paper
; 2.25
    ; Let x be:
    ; (1 3 (5 7) 9)
    ;   (car (cdr (car (cdr (cdr x))))) -> 7
    ; ((7))
    ;   (car (car x)) -> 7
    ; (1 (2 (3 (4 (5 (6 7)))))) (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))
    ;   (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))
; 2.26
    (define x (list 1 2 3))
    (define y (list 4 5 6))
    (append x y)
    -> (1 2 3 4 5 6)
    (cons x y)
    -> ((1 2 3) 4 5 6)
    (list x y)
    -> ((1 2 3) (4 5 6))
; 2.27
    ; (list reverse_list_w/o_first_element first_element)
    ; But now, the first element can also be a list, so we have to reverse that too
    ; It can also not be a list, in which case we return itself - BASE CASE
    ; Version 1 - wrong; but on the right track.
    (define (deep-reverse items)
        (cond ((not (pair? items)) items)
              (else (list (deep-reverse (cdr items)) (deep-reverse (car items))))
        )
    )
    ; Version 2
    ; if cdr is a (), don't run deep-reverse on it
    (define (deep-reverse items)
        (cond ((not (pair? items)) items)
              ((null? (cdr items)) (list (deep-reverse (car items))))
              (else (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))
        )
    )
    ; Notice that append is used instead of (list reversed_cdr reversed_car). This is
    ; because cdr will wrap things in a list, but car won't. Since append takes 2 lists
    ; we have to wrap our reversed_car in a list AND we also have to wrap the base case
    ; in which we are the last element (before nil) in a list. 
    ; The rough outline was pretty clear (it's just reverse all the way down), but
    ; using append and wrapping the appropriate cars in a list felt pretty hand-wavy
    ; to me. I didn't really think it through, I just sort of "felt" it out.
    ; Let's use the substitution model to run through my algorithm.
    (deep-reverse (list 1 2 (list 3 4)))
    -> (cond ((not (pair? (list 1 2 (list 3 4)))) (list 1 2 (list 3 4)))
              ((null? (cdr (list 1 2 (list 3 4)))) (list (deep-reverse (car (list 1 2 (list 3 4))))))
              (else (append (deep-reverse (cdr (list 1 2 (list 3 4)))) (list (deep-reverse (car (list 1 2 (list 3 4)))))))
       )
    -> (append (deep-reverse (cdr (list 1 2 (list 3 4)))) (list (deep-reverse (car (list 1 2 (list 3 4))))))
    -> (append (deep-reverse (list 2 (list 3 4))) (list (deep-reverse 1)))
        (deep-reverse 1)
        -> 1
        (deep-reverse (list 2 (list 3 4)))
        -> (append (deep-reverse (cdr (list 2 (list 3 4)))) (list (deep-reverse (car (list 2 (list 3 4))))))
        -> (append (deep-reverse (list (list 3 4))) (list (deep-reverse 2)))
            (deep-reverse 2)
            -> 2
            (deep-reverse (list (list 3 4))) ;(null? (cdr (list (list 3 4)))) -> true
            -> (list (deep-reverse (car (list (list 3 4)))))
            -> (list (deep-reverse (list 3 4)))
                (deep-reverse (list 3 4))
                -> (append (deep-reverse (cdr (list 3 4))) (list (deep-reverse (car (list 3 4)))))
                -> (append (deep-reverse (list 4)) (list (deep-reverse 3)))
                    (deep-reverse 3)
                    -> 3
                    (deep-reverse (list 4)) ;(null? (cdr (list 4))) -> true
                    -> (list (deep-reverse (car (list 4))))
                    -> (list (deep-reverse 4))
                        (deep-reverse 4)
                        -> 4
                    -> (list 4)
                -> (append (list 4) (list 3))
                -> (list 4 3)
            -> (list (list 4 3))
        -> (append (list (list 4 3)) (list 2))
        -> (list (list 4 3) 2)
    -> (append (list (list 4 3) 2) (list 1))
    -> (list (list 4 3) 2 1)
    -> ((4 3) 2 1)
    ; Summary
    (deep-reverse (list 1 2 (list 3 4)))
    -> (append (deep-reverse (list 2 (list 3 4))) (list (deep-reverse 1)))
        (deep-reverse 1)
        -> 1        
        (deep-reverse (list 2 (list 3 4)))
        -> (append (deep-reverse (list (list 3 4))) (list (deep-reverse 2)))
            (deep-reverse 2)
            -> 2        
            (deep-reverse (list (list 3 4)))
                (deep-reverse (list 3 4))
                    (deep-reverse 3)
                    -> 3
                    (deep-reverse (list 4))
                        (deep-reverse 4)
                        -> 4
                    -> (list 4) ; (list (deep-reverse 4))
                -> (list 4 3)   ; (append (deep-reverse (list 4)) (list (deep-reverse 3)))
            -> (list (list 4 3))
        -> (list (list 4 3) 2)
    -> (append (list (list 4 3) 2) (list 1))
    ; Scratch template
    -> (cond ((not (pair? items)) items)
              ((null? (cdr items)) (list (deep-reverse (car items))))
              (else (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))
       )
; 2.28
    (define (fringe tree)
        (cond ((not (pair? tree)) (list tree))
              ((null? (cdr tree)) (fringe (car tree)))
              (else (append (fringe (car tree)) (fringe (cdr tree))))
        )
    )
    ; Hmm...this is a little cleaner
  (define (fringe tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (list tree))
          (else (append (fringe (car tree)) (fringe (cdr tree))))))
; 2.29
    (define (make-mobile left right) (list left right))
    (define (make-branch length structure) (list length structure))
    ; test
    (define mobile1
      (make-mobile (make-branch 10 1)
                   (make-branch 2 5)))

    (define mobile2
      (make-mobile (make-branch 10 1)
                   (make-branch 2 (make-mobile (make-branch 4 1)
                                               (make-branch 1 4)))))

    (define mobile3
      (make-mobile (make-branch 9 1)
                   (make-branch 2 4)))
    ; a)
    (define (left-branch m) (car m))
    (define (right-branch m) (car (cdr m)))
    (define (branch-length b) (car b))
    (define (branch-structure b) (car (cdr b)))
    ; b)
    (define (total-weight m)
        (cond ((null? m) 0)
              ((not (pair? m)) m)
              (else (+ (total-weight (branch-structure (left-branch m))) (total-weight (branch-structure (right-branch m)))))
        )
    )
    ; c)
    (define (torque b)
        (* (branch-length b) (total-weight (branch-structure b)))
    )
    (define (balanced? m)
        (if (pair? m)
            (and (= (torque (left-branch m)) (torque (right-branch m)))
                 (balanced? (branch-structure (left-branch m)))
                 (balanced? (branch-structure (right-branch m)))
            )
            #t
        )
    )
    ; d)
    ; We would simply need to change the selectors (aka (car (cdr)) would become cdr)
;
    ; The copy of a tree is the copy of its left branch(car) +(cons) the copy of its right branch(cdr)
    (define (copy tree)
        (cond ((null? tree) ())
              ((not (pair? tree)) tree)
              (else (cons (copy (car tree)) (copy (cdr tree))))
        )
    )
    ; Really the conditions are: If its the empty list(primitive), if it is not a list(primitive), if it is a tree/list(keep going)
    ;-> (cond ((null? tree) ())
    ;         ((not (pair? tree)) tree)
    ;         (else (cons (copy (car tree)) (copy (cdr tree))))
    ;   )
    (copy (list (list 1 2) (list 3 4)))
    -> (cons (copy (car (list (list 1 2) (list 3 4)))) (copy (cdr (list (list 1 2) (list 3 4)))))
    -> (cons (copy (list 1 2)) (copy (list (list 3 4))))
        (copy (list 1 2))
        -> (cons (copy (car (list 1 2))) (copy (cdr (list 1 2))))
        -> (cons 1 (copy (list 2)))
            (copy (list 2))
            -> (cons (copy (car (list 2))) (copy (cdr (list 2))))
            -> (cons (copy 2) (copy ()))
            -> (cons 2 ())
        -> (cons 1 (cons 2 ())) ; (cons (cons 1 (cons 2 ())) (copy (list (list 3 4))))
        (copy (list (list 3 4)))
        -> (cons (copy (car (list (list 3 4)))) (copy (cdr (list (list 3 4)))))
        -> (cons (copy (list 3 4)) (copy ()))
        -> (cons (cons 3 (cons 4 ())) ())
    -> (cons (cons 1 (cons 2 ())) (cons (cons 3 (cons 4 ())) ()))
    -> (           (1 2)                      (3 4)             )
    ; The main way to "use" a tree is (combination_operator (fn (car tree)) (fn (cdr tree)))
    ; You can either car or cdr a tree (cuz its a list) - pretty simple 
;
    (define (scale-tree tree factor)
        (cond ((null? tree) ())
              ((not (pair? tree)) (* tree factor))
              (else (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))
        )
    )
    ; It's like copy tree, but it requires 2 parameters, and in the base case: (not (pair? tree)), we multiply the
    ; leaf as opposed to just return it.
    ; Another way to implement scale-tree however, is to use the map function
    (define (scale-tree tree factor)
        (map (lambda (sub-tree)
                (if ((not (pair? sub-tree)) 
                    (* sub-tree factor))
                    (scale-tree sub-tree factor)
                )
             )
            tree
        )
    )
    ; It's like the map function already handles a couple things:
    ;  A base case: (null? tree)
    ;  Tree copying (since map outputs the same list structure that you feed it)
    ;       But because map is designed to only map over a list up to first depth (shallow vs deep),
    ;       you must use recursion to make sure you map over all the sub-lists as well.
    ; If we implement deep-map, we can further simply the scale-tree; I believe this is excercise 2.31
; 2.30
    (define (square-tree tree)
        (cond ((null? tree) ())
              ((not (pair? tree)) (* tree tree))
              (else (cons (square-tree (car tree)) (square-tree (cdr tree))))
        )
    )
    (define (square-tree tree)
        (map (lambda (sub-tree)
                (if (pair? sub-tree)
                    (square-tree sub-tree)
                    (* sub-tree sub-tree)
                )
             )
             tree
        )
    )
; 2.31
    ; A reminder of map
    (define (map operator list)
        (cond ((null? list) ())
              (else (cons (operator (car list)) (map operator (cdr list))))
        )
    )
    ; Instead of deep-map, let's call it tree-map
    (define (tree-map operator tree)
        (cond ((null? tree) '())
              ; Calculation
              ((not (pair? tree)) (operator tree))
              ; Traversal
              (else (cons 
                            (tree-map operator (car tree)) 
                            (tree-map operator (cdr tree))
                    )
              )
        )
    )
    ; Redefine square-tree
    (define (square-tree tree) (tree-map square tree))
 
; 2.32
    ; The set of all subsets is 
    ;   the set of all subsets without the first element
    ;   joined with (append)
    ;   the set of all subsets w/ the first element
    (define (subsets s)
        (if (null? s)
            (list nil)
            (let ((rest (subsets (cdr s))))
                 (append rest (map
                               (lambda (subset) (append (list (car s)) subset))
                               rest)
                 )
            )
        )
    )
; 2.2.3 - Sequences as Conventional Interfaces
    ; "Abstraction preserves for us the flexibility to experiment with alternate representations."
    ; In this section, we consider the possibility of viewing all(most) computation as a sequence
    ; of operations on sequences.
    ; For example, the procedure to square all odd leaves on a tree, then sum them can be viewed as:
    ; enumerate: tree leaves ---> filter: odd? ---> map: square ---> accumulate: +, 0
    (define (sum-odd-squares tree)
      (accumulate + 0
                  (map square
                       (filter odd?
                               (enumerate-tree tree)))))
    ; Where
    (define (accumulate op initial seq)
      (if (null? seq)
          initial    ; Notice that the initial value is "added"(op) last
          (op (car seq) (accumulate op initial (cdr seq)))))
    (define (filter predicate? seq)
      (cond ((null? seq) nil)
            ((predicate? (car seq)) 
             (cons (car seq) (filter predicate? (cdr seq))))
            (else (filter predicate? (cdr seq)))))
    ; enumerate-tree is just like the fringe procedure we wrote for excercise 2.28
    (define (enumerate-tree tree)
      (cond ((null? tree) nil)
            ((not (pair? tree)) (list tree))
            (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))
    ; Another example is getting the list of even Fibonacci numbers less than n:
    ; enumerate: integers ---> map: Fib ---> filter: even? ---> accumulate: cons, ()
    (define (even-fibs n)
      (accumulate cons nil
                  (fiter even?
                         (map Fib
                              (enumerate-interval 0 n)))))
    ; Where
    (define (enumerate-interval a b)
      (if (> a b)
          nil
          (cons a (enumerate-interval (+ a 1) b))))
    (define (Fib n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (Fib (- n 1)) (Fib (- n 2))))))
    ; Notice how similar sum-odd-squares and even-fibs is when viewed this way.
    ; We can even start mixing and matching their code...
    (define (list-of-fib-squares n)
      (accumulate cons nil
                  (map square
                       (map Fib
                            (enumerate-interval 0 n)))))
    ; BECAUSE ITS ALL SEQUENCES!!!
    ; This way of programming lends itself naturally to modular design
    ; "Sequences, implemented here as lists, serve as a conventional interface that permits us to combine
    ; processing modules."
; 2.33
    ; "The procedure accumulate produces a single value by iterating over the list and, for each element
    ; in the list, applying a supplied two-parameter procedure to the element and the results of applying 
    ; accumulate to the rest of the list." -JoT's Jottings
    ; So y is a list that's already been mapped
    (define (map op seq)
        (accumulate (lambda (x y) (cons (op x) y)) nil seq))
    ; "accumulate works in a manner equivalent to starting with the last item of the list it is accumulating 
    ; over and applying op to it and the accumulated result so far, starting with some initial value and repeating 
    ; that backwards throughout the list. So if we start with an initial value of seq2 and cons each item from seq1
    ; to it, starting at the last and working through to the first then we will produce an equivalent implementation 
    ; to append." -JoT's Jottings
    (define (append seq1 seq2)
        (accumulate cons seq2 seq1))
    ; Increment the accumulated value by 1
    (define (length seq)
     (accumulate (lambda (x y) (+ 1 y)) 0 seq))
; 2.34
    ; Look up Horner's rule - pretty cool. The key is the distributive law.
    ; For example, (a + b)x is 2 operations, while ax + bx is 3 operations.
    (define (horner-eval x coefficient-sequence)
        (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
                    0
                    coefficient-sequence))
; 2.35
    (define (count-leaves t)
        (accumulate +
                    0
                    (map (lambda (x) (if (not (pair? x)) 1 (count-leaves x)))
                         t)))   
    ; ((2 1) (3 4) 5) ---> (2 2 1)
   ; Just for fun, flatten aka fringe aka enumerate-tree
   (define (flatten tree)
     (accumulate append
                 nil
                 (map (lambda (x) (if (not (pair? x)) (list x) (flatten x))) 
                      tree)))
     
; 2.36
    (define (accumulate-n op init seqs)
        (if (null? (car seqs))
            nil
            (cons (accumulate op init (map car seqs))
                  (accumulate-n op init (map cdr seqs)))))
; 2.37
   (define (dot-product v w)
     (accumulate + 0 (map * v w)))
  
   (define (matrix-*-vector m v)
     (map (lambda (i) (dot-product i v)) m))
   (define (transpose mat)
     (accumulate-n cons nil mat))
   (define (matrix-*-matrix m n)
     (let ((cols (transpose n)))
       (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))
   (define matrix2 (list (list 0 0 1) (list 0 0 1) (list 0 0 1)))
; Rich Hickey's Talk on Transducers
    (define process-bags
        (comp
            (mapcat unbundle-pallet)
            (filter non-food?)
            (map label-heavy)))
    ; mapcat, filter, and map all return transducers
    ; composing them to make process-bags is also a transducer
    ; transducers modify a process by transforming its reducing funtion
    ; the same transducer can now be used for different sequence-like
    ; interfaces!
    ; build a concrete collection (a collection)
    (into airplane process-bags pallets)
    ; build a lazy sequence (a sequence)
    (sequence process-bags pallets)
    ; like reduce, but takes a transducer
    (transduce 
      (comp process-bags (mapping weight-bag))
      + 0 pallets)
    ; a CSP channel that processes bags (a channel)
    (chan 1 process-bags)
    ; it's an open system (an observable)
    (observable process-bags pallet-source)
    ; "A concrete list and the operations that constructed are isomorphic
    ; to each other."
    ; I never knew what parameterization was until this talk. For example,
    ; this is how you parameterize the cons operation in the map function we 
    ; wrote in Excercise 2.33
    (define (map op seq)
        (accumulate (lambda (x y) (cons (op x) y)) nil seq))
    ; Becomes
    (define (map op seq)
        (lambda (stepfn)
            (accumulate (lambda (x y) (stepfn (op x) y)) nil seq)))
    ; You simply make it return a function that takes the thing you want
    ; to parameterize!! duh. Btw, this version of map is now a transducer -
    ; it makes no assumption about what you are mapping over (cons is specific
    ; to a data structure)
;
    ; The key to making programming easy (or one of them) is
    ; to make sure your functions are semantically complete and consistent
    ; aka they are mathematical functions that are well-described
    ; by their name
; 2.38
    ; Accumulate is also known as fold-right; this is fold left
   (define (fold-left op initial sequence)
     (define (iter result seq)
       (if (null? seq)
           result
           (iter (op result (car seq))
                 (cdr seq))))
     (iter initial sequence)) ; Notice that initial is combined FIRST 
    (fold-right / 1 (list 1 2 3))
    ; -> ((3/2)/1)/1 = 3/2
    (fold-left / 1 (list 1 2 3))
    ; -> ((1/1)/2)/3 = 1/6
    (fold-right list nil (list 1 2 3))
    ; -> (list (list (list 3 2) 1) 1) -> (((3 2) 1) 1)
    (fold-left list nil (list 1 2 3))
    ; -> (list (list (list 1 1) 2) 3) -> (((1 1) 2) 3)
    ; Interestingly, the names of these procedures are descriptive of the underlying 
    ; processes they describe.
    ; For example, fold-right is a recursive process: the process starts at the
    ; highest-level description, then works left, expanding these higher descriptions
    ; into primitive operations. When all the expansion is finished, the process
    ; "collapses" right, combining each layer from right-to-left.
    ; Fold-left on the other hand, builds "up"(left-to-right). It starts from nothing, 
    ; and layers the iterative steps on top of each other until we come up with the final
    ; answer. 
    ; To make sure that fold-right and fold-left produce the same result,
    ; make sure your op is associative.
; 2.39
   ; -> (op (op (op 3 2) 1) ())
   ; -> (op (op reversed_list 1) ())
   ; -> (op (op y x) ())
   (define (reverse sequence)
     (fold-right (lambda (x y) (append y (list x))) nil sequence))
   ; -> (op (op (op () 1) 2) 3)
   ; (op () 1) should return (list 1 ())
   ; -> (op (op (op x y) 2) 3)
   ; (op x y) should return (list y x)
   (define (reverse sequence)
     (fold-left (lambda (x y) (cons y x)) nil sequence))
; 2.40
   (define (flatmap proc seq)
     (accumulate append nil (map proc seq)))
   (define (unique-pairs n)
     (flatmap (lambda (i)
                (map (lambda (j)
                       (list i j)) 
                     (enumerate-interval 1 (- i 1))))
              (enumerate-interval 1 n)))
   (define (prime-sum? pair)
     (prime? (+ (car pair) (cadr pair))))
   (define (make-pair-sum pair)
     (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
   (define (prime-sum-pairs n)
     (map make-pair-sum 
          (filter prime-sum? 
                  (unique-pairs n))))
; 2.41
   (define (unique-triplets n)
     (flatmap (lambda (i)
                (flatmap (lambda (j)
                       (map (lambda (k)
                              (list i j k))
                            (enumerate-interval 1 i)))
                     (enumerate-interval 1 i)))
              (enumerate-interval 1 n)))
   (define (sum-to s? triplet)
     (= s? (+ (car triplet) (cadr triplet) (caddr triplet))))
   (define (sum-to-s-pairs s n)
     (filter (lambda (triplet) (sum-to s triplet))
             (unique-triplets n)))
; 2.42
   (define (queens board-size)
     (define (queen-cols k)
       (if (= k 0)
           (list empty-board)
           (filter
             (lambda (positions) (safe? k positions)) ; filter out all the invalid ones
             (flatmap ; ( ((1 2) (2 4)) ((3 5) (2 1)) ((7 8) (3 7)) ) - we have now transformed a set of solutions to the k-1 case to a set of positions, each of which may or may not be valid
               (lambda (rest-of-queens) ;rest-of-queens is ONE valid solution to (k-1) case
                 (map (lambda (new-row) ; For a valid solution to (k-1) case, map it to a set of potential solutions by adding the kth queen in every possible row;
                        (adjoin-position new-row k rest-of-queens))
                      (enumerate-interval 1 board-size)))
               (queen-cols (- k 1))))))
     (queen-cols board-size))
   ; Sample board:
   ;((r1 . c1) (r2 . c2) (r3 . c3)...(rk . ck))
   (define (adjoin-position new-row k rest-of-queens)
     ;(display rest-of-queens) (newline)
     (append (list (cons new-row k)) rest-of-queens))
   ; Too lazy to build abstractions
   ; No queens:
   ; ()
   (define empty-board ())
   ; positions is a list of queens' positions - a list of lists(pairs):
   ; Given a set of queens: ((r1, c1) (r2, c2) (r3, c3)...(rk, ck))
   ; Filter out the kth queen:
   (define (filter-kth-queen set-of-queens k)
     (filter (lambda (pair)
               (not (= (cdr pair) k)))
             set-of-queens))
   ; ((r1, c1) (r2, c2) (r3, c3)...(rk-1, ck-1))
   ; Now, let's test each of them to see if they are safe w/ (rk, ck)
   ; if rk = rx
   ; OR rk - (k - cx) = rx
   ; OR rk + (k - cx) = rx
   ; -> not safe
   (define (safe-with-k? queen k rk)
     (cond ((= rk (car queen)) #f)
           ((= (- rk (- k (cdr queen))) (car queen)) #f)
           ((= (+ rk (- k (cdr queen))) (car queen)) #f)
           (else #t)))
   ; (#t #t #f...#t)
   ; Finally, we accumulate them using "and" wrapped in a lambda
   (define (safe? k positions)
     (let ((rk (car (list-ref positions 0))))
       (accumulate (lambda (x y) (and x y))
                   #t
                   (map (lambda (pair) (safe-with-k? pair k rk))
                        (filter-kth-queen positions k)))));#t)
   ; I just spent HOURS debugging because I didn't pay attention to how
   ; append (used in adjoin-position) works...(and used the wrong index for list-ref in safe? WOW)
   ; REVIEW:
   ; (append list1 list2) returns list1list2; it pushes list1 IN FRONT OF list2
   ; Basic idea of the queens algorithm:
   ; To place kth queen in kth column, 
   ;    Assume we have already placed k-1 queens in a valid manner
   ;    Create a set of positions by placing the kth queen in each row
   ;        For each position, check if it is a valid solution
; 2.43
    ; If the above queens runs in time T,
    ; how would interchanging the two lambdas inside flatmap affect the program?:
   (flatmap
     (lambda (new-row)
       (map (lambda (rest-of-queens)
              (adjoin-position new-row k rest-of-queens))
            (queen-cols (- k 1))))
     (enumerate-interval 1 board-size))
    ; queen-cols is now called board-size times whereas before it was only called once
    ; But since queen-cols calls itself, 
    ; (queen-cols n) calls (queen-cols n-1) n times (let n=board-size)
    ; (queen-cols n-1) calls (queen-cols n-2) n times...
    ; (queen cols 1) calls (queen-cols 0) n times
    ; So queen-cols gets called n^n times.
    ; Hence, Louis' algorithm would run O(n^n). Not sure how to relate it to T.
    ; By the way, since you can look at flatmap, map, and other sequence operations as loops,
    ; you can instantly tell Louis' version will be slower since he places queen-cols inside
    ; of a loop.
; 2.44
   (define (up-split painter n)
     (if (= n 0)
         painter
         (let ((smaller (up-split painter (- n 1))))
           (below painter (beside smaller smaller)))))
; 2.45
   (define (split half quarter)
     (define (recursive painter n)
       (if (= n 0)
           painter
           (let ((smaller (recursive painter (- n 1))))
             (half painter (quarter smaller smaller)))))
     (lambda (painter n) (recursive painter n))
     
     (define right-split (split beside below))
     (define up-split (split below beside))
; 2.46
   (define (make-vect x y) (cons x y))
   (define (xcor-vect v) (car v))
   (define (ycor-vect v) (cdr v))
   
   (define (add-vect v1 v2)
     (make-vect (+ (xcor-vect v1) (xcor-vect v2))
                (+ (ycor-vect v1) (ycor-vect v2))))
   (define (sub-vect v1 v2)
     (make-vect (- (xcor-vect v1) (xcor-vect v2))
                (- (ycor-vect v1) (ycor-vect v2))))
   (define (scale-vect s v)
     (make-vect (* s (xcor-vect v))
                (* s (ycor-vect v))))
; 2.47
   (define (make-frame origin edge1 edge2)
     (list origin edge1 edge2))
   (define (origin-frame frame) (car frame))
   (define (edge1-frame frame) (cadr frame))
   (define (edge2-frame frame) (caddr frame))
   
   (define (make-frame origin edge1 edge2)
     (cons origin (cons edge1 edge2)))
   (define (origin-frame frame) (car frame))
   (define (edge1-frame frame) (cadr frame))
   (define (edge2-frame frame) (cddr frame))
; 2.48
   (define (make-segment v1 v2) (cons v1 v2))
   (define (start-segment segment) (car segment))
   (define (end-segment segment) (cdr segment))
; 2.49
   ; a)
   (define (outline frame)
     (define btl (make-vect 0 0))
     (define btr (make-vect 1 0))
     (define tpr (make-vect 1 1))
     (define tpl (make-vect 0 1))
     (segments->painter (list
                          (make-segment btl btr)
                          (make-segment btr tpr)
                          (make-segment tpr tpl)
                          (make-segment tpl btl))))
   ;SKIP
; 2.50
   (define (flip-horiz painter)
     (transform-painter painter
                        (make-vect 1.0 0.0)
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 1.0)))
   (define (rotate180 painter)
     (transform-painter painter
                        (make-vect 1.0 1.0)
                        (make-vect 0.0 1.0)
                        (make-vect 1.0 0.0)))
   (define (rotate270 painter)
     (transform-painter painter
                        (make-vect 0.0 1.0)
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 1.0)))
; 2.51
   (define (below painter1 painter2)
     (let ((split-point (make-vect 0.0 0.5)))
       (let ((paint-bot
               (transform-painter painter1
                                  (make-vect 0.0 0.0)
                                  (make-vect 1.0 0.0)
                                  split-point))
             (paint-top
               (transform-painter painter2
                                  split-point
                                  (make-vect 1.0 0.5)
                                  (make-vect 0.0 1.0))))
         (lambda (frame)
           (paint-bot frame)
           (paint-top frame)))))
   (define (below painter1 painter2)
     (let ((paint-bot (rotate270 painter1))
           (paint-top (rotate270 painter2)))
       (let ((besides (beside paint-bot paint-top)))
         (lambda (frame)
           ((rotate90 besides) frame)))))
;
   (define (memq symbol alist)
     (cond ((null? alist) #f)
           ((eq? symbol (car alist)) alist)
           (else (memq symbol (cdr alist)))))
; 2.52
   (list 'a 'b 'c) -> (a b c)
   (list (list 'george)) -> ((george))
   (cdr '((x1 x2) (y1 y2))) -> ((y1 y2))
   (cadr '((x1 x2) (y1 y2))) -> (y1 y2)
   (pair? (car '(a short list))) -> #f
   (memq 'red '((red shoes) (blue socks))) -> #f
   (memq 'red '(red shoes blue socks)) -> (red shoes blue socks)
; 2.54
   (define (equalt? list1 list2)
     (cond ((and (not (pair? list1)) (not (pair? list2))) (eq? list1 list2))
           ((and (pair? list1) (pair? list2)) (and (equalt? (car list1) (car list2)) (equalt? (cdr list1) (cdr list2))))
           (else #f)))
; 2.55
   (car ''abracadabra) ; can be rewritten as:
   (car (quote 'abracadabra))
   (car (quote (quote abracadabra)))
   ; Maybe this is easier to see:
   (car '(quote abracadabra))
   ; What is the result of (car '(x y))? Well, it is just x.
   ; So, (car '(quote abracadabra)) -> quote
;
    (define (variable? x) (symbol? x))
    (define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2)))
    (define (=number? exp num)
      (and (number? exp) (= exp num)))
    (define (make-sum a1 a2)
      (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
            (else (list '+ a1 a2))))
    (define (make-product m1 m2)
      (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
            (else (list '* m1 m2))))
    (define (sum? x)
      (and (pair? x) (eq? (car x) '+)))
    (define (addend s) (cadr s))
    (define (augend s) (caddr s))
    (define (product? x)
      (and (pair? x) (eq? (car x) '*)))
    (define (multiplier p) (cadr p))
    (define (multiplicand p) (caddr p))
    (define (deriv exp var)
      (cond ((number? exp) 0)
            ((variable? exp)
             (if (same-variable? exp var) 1 0))
            ((sum? exp)
             (make-sum (deriv (addend exp) var)
                       (deriv (augend exp) var)))
            ((product? exp)
             (make-sum
               (make-product (multiplier exp)
                             (deriv (multiplicand exp) var))
               (make-product (deriv (multiplier exp) var)
                             (multiplicand exp))))
            (else
             (error "unknown expression type -- DERIV" exp))))
; 2.56
   (define (make-exponentiation base exponent)
     (cond ((=number? exponent 0) 1)
           ((=number? exponent 1) base)
           (else (list '** base exponent))))
   (define (base exponentiation) (cadr exponentiation))
   (define (exponent exponentiation) (caddr exponentiation))
   (define (exponentiation? exp)
     (and (pair? exp) (eq? '** (car exp))))
   ; Adding derivative of exponentiation rule:
   (define (deriv exp var)
     (cond ((number? exp) 0)
           ((variable? exp)
            (if (same-variable? exp var) 1 0))
           ((sum? exp)
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var)))
           ((product? exp)
            (make-sum
              (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))
           ; Exponent rule requires 2 multiplications
           ; The exponenent is also an expression you may
           ; have to find the derivative of. (this is symbolic)
           ((exponentiation? exp)
            (make-product
              (exponent exp)
              (make-product
                (make-exponentiation (base exp) (make-sum (exponent exp) -1))
                (deriv (base exp) var))))
           (else
            (error "unknown expression type -- DERIV" exp))))
; 2.57
    ; This is a lazy answer. For a REALLY in-depth, over-the-top answer,
    ; consult http://jots-jottings.blogspot.com/2011/11/sicp-exercise-257-variable-length-sums.html
    ; He handles reduction.
    (define (make-sum a1 . an)
      (if (null? an) 
          (error "make-sum requires at least 2 parameters")
          (append (list '+ a1) an)))
    (define (addend s) (cadr s))
    (define (augend s)
      (if (> (length (cddr s)) 1)
          (append (list '+) (cddr s))
          (caddr s)))
    
    (define (make-product a1 . an)
      (if (null? an) 
          (error "make-product requires at least 2 parameters")
          (append (list '* a1) an)))
    (define (multiplier product) (cadr product))
    (define (multiplicand product)
      (if (> (length (cddr product)) 1)
          (append (list '*) (cddr product))
          (caddr product)))
    ; experimental
    (define (make-sum a1 . an)
      (cond ((null? an) (error "make-sum requires at least 2 parameters"))
            ((=number? a1 0) (append (list '+) an))
            ((and (= (length an) 1) (=number? 0 (car an))) a1)
            ((and (number? a1) (number? (car an))) (apply make-sum (list (+ a1 (car an)) an)))
            (else (append (list '+ a1) an))))
    (define (make-product m1 m2)
      (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
            (else (list '* m1 m2))))
    ; (* x y z) -> (* (* x y) z)
    (define (make-product m1 m2 . mn)
      (cond ((null? mn) (list '* m1 m2))
            ()
            ()
            ()
            ()
            (else (make-product (list '* m1 m2) (car mn) (cdr mn)))))
    (make-product a b c d e f)
    -> (make-product (list '* a b) c (def))
    -> (make-product (list '* (list '* a b) c) d (ef))
    -> (make-product (list '* (list '* (list '* a b) c) d) e (f))
    -> (make-product (list '* (list '* (list '* (list '* a b) c) d) e) f ())
    -> (list '* (list '* (list '* (list '* (list '* 'a 'b) 'c) 'd) 'e) 'f)
; 2.58
    ; a)
    (define (make-sum a1 a2)
      (list a1 '+ a2))
    (define (sum? x)
      (and (pair? x) (eq? (cadr x) '+)))
    (define (addend s) (car s))
    (define (augend s) (caddr s))
    
    (define (make-product a1 a2)
      (list a1 '* a2))
    (define (product? x)
      (and (pair? x) (eq? (cadr x) '*)))
    (define (multiplier p) (car p))
    (define (multiplicand p) (caddr p))
    ; b) - supposed to be much harder
;
    (define (element-of-set? x set)
      (cond ((null? set) false)
            ((equal? x (car set)) true)
            (else (element-of-set? x (cdr set)))))
    (define (adjoin-set x set)
      (if (element-of-set? x set)
          set
          (cons x set)))
    (define (intersection-set set1 set2)
      (cond ((or (null? set1) (null? set2)) '())
            ((element-of-set? (car set1) set2)        
             (cons (car set1)
                   (intersection-set (cdr set1) set2)))
            (else (intersection-set (cdr set1) set2))))
; 2.59
    ; Union is just the opposite of intersection, so...
    (define (union-set set1 set2)
      (cond ((null? set1) set2) 
            ((null? set2) set1)
            ((not (element-of-set? (car set1) set2))        
             (cons (car set1)
                   (union-set (cdr set1) set2)))
            (else (union-set (cdr set1) set2))))
; 2.60
    (define (element-of-set? x set)
      (cond ((null? set) false)
            ((equal? x (car set)) true)
            (else (element-of-set? x (cdr set)))))
    (define (adjoin-set x set)
        (cons x set))
    (define (intersection-set set1 set2)
      (cond ((or (null? set1) (null? set2)) '())
            ((element-of-set? (car set1) set2)        
             (cons (car set1)
                   (intersection-set (cdr set1) set2)))
            (else (intersection-set (cdr set1) set2))))
    (define (union-set set1 set2)
      (append set1 set2))
;
    (define (element-of-set? x set)
      (cond ((null? set) false)
            ((= x (car set)) true)
            ((< x (car set)) false)
            (else (element-of-set? x (cdr set)))))
    (define (intersection-set set1 set2)
      (if (or (null? set1) (null? set2))
          '()    
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1
                         (intersection-set (cdr set1)
                                           (cdr set2))))
                  ((< x1 x2)
                   (intersection-set (cdr set1) set2))
                  ((< x2 x1)
                   (intersection-set set1 (cdr set2)))))))
; 2.61
    (define (adjoin-set x set)
      (cond ((null? set) (cons x '()))
            ((= x (car set)) set)
            ((< x (car set)) (cons x set))
            (else (cons (car set) (adjoin-set x (cdr set))))))
; 2.62
    (define (union-set set1 set2)
      (cond ((null? set1) set2)
            ((null? set2) set1)
            ((= (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) (cdr set2))))
            ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
            ((> (car set1) (car set2)) (cons (car set2) (union-set set1 (cdr set2))))))
;
    (define (entry tree) (car tree))
    (define (left-branch tree) (cadr tree))
    (define (right-branch tree) (caddr tree))
    (define (make-tree entry left right)
      (list entry left right))
    (define (element-of-set? x set)
      (cond ((null? set) false)
            ((= x (entry set)) true)
            ((< x (entry set))
             (element-of-set? x (left-branch set)))
            ((> x (entry set))
             (element-of-set? x (right-branch set)))))
    (define (adjoin-set x set)
      (cond ((null? set) (make-tree x '() '()))
            ((= x (entry set)) set)
            ((< x (entry set))
             (make-tree (entry set) 
                        (adjoin-set x (left-branch set))
                        (right-branch set)))
            ((> x (entry set))
             (make-tree (entry set)
                        (left-branch set)
                        (adjoin-set x (right-branch set))))))
; 2.63
    (define (tree->list-1 tree)
      (if (null? tree)
          '()
          (append (tree->list-1 (left-branch tree))
                  (cons (entry tree)
                        (tree->list-1 (right-branch tree))))))
    (define (tree->list-2 tree)
      (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list (right-branch tree)
                                              result-list)))))
      (copy-to-list tree '()))
    ; a) Do the above 2 procedures produce the same list for every tree?
    ; It's once again time for the substitution model. Let us compare 3 trees. (Fig. 2.16)
    ; Tree 1
    (adjoin-set 11 (adjoin-set 5 (adjoin-set 1 (adjoin-set 9 (adjoin-set 3 (make-tree 7 '() '()))))))
    -> (7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))
    (tree->list-1 (7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
    (append (tree->list-1 (3 (1 () ()) (5 () ())))
                      (cons 7
                            (tree->list-1 (9 () (11 () ())))))
        (tree->list-1 (3 (1 () ()) (5 () ())))
            (append (tree->list-1 (1 () ()))
                              (cons 3
                                    (tree->list-1 (5 () ()))))        
            (tree->list-1 (1 () ()))
                (append (tree->list-1 ())
                                  (cons 1
                                        (tree->list-1 ())))
                (append () (cons 1 ()))
            -> (list 1)
            (tree->list-1 (5 () ()))
            -> (list 5)
        (append (list 1) (cons 3 (list 5)))
        (append (list 1) (list 3 5))
        (list 1 3 5)
        (tree->list-1 (9 () (11 () ())))
            (append (tree->list-1 ())
                      (cons 9
                            (tree->list-1 (11 () ()))))
            (append () (cons 9 (list 11)))
        -> (list 9 11)
    (append (list 1 3 5)
                      (cons 7
                            (list 9 11)))
    -> (list 1 3 5 7 9 11)
    ; I just ran it
    (tree->list-2 (7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
    -> (list 1 3 5 7 9 11)
    
    ; Tree 2
    (adjoin-set 11 (adjoin-set 9 (adjoin-set 5 (adjoin-set 7 (adjoin-set 1 (make-tree 3 '() '()))))))
    -> (3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))
    ; Tree 3
    (adjoin-set 11 (adjoin-set 7 (adjoin-set 1 (adjoin-set 9 (adjoin-set 3 (make-tree 5 '() '()))))))
    -> (5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))
    ; They all produce the same list
    ; b) How do their orders of growth compare? Which one is slower?
    ; Well...each of them call themselves twice each iteration, and each iteration
    ; represents going down one level of depth in the tree. As there are about n/2
    ; levels, the order of growth is O(n) where n is the number entries (for both).
; 2.64
    (define (list->tree elements)
      (car (partial-tree elements (length elements))))

    (define (partial-tree elts n)
      (if (= n 0)
          (cons '() elts)
          (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree elts left-size)))
              (let ((left-tree (car left-result))
                    (non-left-elts (cdr left-result))
                    (right-size (- n (+ left-size 1))))
                (let ((this-entry (car non-left-elts))
                      (right-result (partial-tree (cdr non-left-elts)
                                                  right-size)))
                  (let ((right-tree (car right-result))
                        (remaining-elts (cdr right-result)))
                    (cons (make-tree this-entry left-tree right-tree)
                          remaining-elts))))))))
    
    (partial-tree (1 3 5 7 9 11) 2)
        (let ((left-size (quotient (- 2 1) 2)))
                    (let ((left-result (partial-tree (1 3 5 7 9 11) left-size)))
                      (let ((left-tree (car left-result))
                            (non-left-elts (cdr left-result))
                            (right-size (- 2 (+ left-size 1))))
                        (let ((this-entry (car non-left-elts))
                              (right-result (partial-tree (cdr non-left-elts)
                                                          right-size)))
                          (let ((right-tree (car right-result))
                                (remaining-elts (cdr right-result)))
                            (cons (make-tree this-entry left-tree right-tree)
                                  remaining-elts))))))
        (let ((left-size 0))
                    (let ((left-result (partial-tree (1 3 5 7 9 11) 0)))
                      (let ((left-tree (car left-result))
                            (non-left-elts (cdr left-result))
                            (right-size (- 2 (+ 0 1))))
                        (let ((this-entry (car non-left-elts))
                              (right-result (partial-tree (cdr non-left-elts)
                                                          right-size)))
                          (let ((right-tree (car right-result))
                                (remaining-elts (cdr right-result)))
                            (cons (make-tree this-entry left-tree right-tree)
                                  remaining-elts))))))
        (let ((left-size 0))
                    (let ((left-result (() 1 3 5 7 9 11)))
                      (let ((left-tree (car (() 1 3 5 7 9 11)))
                            (non-left-elts (cdr (() 1 3 5 7 9 11)))
                            (right-size (- 2 (+ 0 1))))
                        (let ((this-entry (car non-left-elts))
                              (right-result (partial-tree (cdr non-left-elts)
                                                          right-size)))
                          (let ((right-tree (car right-result))
                                (remaining-elts (cdr right-result)))
                            (cons (make-tree this-entry left-tree right-tree)
                                  remaining-elts))))))
        (let ((left-size 0))
                    (let ((left-result (() 1 3 5 7 9 11)))
                      (let ((left-tree (()))
                            (non-left-elts (1 3 5 7 9 11))
                            (right-size 1))
                        (let ((this-entry (car (1 3 5 7 9 11)))
                              (right-result (partial-tree (cdr (1 3 5 7 9 11))
                                                          1)))
                          (let ((right-tree (car right-result))
                                (remaining-elts (cdr right-result)))
                            (cons (make-tree this-entry (()) right-tree)
                                  remaining-elts))))))
        (let ((left-size 0))
                    (let ((left-result (() 1 3 5 7 9 11)))
                      (let ((left-tree (()))
                            (non-left-elts (1 3 5 7 9 11))
                            (right-size 1))
                        (let ((this-entry 1)
                              (right-result ((3 () ()) 5 7 9 11)))
                          (let ((right-tree (car ((3 () ()) 5 7 9 11)))
                                (remaining-elts (cdr ((3 () ()) 5 7 9 11))))
                            (cons (make-tree 1 (()) right-tree)
                                  remaining-elts))))))
        (let ((left-size 0))
                    (let ((left-result (() 1 3 5 7 9 11)))
                      (let ((left-tree (()))
                            (non-left-elts (1 3 5 7 9 11))
                            (right-size 1))
                        (let ((this-entry 1)
                              (right-result ((3 () ()) 5 7 9 11)))
                          (let ((right-tree (3 () ())))
                                (remaining-elts (5 7 9 11)))
                            (cons (make-tree 1 (()) (3 () ()))
                                  (5 7 9 11)))))))
        (cons (make-tree 1 (()) (3 () ())) (5 7 9 11))
    -> ((1 () (3 () ())) 5 7 9 11)
    ; Hmmm...it is because left-size is zero that this-entry is the first element
    ; of the list. left-size=0 -> left-tree=(()) & non-left-elts=(entire_list) -> this-entry=first_element_of_list
    
    ; a) Explain how partial-tree works (remember this is for ordered lists)
    ; In the case of an even number of elements in the list, left-size = right-size - 1
    ; In the case of an odd number of elements in the list, left-size = right-size
    ; this-entry is the ceiling(n/2)th(index base 1) element (the "middle")
    ; Summarized, the partial tree is the tree with this-entry as the root, the partial tree
    ; genenerated by the first half of the list as the left branch, and the partial tree
    ; generated by the second half of the list as the right branch.
    ; Reminds me of a Cantor Set generated by repeatedly dividing by 2.
    
    ; b) What is the order of growth?
    ; There will be ~n/2 "iterations", and each "iteration" partial-tree calls itself twice.
    ; So, the order of growth is O(n).
; 2.65
    ; Hmmm...the book says to use the results of 2.63 and 2.64 to create union and
    ; intersection. 2.63 and 2.64 are all about converting trees to lists and vice versa,
    ; so I can only assume that what the book meant was to 
    ; 1. Convert the trees to lists
    ; 2. Take the union/intersection
    ; 3. Convert the union/intersection to a tree
    ; This would still be O(n) since O(3n) = O(n)
    (define (union-set-binary-tree set1 set2)
      (list->tree (union-set (tree->list-2 set1) (tree->list-2 set2))))
    (define (intersection-set-binary-tree set1 set2)
      (list->tree (intersection-set (tree->list-2 set1) (tree->list-2 set2))))
; 2.66
    (define (lookup given-key set-of-records)
      (cond ((null? (entry set-of-records)) #f)
            ((= given-key (key (entry set-of-records))) (entry set-of-records))
            ((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
            ((> given-key (key (entry set-of-records))) (lookup given-key (right-branch set-of-records)))))
;
    ; Huffman encoding trees
    ; Leaves
    (define (make-leaf symbol weight)
      (list 'leaf symbol weight))
    (define (leaf? leaf) (eq? 'leaf (car leaf)))
    (define (symbol-leaf leaf) (cadr leaf))
    (define (weight-leaf leaf) (caddr leaf))
    ; Tree
    (define (make-code-tree left right)
      (list left 
            right
            (append (symbols-tree left) (symbols-tree right))
            (+ (weight-tree left) (weight-tree right))))
    (define (left-branch tree) (car tree))
    (define (right-branch tree) (cadr tree))
    (define (symbols-tree tree)
      (if (leaf? tree)
          (list (symbol-leaf tree))
          (caddr tree)))
    (define (weight-tree tree)
      (if (leaf? tree)
          (weight-leaf tree)
          (cadddr tree)))
    (define (decode bits tree)
      (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch
                   (choose-branch (car bits) current-branch)))
              (if (leaf? next-branch)
                  (cons (symbol-leaf next-branch)
                        (decode-1 (cdr bits) tree))
                  (decode-1 (cdr bits) next-branch)))))
      (decode-1 bits tree))
    (define (choose-branch bit branch)
      (cond ((= bit 1) (right-branch branch))
            ((= bit 0) (left-branch branch))
            (else (error "bad bit -- CHOOSE-BRANCH" bit))))
    (define (adjoin-set x set)
      (cond ((null? set) (list x))
            ((< (weight-tree x) (weight-tree (car set))) (cons x set))
            (else (cons (car set)
                        (adjoin-set x (cdr set))))))
    (define (make-leaf-set pairs)
      (if (null? pairs)
          '()
          (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair)
                                   (cadr pair))
                        (make-leaf-set (cdr pairs))))))
; 2.67
    (define sample-tree
      (make-code-tree (make-leaf 'A 4)
                      (make-code-tree
                       (make-leaf 'B 2)
                       (make-code-tree (make-leaf 'D 1)
                                       (make-leaf 'C 1)))))

    (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
    (decode sample-message sample-tree)
; 2.68
    (define (encode message tree)
      (if (null? message)
          '()
          (append (encode-symbol (car message) tree)
                  (encode (cdr message) tree))))
    (define (encode-symbol symbol tree)
      (cond ((check-branch symbol (left-branch tree))
             (if (leaf? (left-branch tree))
                 (cons 0 '())
                 (cons 0 (encode-symbol symbol (left-branch tree)))))
            ((check-branch symbol (right-branch tree))
             (if (leaf? (right-branch tree))
                 (cons 1 '())
                 (cons 1 (encode-symbol symbol (right-branch tree)))))))
    (define (check-branch symbol branch)
      (if (leaf? branch)
          (eq? symbol (symbol-leaf branch))
          (element-of-set? symbol (symbols-tree branch))))
    ; test
    (equal? (encode (decode sample-message sample-tree) sample-tree) '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; 2.69
    (define (generate-huffman-tree pairs)
      (successive-merge (make-leaf-set pairs)))
    (define (successive-merge leaves)
      (if (= (length leaves) 2)
          (make-code-tree (car leaves) (cadr leaves))
          (successive-merge (adjoin-set (make-code-tree (car leaves) (cadr leaves)) (cddr leaves)))))
    
    ; test
    (define some-leaves (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))
    (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
    (generate-huffman-tree '((C 1) (D 1)))
; 2.70
    (define rock-tree
      (generate-huffman-tree
        '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))
    (define rock-msg
      '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))
    (length (encode rock-msg rock-tree)) ; -> 84
    ; The smallest number of bits we would need if we used a fixed-length code would be
    ; log2(8) = 3 bits per symbol
    ; 3 bits/symbol * 36 symbols = 108 bits
; 2.71
    ; The most frequent symbol would correspond to 0, so 1 bit.
    ; The least frequent symbol would correspond to going down the tree
    ; n-1 times, so, n-1 bits.
; 2.72
    ; Order of growth for encode-symbol (adhering to the frequncy distribution in 2.71) 
    ; is: O(n^2) where n is the number of symbols in the alphabet.
    ; We saw in 2.71 that the worst case (least-frequent symbols) scenario would involve
    ; going down the tree n-1 times (O(n)). But at each node, we also would check to see
    ; if the symbol is in the list, which is an O(n) operation.
;
    (define (make-from-real-imag ))
    (define (make-from-mag-ang ))
    (define (add-complex z1 z2)
      (make-from-real-imag (+ (real-part z1) (real-part z2))
                           (+ (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
      (make-from-real-imag (- (real-part z1) (real-part z2))
                           (- (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
      (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2))))
    (define (div-complex z1 z2)
      (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                         (- (angle z1) (angle z2))))
;
    ; Data abstraction can be seen as an application of the "principle of least commitment"
    ; It allows us to defer our decision on the choice of a concrete representation of
    ; the data. Taking this principle to a further extreme, we introduce TYPES which allow
    ; us to use multiple concrete representations by using the type to select the selectors.
    ; In SICP, we use type-tags, a weak type system.
    (define (install-rectangular-package)
      ;; internal procedures
      (define (real-part z) (car z))
      (define (imag-part z) (cdr z))
      (define (make-from-real-imag x y) (cons x y))
      (define (magnitude z)
        (sqrt (+ (square (real-part z))
                 (square (imag-part z)))))
      (define (angle z)
        (atan (imag-part z) (real-part z)))
      (define (make-from-mag-ang r a) 
        (cons (* r (cos a)) (* r (sin a))))
      ;; interface to the rest of the system
      (define (tag x) (attach-tag 'rectangular x))
      (put 'real-part '(rectangular) real-part)
      (put 'imag-part '(rectangular) imag-part)
      (put 'magnitude '(rectangular) magnitude)
      (put 'angle '(rectangular) angle)
      (put 'make-from-real-imag 'rectangular 
           (lambda (x y) (tag (make-from-real-imag x y))))
      (put 'make-from-mag-ang 'rectangular 
           (lambda (r a) (tag (make-from-mag-ang r a))))
      'done)

    (define (install-polar-package)
      ;; internal procedures
      (define (magnitude z) (car z))
      (define (angle z) (cdr z))
      (define (make-from-mag-ang r a) (cons r a))
      (define (real-part z)
        (* (magnitude z) (cos (angle z))))
      (define (imag-part z)
        (* (magnitude z) (sin (angle z))))
      (define (make-from-real-imag x y) 
        (cons (sqrt (+ (square x) (square y)))
              (atan y x)))
      ;; interface to the rest of the system
      (define (tag x) (attach-tag 'polar x))
      (put 'real-part '(polar) real-part)
      (put 'imag-part '(polar) imag-part)
      (put 'magnitude '(polar) magnitude)
      (put 'angle '(polar) angle)
      (put 'make-from-real-imag 'polar
           (lambda (x y) (tag (make-from-real-imag x y))))
      (put 'make-from-mag-ang 'polar 
           (lambda (r a) (tag (make-from-mag-ang r a))))
      'done)

    (define (apply-generic op . args)
      (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
          (if proc
              (apply proc (map contents args))
              (error
                "No method for these types -- APPLY-GENERIC"
                (list op type-tags))))))
    (define (real-part z) (apply-generic 'real-part z))
    (define (imag-part z) (apply-generic 'imag-part z))
    (define (magnitude z) (apply-generic 'magnitude z))
    (define (angle z) (apply-generic 'angle z))

    (define (make-from-real-imag x y)
      ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
      ((get 'make-from-mag-ang 'polar) r a))
; 2.73
    ; a)
    ; We have made a table with deriv as an operator and + and * as types.
    ; We can't assimlate the predicates number? and same-variable? because they
    ; have the types they check for are not type-tagged (what would the put operation
    ; look like? (put 'deriv '(??) deriv)).
    ; b)
    (define (install-deriv-package)
      ;; internal procedures
      ;; make-sum constructor here (takes place of tag)
      ;; addend and augend selectors here
      (define (deriv-+ exp var)
        (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var)))
      ;; make-product constructor here (takes place of tag)
      ;; multiplier and multiplicand selectors here
      (define (deriv-* exp var)
        (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
      ;; interface to the rest of the system
      (put 'deriv '(+) deriv-+)
      (put 'deriv '(*) deriv-*))
    ; c)
    ; Add this code to the install-deriv-package
   (define (make-exponentiation base exponent)
     (cond ((=number? exponent 0) 1)
           ((=number? exponent 1) base)
           (else (list '** base exponent))))
   (define (base exponentiation) (cadr exponentiation))
   (define (exponent exponentiation) (caddr exponentiation))
    (define (deriv-** exp var)
      (make-product
          (exponent exp)
          (make-product
            (make-exponentiation (base exp) (make-sum (exponent exp) -1))
            (deriv (base exp) var))))
    (put 'deriv '(**) deriv-**)
    ; d)
    ; It looks like the only change we would require is a reversal on the put functions
    (put '(+) '(deriv) deriv-+)
    (put '(*) '(deriv) deriv-*)
    (put '(**) '(deriv) deriv-**)
; 2.74
    ; a)
    ; If we treat each divison as a type (division1, divison2, etc.), then
    ; we can "dispatch on division" and run the appropriate get-record function
    (define (get-record employee-id file)
      ((get 'get-record (division file)) employee-id file))
    ; b)
    (define (get-salary employee-record)
      ((get 'get-salary (division employee-record)) employee-record))
    ; c)
    ; Let's assume that get-record returns false if an employee is not
    ; in that division. Notice that this handles the case where an employee
    ; belongs to multiple divisions.
    (define (find-employee-record employee-id division-files)
      (let ((list-of-records (filter 
                               (lambda (file?) (not (eq? file? '#f)))
                               (map (lambda (file) (get-record employee-id file)) division-files))))
        (if (null? list-of-records)
            #f
            list-of-records)))
    ; d)
    ; The only changes required would be to write add the appropriate get-record
    ; and get-salary operations to the table for a new division.
;
    ; Message passing uses a functional representation of data aka the data IS the procedure
    ; just like when we saw a pair being represented by a procedure, or positive integers
    ; represented by procedures (Church numerals). Observe:
    (define (make-from-real-imag x y)
      (define (dispatch op)
        (cond ((eq? op 'real-part) x)
              ((eq? op 'imag-part) y)
              ((eq? op 'magnitude)
               (sqrt (+ (square x) (square y))))
              ((eq? op 'angle) (atan y x))
              (else
               (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
      dispatch)
    (define (apply-generic op arg) (arg op))
    (define z1 (make-from-real-imag 2 3))
    (apply-generic 'real-part z1) ;-> 2
    ; apply-generic passes the message 'real-part' to z1; Hence the name
; 2.75
    ; "Object-oriented"? But the "object" is a function that returns another function.
    (define (make-from-mag-ang r phi)
      (define (dispatch op)
        (cond ((eq? op 'real-part) (* r (sin phi)))
              ((eq? op 'imag-part) (* r (cos phi)))
              ((eq? op 'magnitude) r)
              ((eq? op 'angle) phi)
              (else
                (error "Unknown op -- MAKE-FROM-MAG-ANG")))
        dispatch))
; 2.76
    ; Generic operations w/ explicit dispatch is a good strategy if the types don't change
    ; that often, but operations are added frequently because you only have to add 
    ; Explicit dispatch
    ;   Add new types (concrete representations)
    ;       Each operation must be modified to handle the new types
    ;       Ex: adding polar representation would add to the conditional
    ;       expression in the operation real-part
    ;   Add new operations
    ;       The operation must make sure to properly dispatch on existing types
    ;   Decent strategy if you add new operations frequently
    ; Message passing
    ;   Add new types
    ;       The new type must make sure to implement each operation appropriately aka
    ;       process the messages appropriately
    ;   Add new operations
    ;       We must implement the new operations in each type aka each type must be able
    ;       to handle new messages
    ;   Decent strategy if you add new types frequently
    ; A summary of the above 2 strategies: operations must be aware of (and handle) types
    ; or types must be aware of (and handle) operations
    ; Data-directed
    ;   Add new types
    ;       Write operations for new types and (put ...) them into the table
    ;   Add new operations
    ;       Write them for each type and (put ...) them into the table
    ; In the data-directed, types and operations are put onto "equal footing"
;
    (define (add x y) (apply-generic 'add x y))
    (define (sub x y) (apply-generic 'sub x y))
    (define (mul x y) (apply-generic 'mul x y))
    (define (div x y) (apply-generic 'div x y))

    (define (install-scheme-number-package)
      ;; interface to rest of the system
      (define (tag x)
        (attach-tag 'scheme-number x))    
      (put 'add '(scheme-number scheme-number)
           (lambda (x y) (tag (+ x y))))
      (put 'sub '(scheme-number scheme-number)
           (lambda (x y) (tag (- x y))))
      (put 'mul '(scheme-number scheme-number)
           (lambda (x y) (tag (* x y))))
      (put 'div '(scheme-number scheme-number)
           (lambda (x y) (tag (/ x y))))
      (put 'make 'scheme-number
           (lambda (x) (tag x)))
      'done)
    (define (make-scheme-number n)
      ((get 'make 'scheme-number) n))

    (define (install-rational-package)
      ;; internal procedures
      (define (numer x) (car x))
      (define (denom x) (cdr x))
      (define (make-rat n d)
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g))))
      (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
      (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
      (define (mul-rat x y)
        (make-rat (* (numer x) (numer y))
                  (* (denom x) (denom y))))
      (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer y))))
      ;; interface to rest of the system
      (define (tag x) (attach-tag 'rational x))
      (put 'add '(rational rational)
           (lambda (x y) (tag (add-rat x y))))
      (put 'sub '(rational rational)
           (lambda (x y) (tag (sub-rat x y))))
      (put 'mul '(rational rational)
           (lambda (x y) (tag (mul-rat x y))))
      (put 'div '(rational rational)
           (lambda (x y) (tag (div-rat x y))))

      (put 'make 'rational
           (lambda (n d) (tag (make-rat n d))))
      'done)
    (define (make-rational n d)
      ((get 'make 'rational) n d))

    (define (install-complex-package)
      ;; imported procedures from rectangular and polar packages
      (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
      (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))
      ;; internal procedures
      (define (add-complex z1 z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                             (+ (imag-part z1) (imag-part z2))))
      (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                             (- (imag-part z1) (imag-part z2))))
      (define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                           (+ (angle z1) (angle z2))))
      (define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                           (- (angle z1) (angle z2))))
      ;; interface to rest of the system
      (define (tag z) (attach-tag 'complex z))
      (put 'add '(complex complex)
           (lambda (z1 z2) (tag (add-complex z1 z2))))
      (put 'sub '(complex complex)
           (lambda (z1 z2) (tag (sub-complex z1 z2))))
      (put 'mul '(complex complex)
           (lambda (z1 z2) (tag (mul-complex z1 z2))))
      (put 'div '(complex complex)
           (lambda (z1 z2) (tag (div-complex z1 z2))))
      (put 'make-from-real-imag 'complex
           (lambda (x y) (tag (make-from-real-imag x y))))
      (put 'make-from-mag-ang 'complex
           (lambda (r a) (tag (make-from-mag-ang r a))))
      'done)
    (define (make-complex-from-real-imag x y)
      ((get 'make-from-real-imag 'complex) x y))
    (define (make-complex-from-mag-ang r a)
      ((get 'make-from-mag-ang 'complex) r a))
; 2.77
    ; Put into complex package
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    ; --->magnitude(generic)--->apply-generic--->magnitude(generic)--->apply-generic--->magnitude(rectangular)--->5
    ; A more detailed explanation is in my notebook (w/ diagrams)
; 2.78
    ; Let's take advantage of Scheme's internal type system
    (define (attach-tag type-tag contents)
      (if (number? contents)
          contents
          (cons type-tag contents)))
    (define (type-tag datum)
      (cond ((number? datum) 'scheme-number)
            ((pair? datum) (car datum))
            (else (error "Bad tagged datum -- TYPE-TAG" datum))))
    (define (contents datum)
      (cond ((number? datum) datum)
            ((pair? datum) (cdr datum))
            (else (error "Bad tagged datum -- CONTENTS" datum))))
; 2.79
    ; add this to scheme-number-package
    (put 'equ '(scheme-number scheme-number) =)
    ; add this to rational-package
    (define (equ rat1 rat2)
      (and (= (num rat1) (num rat2)) 
           (= (denom rat1) (denom rat2))))
    (put 'equ '(rational rational) equ)
    ; add this to the complex-package
    (define (equ comp1 comp2)
      (and (= (real-part comp1) (real-part comp2)) 
           (= (imag-part comp1) (imag-part comp2))))
    (put 'equ '(complex complex) equ)
    
    (define (equ? x y)
      (apply-generic 'equ? x y))
; 2.80
    ; add this to scheme-number-package
    (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
    ; add this to rational-package
    (define (=zero? x)
      (= (num x) 0))
    (put '=zero? '(rational) =zero?)
    ; add this to complex-package
    (define (=zero? x)
      (and (= (real-part x) 0) (= (imag-part x) 0)))
    (put '=zero? '(complex) =zero?)

    (define (=zero? x)
      (apply-generic '=zero? x))
;
    ; Get types for args
    ; Get operation for type
    ;   if exists, apply
    ;   if not, 
    ;       check if there were exactly 2 arguments
    ;           if so, try coercion and apply-generic again w/ new types
    ;           if not, give up
    (define (apply-generic op . args)
      (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
          (if proc
              (apply proc (map contents args))
              (if (= (length args) 2)
                  (let ((type1 (car type-tags))
                        (type2 (cadr type-tags))
                        (a1 (car args))
                        (a2 (cadr args)))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                              (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                              (apply-generic op a1 (t2->t1 a2)))
                            (else
                              (error "No method for these types"
                                     (list op type-tags))))))
                  (error "No method for these types"
                         (list op type-tags)))))))
; 2.81
    ; Louis wants to put this in:
    (define (scheme-number->scheme-number n) n)
    (define (complex->complex z) z)
    (put-coercion 'scheme-number 'scheme-number
                  scheme-number->scheme-number)
    (put-coercion 'complex 'complex complex->complex)
    ; a) With above procedures installed, what would happen if apply-generic
    ; is called w/ 2 arguments of type scheme-number or complex and that particular
    ; operation cannot be found?
    ; Say we have
    (define (exp x y) (apply-generic 'exp x y))
    ; and put it into the scheme-number package
    (put 'exp '(scheme-number scheme-number)
         (lambda (x y) (tag (expt x y))))
    ; and called exp w/ 2 complex numbers as arguments?
    (apply-generic 'exp complex1 complex2)
    ; We would reach the (cond (t1->t2 ...)) statement
    ; and run (apply-generic op (t1->t2 a1) a2)...and end
    ; up in an infinite loop.
    
    ; b)
    ; Well, the answer is in the next question: no, apply-generic does not work correctly as is.
    ; If the operation does not exist for a type AND there were 2 arguments passed in, apply-generic
    ; will try coercion, regardless of whether or not the 2 arguments are the same type
    ; c)
    ; Modfiy the second (if...) expression:
    (if (and (not (= (car args) (cadr args))) (= (length args 2)))
        ...)
    ; OR even better (from Eli's website)
    ; insert an (if...) expression after the third let (right before the fourth)
    (if (not (= type1 type2))
        ...)
; 2.82
    ; Map type-tags to coercion fns
    (let ((coersion-fns
            (map (lambda (x) 
                (if (= (car type-tags) x)
                    (lambda (y) y)
                    (get-coercion x (car type-tags)))) type-tags)))
      )
    ; I need a way of checking to see if any of the values in coerced-args is #f
    (define (no-false? list)
      (cond ((null? list) #t)
            ((eq? (car list) #f) #f)
            (else (no-false? (cdr list)))))
    ; I need a way of applying a list of fns to a list of arguments
    (define (apply-zip list-of-fns list-of-args)
      (if (null? list-of-fns)
          list-of-args
          (cons 
            ((car list-of-fns) (car list-of-args))
            (apply-zip (cdr list-of-fns) (cdr list-of-args)))))
    ; If no-false? -> #t then apply-generic
    ; if no-false? -> #f then we have to try coercing to the next arg in line
    ;   if no arg next in line, give up
    ; Pretend type-tags is in scope
    (define (some-name type-tags-to-try)
      (let ((coersion-fns
        (map (lambda (x) 
            (if (= (car type-tags-to-try) x)
                (lambda (y) y)
                (get-coercion x (car type-tags-to-try)))) type-tags)))
        (cond ((null? type-tags-to-try)
               (error "No methods for these types"))
              ((no-false? coersion-fns)
               (apply apply-generic op (apply-zip coersion-fns args)))
              (else (some-name (cdr type-tags-to-try))))))
    ; Putting it all together                
    (define (apply-generic op . args)
      (let ((type-tags (map type-tag args)))
        (define (some-name type-tags-to-try)
          (let ((coersion-fns
            (map (lambda (x) 
                (if (= (car type-tags-to-try) x)
                    (lambda (y) y)
                    (get-coercion x (car type-tags-to-try)))) type-tags)))
            (cond ((null? type-tags-to-try)
                   (error "No methods for these types"))
                  ((no-false? coersion-fns)
                   (apply apply-generic op (apply-zip coersion-fns args)))
                  (else (some-name (cdr type-tags-to-try))))))
        (let ((proc (get op type-tags)))
          (if proc
              (apply proc (map contents args))
              (some-name type-tags)))))
; 2.83
    (define (supertype type)
      (cond ((eq? type 'integer) 'rational)
            ((eq? type 'rational) 'real)
            ((eq? type 'real) 'complex)
            ((eq? type 'complex) 'complex)))
    ; Getting the right tag attached is easy...
    ; How do we adjust the concrete representation?
    (define (raise datum)
      (cons (supertype (type-tag datum)) (contents datum)))
    