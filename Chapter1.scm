; 1.03
	; setup
	(define (square a) a*a)
	(define (sum-of-squares a b) (+ (square a) (square b)))
	; answer
	(define (two-greatest-sum-of-squares a b c)
		(if (>= a b) 
			(if (>= b c) 
				(sum-of-squares a b)
			(sum-of-squares a c))
		(if (>= a c)
			(sum-of-squares a b)
		(sum-of-squares b c))))
; 1.06
	; Why does new-if not behave the same way as regular if? aka Why does if need its own evaluation rule?
	(define (new-if predicate then-clause else-clause)
		(cond (predicate then-clause)
			  (#t else-clause)
		)
	)
	(define (sqrt-iter guess x)
	  (new-if (good-enough? guess x)
	      guess
	      (sqrt-iter (improve guess x)
	                 x)))
	; To find out, we must use the substitution model:
	; Let's evaluate (sqrt-iter guess x)
	; "We begin by retrieving the body of (sqrt-iter guess x)":
	  (new-if (good-enough? guess x)
	      guess
	      (sqrt-iter (improve guess x)
	                 x))
	; Now, we must evaluate the parameters of new-if: (good-enough? guess x), guess, and (sqrt-iter (improve guess x) x).
	; Ahhhh, but now we recurse into an infinite loop.

	; Another example: Let's evaluate (new-if (= 2 3) (display "yes") (display "no"))

	; "We begin be retrieving the body of [new-if]":
	; (cond (predicate then-clause)
	;		(#t else-clause)
	; )

	; "Then we replace the formal parameter[s predicate, then-clause, and else-clause]
	; by the argument[s (= 2 3), (display "yes"), and (display "no")":
	; (cond ( (= 2 3) (display "yes") )
	;		( #t (display "no") )	
	; )
	; "Thus the problem reduces the the evaluation of a combination with two operands
	; an operator cond...We must evaluate the operator to get the procedure to be applied,
	; and we must evaluate the operands to get the arguments."
	; Evaluating ( (= 2 3) (display "yes") ), we end up displaying "yes" in terminal.
	; Evaluating (#t (display "no") ), we end up displaying "no" in terminal.
	; We end up with "noyes" being printed.

	; The point is, because new-if is a newly defined procedure, the interpreter will execute
	; it using the general applicative order model, and hence end up evaluating BOTH the then-clause
	; AND the else-clause.
	; If we had just done,
	; (cond ( (= 2 3) (display "yes") )
	;		( #t (display "no") )
	; )
	; Without invoking new-if, the interpreter would have instead evaluated the expression 
	; using the special rule for cond, and executed the expected behaviour.
; 1.07
	; good-enough? will not be good for numbers <= the square of the
	; threshold. For example, (< (abs (- (square guess) 0.000001)) 0.001). 
	; abs(guess - 0.000001) will be less than 0.001 as soon as the guess is
	; less than 0.001, which can still be orders of magnitude different from
	; 0.000001. For large numbers, say, abs(guess - 100000000)
	(define (good-enough? guess improved-guess)
		(< (abs (- guess improved-guess)) (* guess 0.01))
	)
	(define (sqrt-iter guess x)
		(if (good-enough? guess (improve guess x))
				guess
				(sqrt-iter (improve guess x) x)
		)
	)
	; Is this new good-enough? procedure better? Why? Would it have been OK just
	; to make it so that we measure the distance between x and the square of the
	; guess still, but make it so that the threshold is a fraction of x? Why is
	; watching how guess changes from one iteration to the next better than
	; checking the distance between (square guess) and x? Either way, it is clear
	; why making the threshold dependent on guess or x a better thing then a static
	; threshold.
; 1.08
	; Just change improve guess procedure
	(define (improve guess x)
		(/ (+ (/ x (square guess)) (* 2 guess)) 3)
	)
; 1.09
	(define (+ a b)
		(if (= a 0)
			b
			(inc (+ (dec a) b))
		)
	)
	; (+ 4 5)
	-> (if (= 4 0)
		   5
		   (inc (+ (dec 4) 5))
	   )
	-> (inc (+ (dec 4) 5))
	-> (inc (+ 3 5))
	-> (inc (if (= 3 0)
				5
				(inc (+ (dec 3) 5))
			)
		)
	-> (inc (inc (+ (dec 3) 5)))
	-> (inc (inc (+ 2 5)))
	-> (inc (inc (if (= 2 0)
					 5
					 (inc (+ (dec 2) 5))
				 )
			)
		)		
	-> (inc (inc (inc (+ (dec 2) 5))))
	-> (inc (inc (inc (+ 1 5))))
	-> (inc (inc (inc (if (= 1 0)
					  5
					  (inc (+ (dec 1) 5))
				   	  )
				 )
			)
		)
	-> (inc (inc (inc (inc (+ (dec 1) 5)))))
	-> (inc (inc (inc (inc (+ 0 5)))))
	-> (inc (inc (inc (inc 	(if (= 0 0)
								5
								(inc (+ (dec 0) 5))
							)
					   )
				 )
			)
		)
	-> (inc (inc (inc (inc 5))))
	-> (inc (inc (inc 6)))
	-> (inc (inc 7))
	-> (inc 8)
	-> 9
	(define (+ a b)
		(if (= a 0)
			b
			(+ (dec a) (inc b))
		)
	)
	; (+ 4 5)
	-> (if (= a 0)
		   b
		   (+ (dec a) (inc b))
	   )
	-> (if (= 4 0)
		   5
		   (+ (dec 4) (inc 5))
	   )
	-> (+ (dec 4) (inc 5))
	-> (+ 3 6)
	-> (if (= 3 0)
		   6
		   (+ (dec 3) (inc 6))
	   )
	-> (+ (dec 3) (inc 6))
	-> (+ 2 7)
	-> (if (= 2 0)
		   7
		   (+ (dec 2) (inc 7))
	   )
	-> (+ (dec 2) (inc 7))
	-> (+ 1 8)
	-> (if (= 1 0)
		   8
		   (+ (dec 1) (inc 8))
	   )
	-> (+ 0 9)
	-> (if (= 0 0)
		   9
		   (+ (dec 0) (inc 9))
	   )
	-> 9
;1.10
	(define (A x y)
	  (cond ((= y 0) 0)
	        ((= x 0) (* 2 y))
	        ((= y 1) 2)
	        (else (A (- x 1) (A x (- y 1))))))
	; Evaluate (A 1 10)
	  (cond ((= 10 0) 0)
	        ((= 1 0) (* 2 y))
	        ((= 10 1) 2)
	        (else (A (- 1 1)
	                 (A 1 (- 10 1)))))
	  -> (A (- 1 1) (A 1 (- 10 1)))
	  -> (A 0 (cond ((= 9 0) 0)
	        	((= 1 0) (* 2 y))
	        	((= 9 1) 2)
	        	(else (A (- 1 1)
	                 (A 1 (- 9 1))))))
	  -> (A 0 (A (- 1 1) (A 1 (- 9 1))))
	  ; Ok, so it looks like both x to reach 0 and y to reach 1 for base case.
	  ; The outer-most A in the else statement terminates when x = 0 while 
	  ; the inner A inside of the outer-most A relies on y to go to 1 in order to terminate.
	  ; This is because the outer-most A uses the decrement of x as a parameter, while the
	  ; the inner A uses the decrement of y as its parameter.
	  -> (A 0 (A (- 1 1) (A 1 8)))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A 1 (- 8 1)))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A 1 7))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A 1 (- 8 1))))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A 1 7)))))
	  ; and so on....
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A 1 1))))))))))
	  ; Now, the collapse
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) 2)))))))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A 0 2)))))))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) 4))))))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A 0 4))))))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) 8)))))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A 0 8)))))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) 16))))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A 0 16))))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) 32)))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) (A 0 32)))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A (- 1 1) 64))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) (A 0 64))))
	  -> (A 0 (A (- 1 1) (A (- 1 1) 128)))
	  -> (A 0 (A (- 1 1) (A 0 128)))
	  -> (A 0 (A (- 1 1) 256))
	  -> (A 0 (A 0 256))
	  -> (A 0 512)
	  -> 1024
	  ; Evaluate (A 2 4)
	  (cond ((= 4 0) 0)
	        ((= 2 0) (* 2 y))
	        ((= 4 1) 2)
	        (else (A (- 2 1)
	                 (A 2 (- 4 1)))))
	  -> (A (- 2 1) (A 2 (- 4 1)))
	  -> (A (- 2 1) (A 2 3))
	  ; Hypothesis: Now, when we reach the base case for y, we will have to again expand for x, before contracting for x
	  ; Aka, this Linearly Recursive process depends on TWO index/state variables, x and y.
	  ; Note that this procedure is both the outer-most operator AND the inner-most - so it is Linearly Recursive
	  ; Epiphany: In the iterative case, the variable that is depended on by the base case is the index/state variable - 
	  ; aka. the base case defines which variable to use as state
	  -> (A (- 2 1) (A (- 2 1) (A 2 (- 3 1))))
	  -> (A (- 2 1) (A (- 2 1) (A 2 2)))
	  -> (A (- 2 1) (A (- 2 1) (A (- 2 1) (A 2 (- 2 1)))))
	  -> (A (- 2 1) (A (- 2 1) (A (- 2 1) (A 2 1))))
	  -> (A (- 2 1) (A (- 2 1) (A (- 2 1) 2)))
	  -> (A (- 2 1) (A (- 2 1) (A 1 2)))
	  -> (A (- 2 1) (A (- 2 1) (A (- 1 1) (A 1 (- 2 1)))))
	  -> (A (- 2 1) (A (- 2 1) (A (- 1 1) (A 1 1))))
	  -> (A (- 2 1) (A (- 2 1) (A (- 1 1) 2)))
	  -> (A (- 2 1) (A (- 2 1) (A 0 2)))
	  -> (A (- 2 1) (A (- 2 1) 4))
	  -> (A (- 2 1) (A 1 4))
	  -> (A (- 2 1) (A (- 1 1) (A 1 (- 4 1))))
	  -> (A (- 2 1) (A (- 1 1) (A 1 3)))
	  -> (A (- 2 1) (A (- 1 1) (A (- 1 1) (A 1 (- 3 1)))))
	  -> (A (- 2 1) (A (- 1 1) (A (- 1 1) (A 1 2))))
	  -> (A (- 2 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A 1 (- 2 1))))))
	  -> (A (- 2 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) (A 1 1)))))
	  -> (A (- 2 1) (A (- 1 1) (A (- 1 1) (A (- 1 1) 2))))
	  -> (A (- 2 1) (A (- 1 1) (A (- 1 1) (A 0 2))))
	  -> (A (- 2 1) (A (- 1 1) (A (- 1 1) 4)))
	  -> (A (- 2 1) (A (- 1 1) (A 0 4)))
	  -> (A (- 2 1) (A (- 1 1) 8))
	  -> (A (- 2 1) (A 0 8))
	  -> (A (- 2 1) 16)
	  -> (A 1 16)
	  ; (A 1 y) calculates 2^y so...
	  -> 65536 ; 2^16
	  ; New hypothesis: this procedure calculates 2^(y^x) - WRONG
	  ; Hypothesis: This procedure takes up 
	  ; (A 3 3) calculates 2^(3^3) = 2^27? - WRONG
	  (cond ((= 3 0) 0)
	        ((= 3 0) (* 2 y))
	        ((= 3 1) 2)
	        (else (A (- 3 1) (A 3 (- 3 1)))))
	  -> (A (- 3 1) (A 3 (- 3 1)))
	  -> (A (- 3 1) (A 3 2))
	  -> (A (- 3 1) (A (- 3 1) (A 3 (- 2 1))))
	  -> (A (- 3 1) (A (- 3 1) (A 3 1)))
	  -> (A (- 3 1) (A (- 3 1) 2))
	  -> (A (- 3 1) (A 2 2))
	  -> (A (- 3 1) (A (- 2 1) (A 2 (- 2 1))))
	  -> (A (- 3 1) (A (- 2 1) (A 2 1)))
	  -> (A (- 3 1) (A (- 2 1) 2))
	  -> (A (- 3 1) (A 1 2))
	  -> (A (- 3 1) (A (- 1 1) (A 1 (- 2 1))))
	  -> (A (- 3 1) (A (- 1 1) (A 1 1)))
	  -> (A (- 3 1) (A (- 1 1) 2))
	  -> (A (- 3 1) (A 0 2))
	  -> (A (- 3 1) 4)
	  -> (A 2 4)
	  -> 65536

	(define (A x y)
	  (cond ((= y 0) 0)
	        ((= x 0) (* 2 y))
	        ((= y 1) 2)
	        (else (A (- x 1) (A x (- y 1))))))
	; calculates 2*n
	(define (f n) (A 0 n))
	; calculates 2^n
	(define (g n) (A 1 n))
	; calculates 2^(n^2)
	(define (h n) (A 2 n))
	; calculates 5(n^2)
	(define (k n) (* 5 n n))

;
	; If a procedure describes itself as being the operand, then it is Linearly Recursive.
	; If a procedure describes itself as being the operator (being the "outermost" operator), then it is Iterative.
	; If BOTH, then it is Linearly Recursive aka If there is any operator "outside" the recursive call, then it is Linearly Recursive
; Analysis of recursive Fibinacci
	; "...it is not hard to show that the number of times the procedure will compute (fib 1) or (fib 0) is precisely Fib(n+1)." Why?
	; B/c this procedure eventually comes down to adding a bunch of zeros and ones. Well, there must be at least Fib(n) 1s to add up.
	; But why are there Fib(n-1) zeroes? 
	; Both Fib(0) and Fib(1) are computed to compute Fib(2). However, only Fib(1) is computed to compute Fib(3).
	; Notice that the number of times Fib(3) or Fib(2) is computed is Fib(n).
	; It follows that the number of times Fib(2) is computed is Fib(n-1).
	; Why are there Fib(n-1) computations of Fib(2) then? Is this an easier question to answer?
	; Notice that there will always be a computation of Fib(2) whenever there is a computation of Fib(3) or Fib(4)
	; It is because the computation of Fib(3) boils down to the computation of Fib(2) as well.

	; "...the space required grows only linearly with the input, because we need keep track only of which nodes are above 
	; us in the tree at any point in the computation." aka We don't hold the entire tree in memory at once; just paths
	; from the highest node to a leaf. So the most memory we would need would be the amount to hold all the nodes along the
	; path from the highest node to the deepest leaf. aka space is proportional to the max depth of the tree
; 1.11
	; Tree recursive
	(define (f n)
		(cond ((< n 3) n)
			  (else
			  	(+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))
			  )
		)
	)
	; Iterative
	(define (f-iter n)
		(f-iter-implementation 2 1 0 n)
	)
	(define (f-iter-implementation a b c count)
		(if (= count 0) 
			c
			(f-iter-implementation (+ a (* 2 b) (* 3 c)) a b (- count 1))
		)
	)
	; I noticed that the recursive formulation solves the problem by breaking down the problem into smaller problems (solving "backwards")
	; While the iterative formulation solves the problem by solving smaller problems and building up (solving "forwards"/"layering")
	; Recursive formulation involves breaking down the big problem into its "basis" problems, then combining the solution to the "basis" problems through composition.
	; The iterative formulation starts from initial conditions (which is a basis problem) and solves 1 time-step at a time, updating its state variables accordingly.
	; There must be one state variable to hold the n+1 iteration, and one or more other state variables to hold nth, n-1st, n-2nd,...etc. iterations.
	; In iteration, the process carries its own state. The process itself is "bigger". In recursion however, the process is more compact, but it requires outside help
	; to hold state. In this sense, are recursive processes more "pure" as processes?
; 1.12
	(define (pascal x y)
		(if (or (= y 0) (= y x))
			1
			(+ (pascal (- x 1) (- y 1)) (pascal (- x 1) y))
		)
	)
; 1.14
	(define (cc amount kinds-of-coins)
	  (cond ((= amount 0) 1)
	        ((or (< amount 0) (= kinds-of-coins 0)) 0)
	        (else (+ (cc amount
	                     (- kinds-of-coins 1))
	                 (cc (- amount
	                        (first-denomination kinds-of-coins))
	                     kinds-of-coins)))))

	  (cond ((= 11 0) 1)
	        ((or (< 11 0) (= 5 0)) 0)
	        (else (+ (cc 11
	                     (- 5 1))
	                 (cc (- 11
	                        (first-denomination 5))
	                     5))))
	  (+ 
	  	(cc 11 (- 5 1)) 
	  	(cc (- 11 (first-denomination 5)) 5)
	  )
	  (+ 
	  	(cc 11 4) 
	  	(cc (- 11 50) 5)
	  )
	  (+ 
	  	(cc 11 4) 
	  	(cc -39 5)
	  )
	; The space complexity is linearly proportional to the amount to be changed. O(n)
	; To find the time complexity, let us analyze cc(n, m), starting from m = 1 up to m = 5.
	; ...The time complexity is O(n^m).
	; The time complexity is the number of ways the amount can be changed aka (cc n 5).
	; -- IGNORE --
	; The argument is as follows: The answer is the result of summing a bunch of 0s and 1s.
	; Therefore, the amount of 1s must be the number of ways to turn the given amount into change. 
	; -- IGNORE --
; 1.15
	(define (cube x) (* x x x))
	(define (p x) (- (* 3 x) (* 4 (cube x))))
	(define (sine angle)
	   (if (not (> (abs angle) 0.01))
	       angle
	       (p (sine (/ angle 3.0)))))
	; a) p will be applied 5 times. On the sixth recursive call, it will return (/ 12.15 15)
	; and the process will start collapsing into the answer
	; b) As the input size triples, the procedure will grow by a single call. Therefore the 
	;    time and space complexity will grow as log(n). a/(3^x) < 0.1
;
	(define (even? n)
		(= (remainder n 2) 0)
	)
;1.16
	; The book gave several hints:
	; 1) add an extra state variable a
	; 2) Make sure that any state transitions keeps the quantity a(b^n) invariant
	; I don't feel like I really "figured it out" on my own. I literally just made
	; sure 2) was satisfied, and the code just worked. This brings up many questions.
	(define (fast-expt-iter b n)
		(fast-expt-iter-impl b n 1)
	)
	(define (fast-expt-iter-impl b n a)
		(cond ((= n 0) a)
		      ((even? n) (fast-expt-iter-impl (square b) (/ n 2) a))
		      (else (fast-expt-iter-impl b (- n 1) (* a b)))
		)
	)
	; analysis through substitution model: (fast-expt-iter 2 5)
	-> (fast-expt-iter-impl 2 5 1)
	-> (cond ((= 5 0) 1)
		     ((even? 5) (fast-expt-iter-impl (square 2) (/ 5 2) 1))
		     (else (fast-expt-iter-impl 2 (- 5 1) (* 1 2)))
	   )
	-> (fast-expt-iter-impl 2 (- 5 1) (* 1 2))
	-> (fast-expt-iter-impl 2 4 2)
	-> (cond ((= 4 0) 2)
		     ((even? 4) (fast-expt-iter-impl (square 2) (/ 4 2) 2))
		     (else (fast-expt-iter-impl 2 (- 4 1) (* 2 2)))
	   )
	-> (fast-expt-iter-impl (square 2) (/ 4 2) 2)
	-> (fast-expt-iter-impl 4 2 2)
	-> (cond ((= 2 0) 2)
		     ((even? 2) (fast-expt-iter-impl (square 4) (/ 2 2) 2))
		     (else (fast-expt-iter-impl 4 (- 2 1) (* 2 4)))
	   )
	-> (fast-expt-iter-impl (square 4) (/ 2 2) 2)
	-> (fast-expt-iter-impl 16 1 2)
	-> (cond ((= 1 0) 2)
		     ((even? 1) (fast-expt-iter-impl (square 16) (/ 1 2) 2))
		     (else (fast-expt-iter-impl 16 (- 1 1) (* 2 16)))
	   )
	-> (fast-expt-iter-impl 16 (- 1 1) (* 2 16))
	-> (fast-expt-iter-impl 16 0 32)
	-> (cond ((= 0 0) 32)
		     ((even? 0) (fast-expt-iter-impl (square 16) (/ 0 2) 32))
		     (else (fast-expt-iter-impl 16 (- 0 1) (* 32 16)))
	   )
	-> 32
	; Notice that n will always reach 2, 1, and 0. It is easy to see that if n reaches 2, 
	; then it will reach 1 (2/2) and eventually 0 (1-1). But why would n always eventually reach 2?
	
	; Where might a(b^n) have come from? Well, notice that based on our initial state (a=1) this invariant
	; quantity is actually the answer we are looking for. Also the operator combining the accumulator and
	; the operands is the underlying operator the function is composed of.
	; In summary, the invariant quantity should satisfy these constraints:
	; 1) It should be the answer
	; 2) It must have an accumulator
	;	a) The operator "combining" the accumulator and the operands is the underlying operator that
	;		the function is composed of. For exponentiation, it would be multiplication. For multiplication,
	;		it would be addition.
	;	b) The initial state of the accumulator is given by the way it is combined with the operands
	;	   and the fact that the entire expression must be the answer
; 1.17
	; exactly analogous to the recursive fast-expt algorithm
	(define (new* a b)
		(cond ((= b 0) 0)
		      ((even? b) (double (new* a (half b))))
		      (else (+ a (new* a (- b 1))))
		)
	)
; 1.18
	; exactly analogous to 1.16
	; Here, the invariant quantity would be c + ab
	(define (new*-iter a b)
		(new*-iter-impl a b 0)
	)
	(define (new*-iter-impl a b acc)
		(cond ((= b 0) acc)
		      ((even? b) (new*-iter-impl (double a) (/ b 2) acc))
		      (else (new*-iter-impl a (- b 1) (+ acc a)))
		)
	)
	; The main idea, is to have some sort of accumulator(acc) that acts
	; as a "stack" that build up as the operands (a and b) are modified.
	; Notice that the operand that is not the counter (a) must combine with
	; the accumulator at some point in the processs (like in the odd case)
; 1.19
	; Using some linear algebra, it is not hard to show that
	; p' = (q^2) + (p^2) and
	; q' = q(q+p) + qp
	(define (fib n)
	  (fib-iter 1 0 0 1 n))
	(define (fib-iter a b p q count)
	  (cond ((= count 0) b)
	        ((even? count)
	         (fib-iter a
	                   b
	                   (pp p q)      ; compute p'
	                   (qp p q)      ; compute q'
	                   (/ count 2)))
	        (else (fib-iter (+ (* b q) (* a q) (* a p))
	                        (+ (* b p) (* a q))
	                        p
	                        q
	                        (- count 1)))))
	(define (pp p q)
		(+ (square q) (square p))
	)
	(define (qp p q)
		(+ (* q (+ q p)) (* q p))
	)
; 1.20
	(define (gcd a b)
	  (if (= b 0)
	      a
	      (gcd b (remainder a b))))
	; (gcd 206 40)
	; normal order
	->(if (= 40 0)
	      206
	      (gcd 40 (remainder 206 40)))
	->(if #f
	      206
	      (gcd 40 (remainder 206 40)))
	->(gcd 40 (remainder 206 40))
	->(if (= (remainder 206 40) 0)
	      40
	      (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
	; Execution of remainder
	->(if (= 6 0)
	      40
	      (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
	->(if #f
	      40
	      (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
	->(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
	->(if (= (remainder 40 (remainder 206 40)) 0)
	      (remainder 206 40)
	      (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
	; Execution of remainder twice
	->(if (= 4 0)
	      (remainder 206 40)
	      (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
	->(if #f
	      (remainder 206 40)
	      (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
	->(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
	->(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
	      (remainder 40 (remainder 206 40))
	      (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
	; Execution of remainder 4 times
	->(if (= 2 0)
	      (remainder 40 (remainder 206 40))
	      (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
	->(if #f
	      (remainder 40 (remainder 206 40))
	      (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
	->(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
	->(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
	      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
	      (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
	->(if (= 2 0)
	      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
	      (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))	
	->(if #f
	      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
	      (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))	
	->(gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
	; Execution of remainder 12 times
	->(if (= 0 0)
	      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
	      (gcd (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) (remainder (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))))
	->(if #t
	      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
	      (gcd (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) (remainder (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))))
	->(remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
	; Execution of remainder 7 times
	->2
	; I count 26 executions of remainder using normal-order evaluation. Others say it is supposed to be 18. I'm sure I fucked up somewhere (right after execution of remiander 4 times).
	; applicative order
	->(if (= 40 0)
	      206
	      (gcd 40 (remainder 206 40)))
	->(if #f
	      206
	      (gcd 40 (remainder 206 40)))
	; Execution of remainder
	->(gcd 40 6)
	->(if (= 6 0)
	      40
	      (gcd 6 (remainder 40 6)))
	->(if #f
	      40
	      (gcd 6 (remainder 40 6)))
	; Execution of remainder
	->(gcd 6 4)
	->(if (= 4 0)
	      6
	      (gcd 4 (remainder 6 4)))
	->(if #f
	      6
	      (gcd 4 (remainder 6 4)))
	; Execution of remainder
	->(gcd 4 2)
	->(if (= 2 0)
	      4
	      (gcd 2 (remainder 4 2)))
	->(if #f
	      4
	      (gcd 2 (remainder 4 2)))
	; Execution of remainder
	->(gcd 2 0)
	->(if (= 0 0)
	      2
	      (gcd 0 (remainder 2 0)))
	->(if #t
	      2
	      (gcd 0 (remainder 2 0)))
	->2
	; I count 4 executions of remainder using applicative-order evaluation
; Notes on computing the exponential of a number modulo another number
	; The books code:
	(define (expmod base exp m)
	  (cond ((= exp 0) 1)
	        ((even? exp)
	         (remainder (square (expmod base (/ exp 2) m))
	                    m))
	        (else
	         (remainder (* base (expmod base (- exp 1) m))
	                    m))
	  )
	) 
	; Why not: 
	(define (expmod base exp m)
		(remainder (exp base exp) m)
	)
	; This is an optimization. Notice that we no longer have
	; to deal w/ numbers bigger than m in the first formulation. In fact, see
	; excercise 1.25 for more details

	; Also notice that recursive procedures are kinda magical because they
	; actually are what they mean. aka...
	; Recursive procedures are declarative. The expression
	; (square (expmod base (/ exp 2) m)) ACTUALLY stands for (b^(n/2) mod c)^2. 
	; This is by virtue of the remainder function operating on expmod. 
	; Recursive procedures are defined entirely by what operations they 
	; "queue" up. aka the call to remainder is what gave expmod the proper 
	; MEANING.
	; Again...recursive procedures DEFINE THEMSELVES. They create their own
	; meaning.
	; If a recursive procedure was a person, it would be a person standing
	; in between 2 mirrors (so that he may see both his front and back) describing
	; himself.
; 1.22
	; needed functions
	(define (divides? a b)
		(= (remainder b a) 0)
	)
	(define (find-divisor n test-divisor)
		(cond ((> (square test-divisor) n) n)
			  ((divides? test-divisor n) test-divisor)
			  (else (find-divisor n (+ test-divisor 1)))
		)
	)
	(define (smallest-divisor n)
		(find-divisor n 2)
	)
	(define (prime? n)
		(= n (smallest-divisor n))
	)
	;
	(define (timed-prime-test n)
		(newline)
		(display n)
		(start-prime-test n (runtime))
	)
	(define (start-prime-test n start-time)
		(if (prime? n)
			(report-prime (- (runtime) start-time))
			#f
		)
	)
	(define (report-prime elapsed-time)
		(display " *** ")
		(display elapsed-time)
		#t
	)
	(define (search-for-primes start)
		(define (iter n counter)
			(if (< counter 3)
				(if (timed-prime-test n)
					(iter (+ n 2) (+ counter 1))
					(iter (+ n 2) counter)
				)
			)
		)
		(if (even? start)
			(iter (+ start 1) 0)
			(iter start 0)
		)
	)
	; primes around 100000000 - 8 zeroes; avg time =  .029999999999999655
	100000007 *** .02999999999999936
	100000037 *** .02999999999999936
	100000039 *** .03000000000000025
	; primes around 1000000000 - 9 zeroes; avg time = .09666666666666668
	1000000007 *** .11000000000000032
	1000000009 *** .08999999999999986
	1000000021 *** .08999999999999986
	; primes around 10000000000 - 10 zeroes; avg time = .28666666666666646
	10000000019 *** .27999999999999936
	10000000033 *** .28000000000000025
	10000000061 *** .2999999999999998
	; primes around 100000000000 - 11 zeroes; avg time = .9499999999999993
	100000000003 *** .9299999999999997
	100000000019 *** .9499999999999993
	100000000057 *** .9699999999999989
	; (sqrt 10) = 3.1622
	; .09666666666666668/.029999999999999655 = 3.2222222222222596
	; .28666666666666646/.09666666666666668 = 2.965517241379308
	; .9499999999999993/.28666666666666646 = 3.313953488372093
	; so yes - the emperical evidence supports our prediction of sqrt(n) growth
	; in time.
; 1.23
	(define (next n)
		(if (= n 2)
			3
			(+ n 2)
		)
	)
	(define (find-divisor n test-divisor)
		(cond ((> (square test-divisor) n) n)
			  ((divides? test-divisor n) test-divisor)
			  (else (find-divisor n (next test-divisor)))
		)
	)
	; primes around 100000000 - 8 zeroes; avg time = .016666666666666313
	; 1.8000000000000176 times faster
	100000007 *** .009999999999999787
	100000037 *** .019999999999999574
	100000039 *** .019999999999999574
	; primes around 1000000000 - 9 zeroes; avg time = .05666666666666723
	; 1.7058823529411595 times faster
	1000000007 *** .05000000000000071
	1000000009 *** .0600000000000005
	1000000021 *** .0600000000000005
	; primes around 10000000000 - 10 zeroes; avg time = .183333333333333
	; 1.5636363636363655 times faster
	10000000019 *** .17999999999999972
	10000000033 *** .17999999999999972
	10000000061 *** .1899999999999995
	; primes around 100000000000 - 11 zeroes; avg time = .5766666666666662
	; 1.647398843930636 times faster
	100000000003 *** .5899999999999999
	100000000019 *** .5800000000000001
	100000000057 *** .5599999999999987
	; I think the reason the ratio is different from 2 is because it does not
	; halve the number of operations required. The test step is only a part
	; of the computation and this optimization halves that only that part.
	; Others explain the discrepancy by pointing out that the next function
	; is going to be more expensive than a primitive operation like + which may
	; have low-level optimizations applied to it.
; 1.25
	; It would NOT serve as well simply because arithmetic with very large numbers
	; is much slower than arithmetic with smaller numbers. arithmetic operations
	; are proportional to the number of bits of the operands
	(define (expmod base exp m)
		(cond ((= exp 0) 1)
		      ((even? exp) 
		      	(remainder (square (expmod base (/ exp 2) m)) m)
		      )
		      (else
		      	(remainder (* base (expmod base (- exp 1) m)) m)
		      )
		)
	)
	(expmod 2 1000 2)
	->(cond ((= 1000 0) 1)
	      ((even? 1000) 
	      	(remainder (square (expmod 2 (/ 1000 2) 2)) 2)
	      )
	      (else
	      	(remainder (* 2 (expmod 2 (- 1000 1) 2)) 2)
	      )
	  )
	->(remainder (square (expmod 2 (/ 1000 2) 2)) 2)
	->(remainder (square (expmod 2 500 2)) 2)
	->(remainder (square (remainder (square (expmod 2 250 2)) 2)) 2)
	->(remainder (square (remainder (square (remainder (square (expmod 2 125 2)) 2)) 2)) 2)
	->(remainder (square (remainder (square (remainder (square (remainder (square (expmod 2 124 2)) 2)) 2)) 2)) 2)
	->(remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (expmod 2 62 2)) 2)) 2)) 2)) 2)) 2)
	->(remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (expmod 2 31 2)) 2)) 2)) 2)) 2)) 2)) 2)
	->(remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (expmod 2 30 2)) 2)) 2)) 2)) 2)) 2)) 2)) 2)
	->(remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (expmod 2 15 2)) 2)) 2)) 2)) 2)) 2)) 2)) 2)) 2)
	->(remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (expmod 2 14 2)) 2)) 2)) 2)) 2)) 2)) 2)) 2)) 2)) 2)
	->(remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (remainder (square (expmod 2 7 2)) 2)) 2)) 2)) 2)) 2)) 2)) 2)) 2)) 2)) 2)
	;...
	; The point is, the difference between the 2 algorithms is when the 
	; "collapsing" begins. Because there is always a remainder operator upon 
	; collapse, the original implementation will never have to deal w/ large
	; numbers. Alyssa's implementation however, would quickly get out of hand.
; 1.26
	; Louis' procedure recursively calls expmod twice. The evolution of the
	; process can be modeled as a binary tree, each node splitting into 2
	; redundant computations. If the original implementation
	; has a time complexity of log(n), then Louis' implemntation would roughly
	; have a time complexity of 2^log(n) = n.
	; The key nuance is in noticing that in (square (expmod base (/ exp 2) m)), the 
	; actual computation involving the primitive operator * is not executed until the 
	; operand, (expmod base (/ exp 2) m) is actually a NUMBER.
	; In effect, we DELAYED the actual computation(execution of an expression) of 
	; (* a a) until a = anActualNumber as opposed to a = aRecursiveCall
	; aka it is because operands must be computed before they can be operated on
	; The important lesson here is that by using a procedure (an abstraction), we
	; actually made the process more EFFICIENT.
; 1.28
	(define (mill-rab-expmod base exp m)
		(cond ((= exp 0) 1)
		      ((even? exp) 
		      	(remainder (square-check-1modn (mill-rab-expmod base (/ exp 2) m) m) m)
		      )
		      (else
		      	(remainder (* base (mill-rab-expmod base (- exp 1) m)) m)
		      )
		)
	)
	(define (square-check-1modn a m)
		(cond ((= a 1) 1)
			  ((= a (- m 1)) 1)
			  ((= (remainder (square a) m) 1) 0)
			  (else (square a))
		)
	)
	; a^(n-1) = 1 mod n AND no such x such that x^2 = 1 mod n (x != 1 AND x != n-1)
	; whilst calculating a^(n-1) we will encounter possible non-trivial square roots of 1 mod n
	; (if n is not prime AND odd, at least half of a < n will be such that while calculating a^(n-1), we find a non-trivial square root of 1 mod n)
	(define (mill-rab-test n)
		(define (try-it a)
			; if prime, this should be true for every a we test
			(= (mill-rab-expmod a (- n 1) n) 1)
		)
		(try-it (+ 1 (random (- n 1))))
	)
	(define (mill-rab-fast-prime? n times)
		(cond ((= times 0) true)
			  ; as soon as this condition is false, the number is not prime
			  ((mill-rab-test n) (mill-rab-fast-prime? n (- times 1)))
			  (else false)
		)
	)
	; use for sanity check
	(define (mill-rab-test-all n)
		(define (try-it a)
			(cond ((= a n) true)
				  ; if prime, this should be true for every a we test
				  ((= (mill-rab-expmod a (- n 1) n) 1) (try-it (+ a 1)))
				  (else false)
			)
		)
		(try-it 1)
	)
; Introduction to higher-order procedures
	; term is the function to be evaluated for each term to be added
	; a is the 
	; next gives us the next a given the current a
	; a
	(define (sum term a next b)
		(if (> a b)
			0
			(+ (term a) (sum term (next a) next b))
		)
	)
	; Integral approximation using abstract sum
	; term = function to be integrated over
	; a and b are the integral bounds
	; a will be seeded with a value of (a + dx/2)
	; next will just add dx to a
	(define (integral1 f a b dx)
		(define (next x) (+ x dx))
		(* (sum f (+ a (/ dx 2)) next b) dx)
	)
	; test function
	(define (cube x) (* x x x))
; 1.29
	(define (integral2 f a b n)
		(define (h a b n) (/ (- b a) n))
		(define (next i)
			(+ i 1)
		)
		(define (4or2? x)
			(if (= (remainder x 2) 0)
				2
				4
			)
		)
		(define (term i)
			(* (4or2? i) (f (+ a (* i (h a b n)))))
		)
		(* (/ (h a b n) 3.) (+ (sum term 1 next (- n 1)) (f a) (f b)))
	)
; 1.30
	(define (sum term a next b)
		(define (iter a result)
			(if (> a b)
				result
				(iter (next a) (+ (term a) result))
			)
		)
		(iter a 0)
	)
	(define (arithmetic-sum n)
		(define (next a) (+ a 1))
		(define (term x) x)
		(sum term 1 next n)
	)
; 1.31
	; a)
	; Lets start with the concrete before we generalize
	(define (product a b)
		(if (> a b)
			1
			(* a (product (+ 1 a) b))
		)
	)
	; Now generalize
	(define (product term a next b)
		(if (> a b)
			1
			(* (term a) (product term (next a) next b))
		)
	)
	; Factorial
	(define (factorial n)
		(define (term x) x)
		(define (next x) (+ x 1))
		(product term 1 next n)
	)
	; Approximate pi/4
	(define (pi/4 n)
		(define (term seed i)
			(if (even? i)
				(+ seed i)
				(term seed (- i 1))
			)
		)
		(define (numterm i)
			(if (= i 0)
				2
				(term 4 (- i 1))
			)
		)
		(define (denterm i)
			(term 3 i)
		)
		(define (fracterm i)
			(/ (numterm i) (denterm i))
		)
		(define (next i) (+ i 1))
		(product fracterm 0. next (- n 1))
	)
	; b) Iterative version of product
	(define (product term a next b)
		(define (iter a result)
			(if (> a b)
				result
				(iter (next a) (* result (term a)))
			)
		)
		(iter a 1)
	)
; 1.32
	; a)
	(define (accumulate combiner null-value term a next b)
		(if (> a b)
			null-value
			(combiner (term a) (accumulate combiner null-value term (next a) next b))
		)
	)
	; sum as accumulate
	(define (sum term a next b)
		(accumulate + 0 term a next b)
	)
	; product as accumulate
	(define (product term a next b)
		(accumulate * 1 term a next b)
	)
	; b) Iterative accumulate
	(define (accumulate combiner null-value term a next b)
		(define (iter a result)
			(if (> a b)
				result
				(iter (next a) (combiner (term a) result))
			)
		)
		(iter a null-value)
	)
; 1.33
	(define (filtered-accumulate include? combiner identity-value term a next b)
		(cond ((> a b) identity-value)
			  ((include? a) (combiner (term a) (filtered-accumulate include? combiner identity-value term (next a) next b)))
			  (else (filtered-accumulate include? combiner identity-value term (next a) next b))
		)
	)
	; Iterative version
	(define (filtered-accumulate include? combiner identity-value term a next b)
		(define (iter a result)
			(cond ((> a b) result)
				  ((include? a) (iter (next a) (combiner (term a) result)))
				  (else (iter (next a) result))
			)
		)
		(iter a identity-value)
	)
	(define (accumulate combiner identity-value term a next b)
		(define (include? x) true)
		(filtered-accumulate include? combiner identity-value term a next b)
	)
	; a) sum of the squares of primes
	(define (sum-of-squares-prime a b)
		(define (next x) (+ x 1))
		(filtered-accumulate prime? + 0 square a next b)
	)
	; b) sum of all positive integers i < n s.t. GCD(i, n) = 1
	(define (product-of-relatively-prime n)
		(define (condition x) (= (gcd x n) 1))
		(define (term x) x)
		(define (next x) (+ x 1))
		(filtered-accumulate condition * 1 term 0 next n)
	)
; 1.34
	(define (f g)
		(g 2)
	)
	(f f)
	-> (f 2)
	-> (2 2)
	-> ERROR ;since 2 is not a function
; Fixed point algorithm
	(define (fixed-point f first-guess)
		(define (close-enough? x1 x2)
			(< (abs (- x1 x2)) 0.00001)
		)
		(define (try guess)
			(let ((next-guess (f guess)))
				(if (close-enough? guess next-guess)
					next-guess
					(try next-guess)
				)
			)
		)
		(try first-guess)
	)
; 1.35
	(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
; 1.36
	(define (fixed-point-logged f first-guess)
		(define (close-enough? x1 x2)
			(< (abs (- x1 x2)) 0.00001)
		)
		(define (try guess)
			(newline)
			(display guess)
			(let ((next-guess (f guess)))
				(if (close-enough? guess next-guess)
					next-guess
					(try next-guess)
				)
			)
		)
		(try first-guess)
	)
	; w/o damping
	(fixed-point-logged (lambda (x) (/ (log 1000) (log x))) 1.1)
	; w/ damping
	(fixed-point-logged (lambda (x) (average (/ (log 1000) (log x)) x)) 1.1)
	; Processes converges much faster w/ damping

; 1.37
	; a)
	(define (cont-frac n d k)
		(if (= k 1)
			0
			(/ (n k) (+ (d k) (cont-frac n d (- k 1))))
		)
	)
	(cont-frac (lambda (i) 1.0)
			   (lambda (i) 1.0)
			   100
	)
	; b) - Iterative version of cont-frac
	(define (cont-frac n d k)
		(define (iter k result)
			(if (= k 1)
				(/ (n 1) result)
				(iter (- k 1) (+ (d (- k 1)) (/ (n k) result)))
			)
		)
		(let ((result (+ (/ (n k) (d k)) (d (- k 1)))))
			 (iter k result))
	)
	; Can I do it using a (modified) fixed-point algorithm?
	; Notice that f must take 2 arguments:
	; 	1 argument for the result of applying the function
	;   1 argument for the iteration number
	(define (applyfn f toseed ktimes)
		(define (iter result ktimes)
			(if (= 0 ktimes)
				(f result ktimes)
				(iter (f result ktimes) (- ktimes 1))
			)
		)
		(iter toseed ktimes)
	)
	(define (cont-frac n d k)
		(define (f x i)
			(+ (d (- i 1)) (/ (n i) x))
		)
		(/ (n 1) (applyfn f (d k) (- k 1)))
	)
	; To make it a recursive process, I would just change the applyfn to be recursive
; 1.38
	(define (approximatee k)
		(define (n i) 1.0)
		(define (d i) 
			(cond ((< i 3) i)
				  ((= (remainder (- i 1) 3) 1) (/ (+ i 1) 3))
				  (else 1)
			)
		)
		(+ (cont-frac n d k) 2)
	)
; 1.39
	(define (tan-cf x k)
		(define (n i) 
			(if (= i 1)
				x
				(- 0 (square x))
			)
		)
		(define (d i)
			(- (* 2 i) 1)
		)
		(cont-frac n d k)
	)
; 1.3.4 - Procedures as Return values - another form of abstraction (the other being using procedures as arguments)
	; Return a function that evaluates x and f(x), then averages them
	(define (average-damp f)
		(lambda (x) (average x (f x)))
	)
	; ((average-damp square) 10) -> 55
	; Reformulate square-root procedure
	(define (sqrt x)
		(fixed-point 
			(average-damp (lambda (y) (/ x y))) 
			1.0
		)
	)
	; Cube root
	(define (cube-root x)
		(fixed-point
			(average-damp (lambda (y) (/ x (square y))))
			1.0
		)
	)
	; Derivative of a function
	(define (D f)
		(define dx 0.00001)
		(lambda (x)
			(/ (- (f (+ x dx)) (f x)) dx)
		)
	)
	; Newton transform - converts g into another function f.
	; Find the fixed point of f will give a solution to g(x) = 0.
	(define (newton-transform g)
		(lambda (x)
			(- x (/ (g x) ((D g) x)))
		)
	)
	; So the general Newton method is:
	(define (newtons-method g guess)
		(fixed-point (newton-transform g) guess)
	)
	; Square root method using newtons-method
	(define (sqrt x)
		(newtons-method (lambda (x) (- (square x) y)) 1.0)
	)
	; Even more general then newtons-method:
	(define (fixed-point-transform g transform guess)
		(fixed-point (transform g) guess)
	)
	; Up to this point we have seen 2 ways to obtain the sqrt:
	; 1) Finding the fixed point of x/y (using damping) AND
	; 2) Newton's method
	; Notice that both of them find a fixed point of some
	; function derived from an original function using a transform
	; Let's recast method 1) using fixed-point-transform
	(define (sqrt x)
		(fixed-point-transform 
			(lambda (y) (/ x y))
			average-damp
			1.0
		)
	)
	; Recasting method 2)
	(define (sqrt y)
		(fixed-point-transform
			(lambda (x) (- (square x) y))
			newton-transform
			1.0
		)
	)
; 1.40
	(define (cubic a b c)
		(lambda (x)
			(+ (cube x) (* a (square x)) (* b x) c)
		)
	)
; 1.41
	(define (double f)
		(lambda (x)
			(f (f x))
		)
	)
	; (((double (double double)) inc) 5) returns what?
	; Hmm...I want to say that ((double (double double)) inc) is a procedure that adds 8
	(double double) -> (lambda (f) (double (double f)))
	(double (double double)) 
	-> ( double (lambda (f) (double (double f))) )
	-> (lambda (f) 
		( (lambda (f) (double (double f))) ((lambda (f) (double (double f))) f) )
	   )
	-> (lambda (f) 
		( (lambda (f) (double (double f))) (double (double f)) )
	   )
	-> (lambda (f) 
		( (lambda (f) (double (double (double (double f))))) )
	   )
	; ((double (double double)) inc)
	-> (double (double (double (double inc))))
	-> (double (double (double add2)))
	-> (double (double add4))
	-> (double add8)
	-> add16
	; So (((double (double double)) inc) 5) returns 21
	; An alternative way of thinking about this is:
	((double (double double)) inc)
	-> ((double (quadruple)) inc)
	-> (quadruple (quadruple inc))
; 1.42
	(define (compose f g)
		(lambda (x)
			(f (g x))
		)
	)
; 1.43
	; Say we want to repeatedly compose a function f 2m( = n) times
	; We can either make a function that is the composition of f m times, g,
	; then compose g w/ itself (double g).
	; (double (repeated f (/ n 2)))
	; OR
	; We can make a function that is the composition of f 2 times, h aka (double f),
	; then compose h m times.
	; (repeated (double f) (/ n 2))
	(define (repeated f n)
		(cond ((= n 1) f)
			  ((even? n)
			  	(double (repeated f (/ n 2))); or (repeated (double f) (/ n 2))
			  )
			  (else
			  	(compose f (repeated f (- n 1)))
			  )
		)
	)
; 1.44
	(define dx 0.0001)
	(define (smooth f)
		(define (avg x y z) (/ (+ x y z) 3))
		(lambda (x)
			(avg (f (- x dx)) (f x) (f (+ x dx)))
		)
	)
	; INCORRECT - ((n-fold-smooth square 2 0.00001) 6.48074) -> 1763.9992396041487
	; but what DOES it do?
	(define (n-fold-smooth f n)
		(repeated (smooth f) n)
	)
	; You are producing a function (repeated (smooth f)) and evaluating it at n; smooth
	; is not the function being composed here, (smooth f) is.
	; ((n-fold-smooth f 2) x)
	-> ((repeated (smooth f) 2) x)
	-> ((double (repeated (smooth f) 1)) x)
	-> ((double (smooth f)) x)
	-> ((double 
		 (lambda (x)
			(avg (f (- x dx)) (f x) (f (+ x dx)))
		 ))
	   x)
	-> (avg 
		(f (- (avg (f (- x dx)) (f x) (f (+ x dx))) dx)) 
		(f (avg (f (- x dx)) (f x) (f (+ x dx)))) 
		(f (+ (avg (f (- x dx)) (f x) (f (+ x dx))) dx)))
	; -----------------------------------------------------------------------------
	; CORRECT - ((n-fold-smooth square 2 0.00001) 6.48074) -> 41.99999094773333
	(define (n-fold-smooth f n)
		((repeated smooth n) f)
	)
	; Smooth is the function being composed here, and f is treated correctly as the
	; parameter.
	; ((n-fold-smooth f 2) x)
	-> (((repeated smooth 2) f) x)
	; replace (repeated smooth 2)
	-> (((double (repeated smooth 1)) f) x)
	; evaluate (repeated smooth 1)
	-> (((double smooth) f) x)
	; evaluate (double smooth)
	-> ((smooth (smooth f)) x)
	; evaluate (smooth f)
	-> ((smooth 
		 (lambda (x)
			(avg (f (- x dx)) (f x) (f (+ x dx)))
		 )
		) x)
	-> ((smooth 
		 (lambda (x)
			(avg (f (- x dx)) (f x) (f (+ x dx)))
		 )
		) x)
	-> ((lambda (x)
			 (avg 
			 	((lambda (x) (avg (f (- x dx)) (f x) (f (+ x dx)))) (- x dx)) 
			 	((lambda (x) (avg (f (- x dx)) (f x) (f (+ x dx)))) x) 
			 	((lambda (x) (avg (f (- x dx)) (f x) (f (+ x dx)))) (+ x dx)))
	   ) x)
	-> (avg 
	 	((lambda (x) (avg (f (- x dx)) (f x) (f (+ x dx)))) (- x dx)) 
	 	((lambda (x) (avg (f (- x dx)) (f x) (f (+ x dx)))) x) 
	 	((lambda (x) (avg (f (- x dx)) (f x) (f (+ x dx)))) (+ x dx)))
; 1.45
	(define (sqrt x)
		(fixed-point-transform 
			(lambda (y) (/ x y))
			average-damp
			1.0
		)
	)
	(define (cubert x)
		(fixed-point-transform 
			(lambda (y) (/ x (expt y 2)))
			average-damp
			1.0
		)
	)
	(define (4rt x)
		(fixed-point-transform 
			(lambda (y) (/ x (expt y 3)))
			(repeated average-damp 2)
			1.0
		)
	)
	(define (5rt x)
		(fixed-point-transform 
			(lambda (y) (/ x (expt y 4)))
			(repeated average-damp 2)
			1.0
		)
	)
	(define (6rt x)
		(fixed-point-transform 
			(lambda (y) (/ x (expt y 5)))
			(repeated average-damp 2)
			1.0
		)
	)
	(define (7rt x)
		(fixed-point-transform 
			(lambda (y) (/ x (expt y 6)))
			(repeated average-damp 2)
			1.0
		)
	)
	(define (8rt x)
		(fixed-point-transform 
			(lambda (y) (/ x (expt y 7)))
			(repeated average-damp 3)
			1.0
		)
	)
	(define (15rt x)
		(fixed-point-transform 
			(lambda (y) (/ x (expt y 14)))
			(repeated average-damp 3)
			1.0
		)
	)
	(define (16rt x)
		(fixed-point-transform 
			(lambda (y) (/ x (expt y 15)))
			(repeated average-damp 4)
			1.0
		)
	)
	(define (31rt x)
		(fixed-point-transform 
			(lambda (y) (/ x (expt y 30)))
			(repeated average-damp 4)
			1.0
		)
	)
	(define (32rt x)
		(fixed-point-transform 
			(lambda (y) (/ x (expt y 31)))
			(repeated average-damp 5)
			1.0
		)
	)
	(define (log2 x)
		(/ (log x) (log 2))
	)
	(define (nthrt n x)
		(fixed-point-transform 
			(lambda (y) (/ x (expt y (- n 1))))
			(repeated average-damp (floor (log2 n)))
			1.0
		)
	)
; 1.46
	(define (iterative-improve good-enough? improve)
		(define (try guess)
			(let ((next (improve guess)))
				(if (good-enough? guess next)
					guess
					(try next)
				)
			)
		)
		(lambda (initial-guess)
			(try initial-guess)
		)
	)
	(define (sqrt x)
		(define (good-enough guess next)
			(< (abs (- (square guess) x)) 0.001) 
		)
		(define (improve guess)
			(average guess (/ x guess))
		)
		((iterative-improve good-enough improve) 1.0)
	)
	(define (fixed-point f first-guess)
		(define (good-enough guess x)
			(< (abs (- guess x)) 0.00001)
		)
		(define (improve guess)
			(f guess)
		)
		((iterative-improve good-enough improve) first-guess)
	)
; END