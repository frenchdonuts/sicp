;
    (define new-withdraw
      (let ((balance 100))
        (lambda (amount)
          (if (>= balance amount)
              (begin (set! balance (- balance amount))
                balance)
              "Insufficient funds"))))
    ; Notice that new-withdraw is a VARIABLE bound to the result of evaluating the
    ; let expression. The let expression returns a procedure defined by lambda in 
    ; which the variable balance is bound, initially to 100. The let expression is
    ; EVALUATED ONLY ONCE (when that evaluation - a procedure - is bound to new-withdraw,
    ; a variable).
    ; From mit-scheme docs:
    ; let ((variable init) ...) expression expression ...
    ; The inits are evaluated in the current environment (in some unspecified order), 
    ; the variables are bound to fresh locations holding the results, the expressions 
    ; are evaluated sequentially in the extended environment, and the value of the last 
    ; expression is returned. Each binding of a variable has the expressions as its region.
; 3.1
    ; set! returns the INITIAL value of the variable, which is why we need to evaluate
    ; sum to get its value, else it will look like we are always one set! behind
    (define (make-accumulator sum)
      (lambda (num-to-add)
        (set! sum (+ sum num-to-add))
        sum))
; 3.2
    (define (make-monitored f)
      (let ((count 0))
        (lambda (m)
          (cond ((eq? m 'how-many-calls?) count)
                ((eq? m 'reset-count) (set! count 0))
                (else (set! count (+ count 1))
                      (f m))))))
; 3.3
    (define (make-account balance password)
      (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
      (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
      (define (dispatch password-entry m)
        (cond ((not (eq? password-entry password)) (error "Incorrect password"))
              ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch) 