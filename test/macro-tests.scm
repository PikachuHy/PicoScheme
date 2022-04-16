(define-macro (display* expr) 
  (begin
  (display expr)
  (display "=") 
  (display expr)))

(display* (+ 1 3)) ;;; (+ 1 3)=(+ 1 3)
(newline)

(define-macro (display* x)
  `(begin (display ',x) (display "=") (display ,x)))

(display* (+ 1 2)) ;;; (+ 1 2)=3
(newline)

(define-macro (display* x)
  `(list ',x "=" ,@x))

(display  (display* (+ 1 2))) ;;; ((+ 1 2) "=" #<primop>: op_add 1 2)
(newline)