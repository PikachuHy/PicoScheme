(module (unit-test))
(use-module (test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "unit test")

(test '(4) (list-tail '(1 2 3 4) 3))
(test '(3 4) (list-tail '(1 2 3 4) 2))
(test '(2 3 4) (list-tail '(1 2 3 4) 1))
(test '(1 2 3 4) (list-tail '(1 2 3 4) 0))

(test '(1 2 3 4) (list-head '(1 2 3 4) 4))
(test '(1 2 3) (list-head '(1 2 3 4) 3))
(test '(1 2) (list-head '(1 2 3 4) 2))
(test '(1) (list-head '(1 2 3 4) 1))
(test '() (list-head '(1 2 3 4) 0))

(test '(a `(b `(c ,@(+1 1 ,@(+ 2 2 (3 3)))))) `(a `(b `(c ,@(+1 1 ,@(+ 2 2 ,(list 3 3)))))))
(test '(a `(b `(c ,@(+1 1 ,@(+ 2 2 3 3))))) `(a `(b `(c ,@(+1 1 ,@(+ 2 2 ,@(list 3 3)))))))

(test 3 (let* ((table (make-hash-table))
	       (1+ (lambda (x) (+ 1 x))))
	  (begin
	    (hash-table-set! table "a" 1)
	    (hash-table-set! table "b" 2)
	    (hash-table-set! table "c" 3)
	    (hash-table-set! table 1 5)
	    (hash-table-fold (lambda (key value prior)
			       (if (string? key) (1+ prior) prior))
			     0 table))))

(test 4 ((lambda (x) (+ x x) 4) 20))

(test ':synopsis (begin
(define ((define-property which) opt decl)
  which
)
((define-property ':synopsis) 'a 'b)
))

(test 5 (call-with-values (lambda () (values 4 5))
        (lambda (a b) b)))

(test -1 (call-with-values * -))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-end)
