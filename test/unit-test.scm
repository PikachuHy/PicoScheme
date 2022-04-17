
(define *tests-run* 0)
(define *tests-passed* 0)

(define-syntax test
  (syntax-rules ()
    ((test name expect expr)
     (test expect expr))
    ((test expect expr)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (let ((str (call-with-output-string
                    (lambda (out)
                      (write *tests-run*)
                      (display ". ")
                      (display 'expr out))))
             (res expr))
         (display str)
         (write-char #\space)
         (display (make-string (max 0 (- 72 (string-length str))) #\.))
         (flush-output)
         (cond
          ((equal? res expect)
           (set! *tests-passed* (+ *tests-passed* 1))
           (display " [PASS]\n"))
          (else
           (display " [FAIL]\n")
           (display "    expected ") (write expect)
           (display " but got ") (write res) (newline))))))))

(define-syntax test-assert
  (syntax-rules ()
    ((test-assert expr) (test #t expr))))

(define (test-begin . name)
  #f)

(define (test-end)
  (write *tests-passed*)
  (display " out of ")
  (write *tests-run*)
  (display " passed (")
  (write (* (/ *tests-passed* *tests-run*) 100))
  (display "%)")
  (newline))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-end)
