(module (test))
(define *tests-run* 0)
(define *tests-passed* 0)
(define primitive-add +)
(define-syntax test
  (syntax-rules ()
    ((test name expect expr)
     (test expect expr))
    ((test expect expr)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (let ((str (call-with-output-string
                    (lambda (out)
                      (display *tests-run* out)
                      (display ". " out)
                      (display 'expr out))))
             (res expr))
         (display str)
         (write-char #\space)
         (display (make-string (max 0 (- 150 (string-length str))) #\.))
         (flush-output)
         (cond
          ((equal? res expect)
           (set! *tests-passed* (primitive-add *tests-passed* 1))
           (display-green " [PASS]\n")
           #t)
          (else
           (display-red " [FAIL]\n")
           (display "    expected ") (write expect)
           (display " but got ") (write res) (newline)
           #f)
           ))))))

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
