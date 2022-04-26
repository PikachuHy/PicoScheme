(define (f return)
  (return 2)
  3)

(display (f (lambda (x) x))) ; displays 3
(newline)

(display (call-with-current-continuation f)) ; displays 2
(newline)

(define c #f)
(display (call-with-current-continuation
                    (lambda (c0)
                      (set! c c0)
                      'talk1)))
(c 'talk2)
(newline)

;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end)
(define (generate-one-element-at-a-time lst)
  ;; Both internal functions are closures over lst

  ;; Internal variable/Function which passes the current element in a list
  ;; to its return argument (which is a continuation), or passes an end-of-list marker
  ;; if no more elements are left. On each step the function name is
  ;; rebound to a continuation which points back into the function body,
  ;; while return is rebound to whatever continuation the caller specifies.
  (define (control-state return)
    (for-each
     (lambda (element)
               (set! return (call-with-current-continuation
                              (lambda (resume-here)
                                ;; Grab the current continuation
                               (set! control-state resume-here)
                               (return element))))) ;; (return element) evaluates to next return
     lst)
    (return 'you-fell-off-the-end))

  ;; (-> X u 'you-fell-off-the-end)
  ;; This is the actual generator, producing one item from a-list at a time.
  (define (generator)
    (call-with-current-continuation control-state))

  ;; Return the generator
  generator)

(define generate-digit
  (generate-one-element-at-a-time '(0 1 2)))

(display (generate-digit)) ;; 0
(display (generate-digit)) ;; 1
(display (generate-digit)) ;; 2
(display (generate-digit)) ;; you-fell-off-the-end

; 
; First time through
; my-val is a continuation object
; Second time through
; my-val is 5
(let ((my-val
       (call/cc (lambda (c) c))))
  (if (procedure? my-val)
      (begin
	(display "First time through\n")
	(display "my-val is a continuation object\n")
	(my-val 5))
      (begin
	(display "Second time through\n")
	(display "my-val is ")
	(display my-val)
	(newline))))

; display -3
(call/cc
 (lambda (exit)
   (for-each (lambda (x)
	       (if (negative? x) (exit x)))
	     '(54 0 37 -3 245 19))
   #t))
