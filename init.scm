(define (apply fun . args)
  (primitive-apply fun args))

(define (map f . l)

  (define (map-car l)
    (if (null? l)
	'()
	(cons (car (car l)) (map-car (cdr l)))))
  (define (map-cdr l)
    (if (null? l)
	'()
	(cons (cdr (car l)) (map-cdr (cdr l)))))

  (define (map-sub f l)
    ; (display l) (newline)
    (if (or (null? l) (null? (car l)))
	'()
	(cons (apply f (map-car l))
	      (map-sub f (map-cdr l)))))
  (map-sub f l))

(define map-in-order map)

(define (for-each f . l)
  (define (for-each-car l)
    (if (null? l)
	'()
	(cons (car (car l)) (for-each-car (cdr l)))))
  (define (for-each-cdr l)
    (if (null? l)
	'()
	(cons (cdr (car l)) (for-each-cdr (cdr l)))))

  (define (for-each-sub f l)
    ; (display l) (newline)
    (if (or (null? l) (null? (car l)))
	'()
	(cons (apply f (for-each-car l))
	      (for-each-sub f (for-each-cdr l)))))
  (if (null? l)
      '()
      (begin (apply f (for-each-car l))
	     (for-each-sub f (for-each-cdr l)))))

(define (call-with-values producer consumer)
  (apply consumer (producer)))

(define-macro  (quasiquote x)

  (define (constant? exp)
    (if (pair? exp)
        (eq? (car exp) 'quote)
        (not (symbol? exp))))

  (define (combine-skeletons left right exp)
    (cond
     ((and (constant? right) (constant? left))
      (if (and (eqv? (eval left)  (car exp))
               (eqv? (eval right) (cdr exp)))
          (list 'quote exp)
          (list 'quote (cons (eval left) (eval right)))))

     ((null? right)
      (list 'list left))

     ((and (pair? right) (eq? (car right) 'list))
      (cons 'list (cons left (cdr right))))

     (else
      (list 'cons left right))))

  (define (expand-quasiquote exp nesting)
    (cond
     ((vector? exp)
      (list 'apply 'vector (expand-quasiquote (vector->list exp) nesting)))

     ((not (pair? exp))
      (if (constant? exp)
          exp (list 'quote exp)))

     ((and (eq? (car exp) 'unquote) (= (length exp) 2))
      (if (= nesting 0)
          (cadr exp)
          (combine-skeletons ''unquote
                             (expand-quasiquote (cdr exp) (- nesting 1))
                             exp)))

     ((and (eq? (car exp) 'quasiquote) (= (length exp) 2))
      (combine-skeletons ''quasiquote
                         (expand-quasiquote (cdr exp) (+ nesting 1))
                         exp))

     ((and (pair? (car exp))
           (eq? (caar exp) 'unquote-splicing)
           (= (length (car exp)) 2))

      (if (= nesting 0)
          (list 'append (cadr (car exp))
                (expand-quasiquote (cdr exp) nesting))
          (combine-skeletons (expand-quasiquote (car exp) (- nesting 1))
                             (expand-quasiquote (cdr exp) nesting)
                             exp)))
     (else
      (combine-skeletons (expand-quasiquote (car exp) nesting)
                         (expand-quasiquote (cdr exp) nesting) exp))))

  (expand-quasiquote x 0))

(define-macro (let bindings . body)
  (define (varval v)
    (string->symbol (string-append (symbol->string v) "=")))

  (define (named-let name bindings body)
    ((lambda (new-bindings)
       `(let ,(cons `(,name #f) new-bindings)
          (set! ,name (lambda ,(map car bindings) . ,body))
          (,name . ,(map car  new-bindings))))
     (map (lambda (b)
            `(,(varval (car b)) ,(cadr b))) bindings)))

  (if (symbol? bindings)
      (named-let bindings (car body) (cdr body))
      `((lambda ,(map car bindings) . ,body)  ,@(map cadr bindings))))

(define-macro  (let* bindings . body)
  (if (null? bindings) `((lambda () . ,body))
      `(let (,(car bindings))
         (let* ,(cdr bindings) . ,body))))

(define-macro (letrec bindings . body)
  (let ((vars (map car  bindings))
        (vals (map cadr bindings)))

    `(let ,(map (lambda (var)
                  `(,var #f)) vars)

       ,@(map (lambda (var val)
                `(set! ,var ,val)) vars vals)
       . ,body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-macro (do bindings test-and-result . body)
  (let ((variables (map car bindings))
        (inits     (map cadr bindings))
        (steps     (map (lambda (clause)
                          (if (null? (cddr clause))
                              (car clause)
                              (caddr clause)))
                        bindings))
        (test   (car test-and-result))
        (result (cdr test-and-result))
        (loop   (gensym)))

    `(letrec ((,loop (lambda ,variables
                       (if ,test
                           ,(if (not (null? result))
                                `(begin . ,result))
                           (begin
                             ,@body
                             (,loop . ,steps))))))
       (,loop . ,inits)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-macro (case expr . cases)
  (let ((item (gensym)))

    (define (do-case case)
      (cond ((not (pair? case)) (error "bad syntax in case" case))
            ((eq? (car case) 'else) case)
            (else `((member ,item ',(car case)) . ,(cdr case)))))

    `(let ((,item ,expr))
       (cond . ,(map do-case cases)))))

(define-macro (or . expr)

  (define (last-expr? expr)
    (null? (cdr expr)))

  (define (expand-or expr)
    (if (last-expr? expr)
	(car expr)
	`(let ((t ,(car expr)))
	  (if t
	      t
	      ,(expand-or (cdr expr))))))
  (if (null? expr)
      #f
      (expand-or expr)))


(define-macro (and . expr)

  (define (last-expr? expr)
    (null? (cdr expr)))

  (define (expand-and expr)
    (if (last-expr? expr)
	(car expr)
	`(let ((t ,(car expr)))
	  (if (not t)
	      t
	      ,(expand-and (cdr expr))))))
  (if (null? expr)
      #t
      (expand-and expr)))


(define (make-promise proc)
  (let ((result-ready? #f)
	(result #f))
    (lambda ()
      (if result-ready?
	  result
	  (let ((x (proc)))
	    (if result-ready?
		result
		(begin (set! result-ready? #t)
		       (set! result x)
		       result)))))))
(define-macro (delay expr)
  `(make-promise (lambda () ,expr)))

(define (force object) (object))

(define (list-length obj)
  (call/cc
   (lambda (return)
     (letrec ((r
	       (lambda (obj)
		 (cond ((null? obj) 0)
		       ((pair? obj)
			(+ (r (cdr obj)) 1))
		       (else (return #f))))))
       (r obj)))))

(define-macro (define-public var . val)
  (define (get-sym var)
    (if (symbol? var)
	var
	(get-sym (car var))))
  (let ((sym (get-sym var)))
    `(begin
       (define ,var ,@val)
       (export ',sym))))

(define-macro (define-public-macro var . val)
  (define (get-sym var)
    (if (symbol? var)
	var
	(get-sym (car var))))
  (let ((sym (get-sym var)))
    `(begin
       (define-macro ,var ,@val)
       (export ',sym))))

(define (noop) #f)

(define (dynamic-wind before thunk after)
  (generate-dynamic-wind before thunk after)
  (let ((t #f))
    (before)
    (set! t (thunk))
    (after)
    (pop-dynamic-wind)
    t))

(define OPEN_READ "r")
(define OPEN_WRITE "w")
(define (open-file filename mode)
  ;; (display "mode:") (display mode) (newline)
  (cond ((eq? mode OPEN_READ)
	 (open-input-file filename))
	((eq? mode OPEN_WRITE)
	 (open-output-file filename))
	(else
	 (display "mode:") (display mode) (newline)
	 (error "not support mode"))))

(define (acons a b c)
  (cons (cons a b) c))
