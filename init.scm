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
      `((lambda ,(map car bindings) . ,body) . ,@(map cadr bindings))))

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

(define (for-each f l)
  (if (null? l)
      '()
      (begin (f (car l))
	     (for-each f (cdr l)))))

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
	    (map f (cdr l)))))