# call/cc implement

[Continuations: The Swiss Army Knife of Flow Control](https://www.youtube.com/watch?v=Ju3KKu_mthg&ab_channel=BPLearning)

## History

### exception with self args

1. define exception

```c++
struct continuation_exception {
    continuation_exception(Scheme& scm, const std::vector<Cell>& args) {
        if (args.empty())
            return;
        continuation = args.size() != 1 ? primop::list(scm, args) : args[0];
    }
    Cell continuation = none;
};
```

2. construct lambda

```c++
auto lambda = [](Scheme& scm, const SymenvPtr&, const std::vector<Cell>& args) -> Cell {
    throw continuation_exception{ scm, args };
};
```

3. apply f c

```c++
try {
    // call (f c)
    auto f = args[0];
    auto c = scm.function(senv, std::move(lambda))
    return pscm::apply(scm, senv, f, c);
} catch (const continuation_exception& e) {
    return e.continuation;
}
```

This implementation works with the following code:

```scheme
;;; (call/cc f) return a procedure
(test #t (call-with-current-continuation procedure?))

;;; k has no work
(test 7 (call-with-current-continuation (lambda (k) (+ 2 5))))

;;; throw exception with arg 3
(test 3 (call-with-current-continuation (lambda (k) (+ 2 5 (k 3)))))
```

```scheme
(define (f return)
  (return 2)
  3)

(display (f (lambda (x) x))) ; displays 3

(display (call-with-current-continuation f)) ; displays 2
```

However, when we save the continuation to a variable,
we cannot call the continuation out of the procedure `f` scope.

For example,

```scheme
(define c #f)
(display
  (string-append "a" "c"
    (call/cc (lambda (return)
			   (begin
			     (set! c return)
			     "hhh"
			     )))
	"\n"))

(c "555")
(c "666")
(c "777")
```

run result

```shell
libc++abi: terminating with uncaught exception of type pscm::continuation_exception
```

if we want to call more times,
we can disable the exception and just return args.

```c++
auto lambda = [](Scheme& scm, const SymenvPtr&, const std::vector<Cell>& args) -> Cell {
    return args[0];
};
```

However, the output is wrong. Only `achhh` are printed.
That means the continuation we saved is not right.

What's more. `(display (call/cc (lambda (return) (return 2) 3)))` not work.

In a nutshell, the implementation just works as `return`.

### change code in lambda

To find a right continuation, we can change the code in lambda.
For example, the proper continuation can write as following.

```scheme
(lambda (x)
  (display 
    (string-append "a" "c" x "\n")))
```

We do this in C++.

```c++
std::optional<Cell> change_code(Cell code, const Cell& cc) {
    if (!is_pair(code)) {
        return std::nullopt;
    }
    while (is_pair(code)) {
        if (car(code) == cc) {
            return code;
        }
        auto tmp = car(code);
        auto val = change_code(tmp, cc);
        if (val.has_value()) {
            return val;
        }
        code = cdr(code);
    }
    return std::nullopt;
}
// get code from context
auto code = ...;
/* cc is (call/cc (lambda (return)
			   (begin
			     (set! c return)
			     "hhh"
			     )))

*/
auto new_code = change_code(code, cc);
// construct new_proc with new_code
auto new_proc = ...;
```

Then the lambda change to

```c++
auto lambda = [](Scheme& scm, const SymenvPtr&, const std::vector<Cell>& args) -> Cell {
    auto ret = apply(env, new_proc, cons(args[0], nil));
    // to call multi times
    // throw continuation_exception{ scm, {args} };
    return ret;
};
```

now, we get right output.

```shell
achhh
ac555
ac666
ac777
```

This implementation has two major drawbacks.
On the one hand, no exception means no `return` feature.
On the other hand, `change_code` is just replacement.
It is not a real continuation.

### stack frames

To solve the problem above, we develop a stack-based call/cc implementation. For each procedure call, we use a frame to
record the expression. For example,

```scheme
(display 
 (call/cc 
  (lambda (return) 
    (return 2) 
    3)))
```

we save the operator `display` and operand `(call/cc (lambda (return) (return 2) 3))` to frame.

The `frame` is defined as follow.

```c++
class Frame {
public:
    Frame(SymenvPtr env, Cell cell) : m_env(env), m_expr(cell) {}
private:
    SymenvPtr m_env;
    Cell m_expr;
    std::vector<Cell> m_args_stack;
};
```

The class member `m_expr` holds the full expression `(display (call/cc (lambda (return) (return 2) 3)))`. Our scheme
interpreter holds a frame stack.

```c++
using FrameStack  = std::vector<Frame>;
```

When eval `call/cc`, we create a `Continuation` object.

```c++
class Continuation {
public:
    explicit Continuation(FrameStack frames) : m_frames(std::move(frames)) {
        // the last frame is call/cc
        m_frames.pop_back();
    };
private:
    FrameStack m_frames;
};

// using ContPtr     = std::shared_ptr<Continuation>;
auto cont = std::make_shared<ContPtr::element_type>(m_frames);
auto f = eval(senv, cadr(cell));
Cell expr = list(f, cont);
return eval(senv, expr);
```

When eval `Continuation` object, we also need to throw exception.

```c++
auto cont = get<ContPtr>(op);
auto cont_args = eval(env, cdr(expr));
// continuation_exception cannot be catched. why?
throw Cell(cons(cont, cons(Intern::_quote, cons(cont_args, nil))));
```

With two reasons,

- As discuss above, if no exeception, we cannot get `return` feature.
- restore frames when the continuation object is assigned to a variable and called.

The exception is handled in a special function called `eval_with_continuation`.

```c++
ContPtr c;
Cell c_args;
bool need_restore = false;
while (true) {
    try {
        if (need_restore) {
            return restore_from_continuation(c, c_args);
            need_restore = false;
        }
        else {
            return eval(env, expr);
        }
    }
    catch (const Cell& cell) {
        if (is_cont(car(cell))) {
            c = get<ContPtr>(car(cell));
            c_args = cdr(cell);
            need_restore = true;
            continue;
        }
        throw std::runtime_error("unsupported op");
    }
}
```

When restore frames from continuation, all frames must be evaled.

```c++
// cont is c
// and args is c_args
m_frames = cont->frames();
auto env = m_frames.back().env();
Cell return_arg = eval(env, args);
while (!m_frames.empty()) {
    m_frames.back().push_arg(return_arg);
    return_arg = eval_frame_based_on_stack();
    pop_frame();
}
return return_arg;
```

If no continuation object called, no change compared to prior.

This implementaion works with all test cases discussed above.

Seem to prefect, isn't it?

However, the FrameStack snapshot is created when `call/cc` is evaluating, which cause something unexcepted.

For example,

```scheme
(test '(connect talk1 disconnect connect talk2 disconnect)
    (let ((path '())
          (c #f))
      (let ((add (lambda (s)
                   (set! path (cons s path)))))
        (dynamic-wind
            (lambda () (add 'connect))
            (lambda ()
              (add (call-with-current-continuation
                    (lambda (c0)
                      (set! c c0)
                      'talk1))))
            (lambda () (add 'disconnect)))
        (if (< (length path) 4)
            (c 'talk2)
            (reverse path)))))
```

run result

```shell
argument () is not a #<symbol>

trace:
(begin (set! ...) (let ((str ...) (res ...))...))
(begin ((lambda (str res) ...)))
((lambda (str res) ...))
(begin ((lambda (path c) (let ((add ...)) ...))))
((lambda (path c) (let ((add ...)) ...)))
(begin ((lambda (add) (dynamic-wind ...))))
((lambda (add) (dynamic-wind ...)))
```

Ideally, the continuation frames are

```scheme
; (ignore macro expand)
(test ...)
(let ...)
(let ...)
(dynamic-wind ...)
```

However, when inspect the continuation, the frames like that

```scheme
; (ignore macro expand)
(test ...)
(let ...)
(let ...)
(dynamic-wind ...)
((lambda ...))
(add ...)
```

Why?

In this implementation, continuation is frames snapshot when  `call/cc` is evaluating.

`(lambda () (add ...))` returns a procedure, while the procedure code is not evaluated until `dynamic-wind` called.

How to solve it?



