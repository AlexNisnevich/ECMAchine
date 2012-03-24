ECMAchine
=========

Introduction
------------

ECMAchine is an in-browser Scheme REPL that is also a toy operating system. It has a virtual filesystem that is accessed through Unix-like commands, as well as a rudimentary process management system.

The REPL
-----

ECMAchine supports the expected Scheme arithmetic commands, list-processing commands, and control structures:

```scheme
ecmachine:/ guest$ (+ 1 2 3 4)
10
ecmachine:/ guest$ (- (/ 12 3) 2)
2
ecmachine:/ guest$ (list 1 2 (list 3 4))
(1 2 (3 4))
ecmachine:/ guest$ (car '(1 2 3))
1
ecmachine:/ guest$ (cdr '(1 2 3))
(2 3)
ecmachine:/ guest$ (if (= 5 3) 'a 'b)
b
ecmachine:/ guest$ (cond
..  ((> 5 0) 'positive)
..  ((< 5 0) 'negative)
..  (#t 'zero))
positive
```

Of course there are closures, definitions, and binding constructs:

```scheme
ecmachine:/ guest$ (lambda (arg) (+ arg 1))
(λ (arg) (+ arg 1))
ecmachine:/ guest$ ((lambda (arg) (+ arg 1)) 5)
6
ecmachine:/ guest$ (define (square x) (* x x))
ecmachine:/ guest$ (square 5)
25
ecmachine:/ guest$ (begin 
..  (define x 5) 
..  x)
5
ecmachine:/ guest$ (let ((x 2) (y 3))
..  (* x y))
6
```

And we can do fun things like recursively computing Fibonacci numbers:

```scheme
ecmachine:/ guest$ (define fib
..  (lambda (n)
..    (cond
..      ((or (= n 0) (= n 1)) 1)
..      (#t (+ (fib (- n 1)) (fib (- n 2)))))))
ecmachine:/ guest$ (fib 10)
89
```

Higher-Order Functions
-----

Let's define two functions that we'll be using a lot later: `map` and `reduce`. (In fact, these functions are so important that in ECMAchine, they're both defined in `/startup/mapreduce.lsp` and loaded at startup.)

`map` takes another function and maps it to a list, executing it for every element of the list and combining the results into a new list:

```scheme
ecmachine:/ guest$ (define (map proc items)
..  (if (null? items)
..    nil
..    (cons (proc (car items))
..      (map proc (cdr items)))))
ecmachine:/ guest$ (map abs '(1 -3 5 -6 0))
(1 3 5 6 0)
ecmachine:/ guest$ (map length '(hello lisp))
(5 4)
```

`filter` takes a predicate (that is, a function that returns a boolean value) and filters all the elements of a list that satisfy the predicate:

```scheme
ecmachine:/ guest$ (define (filter pred seq)
..  (cond ((null? seq) nil)
..    ((pred (car seq))
..     (cons (car seq)
..       (filter pred (cdr seq))))
..    (#t (filter pred (cdr seq)))))
ecmachine:/ guest$ (filter (lambda (x) (> x 5)) '(3 7 2 0 9 15))
(7 9 15)
ecmachine:/ guest$ (filter (lambda (x) (= (length x) 3)) '(the blue cat))
(the cat)
```

Introspection
-----

You can view all of currently defined variables in the global environment with the `environment` command. 

```scheme
ecmachine:/ guest$ (environment)
(!= * + - / < <= = > >= __fileSystem abs and append cadr car cd cdr cons cp dir? do-nothing else environment exec fact file? filter help inspect-primitive intersperse js-apply kill kill-all length 
list ls map math mkdir mv new newline nil not null? or overlay path peek perfmon.header perfmon.perfInfo performance processes read rm save size smile sort start sum time)
```

Let's take a look at the absolute value function `abs` in order to see the three levels of functions in ECMAchine:

```scheme
ecmachine:/ guest$ abs
(λ (x) (cond ((> x 0) x) (#t (- x))))
ecmachine:/ guest$ -
#<Function ->
ecmachine:/ guest$ (inspect-primitive -)
function (args) { 
        if (args.length == 1) { 
            return (- args[0]); 
        } else { 
            return (args[0] - args[1]); 
        } 
    }
ecmachine:/ guest$ cond
[USER]: Eval Error: Unbound variable cond
```

When we type in `abs` into the evaluator we get to see the underlying Scheme representation, so we know that `abs` is defined as a Scheme function. (It's actually defined in `/startup/math.lsp` and thus loaded at startup, but more on that later.) We can see that `(abs x)` simply outputs `x` if `x > 0` and `-x` otherwise, but then let's say we wanted to dig a little deeper and figure out how some of the functions used in this definition are themselves implemented. 

When we type `-` into the evaluator we get `#<Function ->`, which means that `-` is a primitive function for ECMAchine. We can then use the `inspect-primitive` function to view the (read-only) underlying JavaScript representation of `-`. Finally, when we type `cond` into the evaluator we get an Unbound variable error, signifying that `cond` is not actually a function at all but a language construct.

On the topic of introspection, we can even examine the contents of running processes with the `peek` command (more on processes later). For instance, here we're examining the source code of a currently-running clock application: 

```scheme
ecmachine:/ guest$ (processes)
((-1 Terminal) (0 clock.app) (1 processmonitor.app) (2 memorymonitor.app))
ecmachine:/ guest$ (peek 0)
(overlay (time (list 'h ': 'm ': 's)) -30 -30 'clock)
```

The File System
-----

Scripts
-----

Processes
-----

Overlays
-----

To Do
-----

- Language 
 - ; comments
 - Primitives for AJAX requests
 - Newlines should be preserved when:
     - writing to files
     - displaying function contents
- Library
 - (accumulate), more higher-order functions?
 - wrappers for the AJAX primitives
- Processes
 - Processes (and scripts) should have their own environments rather than using global environment
 - Allow passing arguments to a process/script
 - Refactor processes into a new class (like Filesystem)
 - More accurate performance measurement (rather than just adding up all evals and dividing by total time)
 - Bonus: come up with a simple time-sharing system that works in JavaScript? 
     - For example, processes could continually adjust their interval in an attempt to reach ~1000 evals/sec systemwide 
     - Could timesharing apply to scripts (one-time operations) in addition to processes (recurring operations)?
- Overlays
 - Allow overlays to be draggable?
 - Figure out a solution to the overlay-getting-in-the-way-of-text problem
- Programs
 - It would be very cool to actually get some more involved programs working: e.g.
     - Text editor
     - Web browser
 - This involves lots of challenges: e.g. how to get user input in a program
- Sessions
	- Use localStorage
	- (restart) command
	- User accounts?
	- Some sort of import/export support?
	 - If I could somehow hack up a server to allow SCP access to "ECMAchine", that would be incredibly cool
