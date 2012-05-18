ECMAchine
=========

ECMAchine is an in-browser Scheme REPL that is also a toy operating system. It has a virtual filesystem that is accessed through Unix-like commands, as well as a rudimentary process management system.

**Why?** I made ECMAchine for a few reasons. For one, it's an interesting experiment in what a filesystem based around S-expressions could look like. It was also a good exercise in writing an interpreter and finally applying all those things I learned in SICP. Most importantly, though, ECMAchine is simply a lot of fun. I've spent countless hours playing around with it and coming up with little programs for it, and hopefully by the end of this tutorial you, the reader, will be able to get a sense for how fun it is to work with ECMAchine.

This tutorial will walk you through the file and process management features of ECMAchine, and then show a bunch of examples of cool things that you can do with them. It is aimed at people with at least a little bit of experience with Scheme or another Lisp dialect. 

Table of Contents
-----

1\.  [The REPL](#therepl)  
2\.  [Introspection](#introspection)  
3\.  [The File System](#thefilesystem)  
3.1\.  [Folders](#folders)  
3.2\.  [Files](#files)  
3.3\.  [Other File System Commands](#otherfilesystemcommands)  
4\.  [Review: Higher-Order Functions](#review:higher-orderfunctions)  
4.1\.  [Map](#map)  
4.2\.  [Filter](#filter)  
5\.  [A Few More Functions](#afewmorefunctions)  
5.1\.  [js-apply](#js-apply)  
5.2\.  [length](#length)  
5.3\.  [sort](#sort)  
5.4\.  [time](#time)  
5.5\.  [newline](#newline)  
6\.  [Scripts](#scripts)  
6.1\.  [Special Types of Scripts](#specialtypesofscripts)  
7\.  [Processes](#processes)  
7.1\.  [Overlays](#overlays)  
7.2\.  [Example: A Simple Clock](#example:asimpleclock)  
7.3\.  [Process Management](#processmanagement)  
7.4\.  [Process Performance](#processperformance)  
8\.  [Recipes](#recipes)  
8.1\.  [File System Recipes](#filesystemrecipes)  
8.1.1\.  [Directory Cleanup](#directorycleanup)  
8.1.2\.  [File/Folder Size](#file/foldersize)  
8.1.3\.  [File Search](#filesearch)  
8.2\.  [Process Manipulation Recipes](#processmanipulationrecipes)  
8.2.1\.  [Process Cleanup](#processcleanup)  
8.2.2\.  [A Simple Task Manager](#asimpletaskmanager)  
8.3\.  [Miscellaneous Recipes](#miscellaneousrecipes)  
8.3.1\.  [Analog Clock](#analogclock)  
8.3.2\.  [Other Ideas](#otherideas)  
9\.  [ed, a Simple Text Editor](#edasimpletexteditor)  
9.1\.  [Introducing ed](#introducinged)  
9.2\.  [Design Overview](#designoverview)  
9.3\.  [Helper Functions](#helperfunctions)  
9.4\.  [The Skeleton of ed](#theskeletonofed)  
9.5\.  [Displaying the Editor Contents](#displayingtheeditorcontents)  
9.6\.  [File Commands](#filecommands)  
9.7\.  [Edit Commands](#editcommands)  
9.8\.  [Cool Things](#coolthings)  
9.8.1\.  [Map](#map)  
9.8.2\.  [Run](#run)  
9.8.3\.  [And more](#andmore)  
9.9\.  [Putting It All Together](#puttingitalltogether)  
10\.  [What's Next?](#what'snext?)  
11\.  [Acknowledgements](#acknowledgements)  

<a name="therepl"></a>

1\. The REPL
-----

ECMAchine supports the expected Scheme arithmetic commands, list-processing commands, and control structures:

```
ecmachine:/ guest$ (+ 1 2 3 4)
10
ecmachine:/ guest$ (- (/ 12 3) 2)
2
ecmachine:/ guest$ (cons 'a '(b c))
(a b c)
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

```
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

```
ecmachine:/ guest$ (define fib
..  (lambda (n)
..    (cond
..      ((or (= n 0) (= n 1)) 1)
..      (#t (+ (fib (- n 1)) (fib (- n 2)))))))
ecmachine:/ guest$ (fib 10)
89
```

But we already know about Lisp. What makes ECMAchine different?

<a name="introspection"></a>

2\. Introspection
-----

You can view all of currently defined variables in the global environment with the `environment` command. 

```
ecmachine:/ guest$ (environment)
(!= * + - / < <= = > >= __fileSystem abs and append cadr car cd cdr cons cp dir? do-nothing else environment exec fact file? filter help inspect-primitive intersperse js-apply kill kill-all length 
list ls map math mkdir mv new newline nil not null? or overlay path peek perfmon.header perfmon.perfInfo performance processes read rm save size smile sort start sum time)
```

Let's take a look at the absolute value function `abs` in order to see the three levels of functions in ECMAchine:

```
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

```
ecmachine:/ guest$ (processes)
((-1 Terminal) (0 clock.app) (1 processmonitor.app) (2 memorymonitor.app))
ecmachine:/ guest$ (peek 0)
(overlay (time (list 'h ': 'm ': 's)) -30 -30 'clock)
```

<a name="thefilesystem"></a>

3\. The File System
-----

<a name="folders"></a>

### 3.1\. Folders

The file system is navigated using the `cd` and `ls` functions. The preferred method for concatenating file/directory names into paths is with the `path` function, as shown below:

```
ecmachine:/ guest$ (ls)
(apps cleanup.s readme.txt startup usr)
ecmachine:/ guest$ (ls 'apps)
(clock.app memorymonitor.app processmonitor.app unixclock.app virushunter.app)
ecmachine:/ guest$ (cd 'usr)
ecmachine:/usr guest$ (ls)
()
ecmachine:/usr guest$ (mkdir 'usr2)
(Directory /usr/usr2 created)
ecmachine:/usr guest$ (ls)
(usr2)
ecmachine:/usr guest$ (cd '..)
ecmachine:/ guest$ (path 'usr 'usr2)
usr/usr2
ecmachine:/ guest$ (cd (path 'usr 'usr2))
ecmachine:/usr/usr2 guest$ 
```

<a name="files"></a>

### 3.2\. Files

The `read` function is used to read the contents of a file, while `save` and `append` create a new file and append data to an existing file, respectively.

```
ecmachine:/ guest$ (ls)
(apps cleanup.s readme.txt startup usr)
ecmachine:/ guest$ (read 'readme.txt)
ECMAchine by Alex Nisnevich. Thanks to Jakub Jankiewicz for his jQuery Terminal plugin (distributed under LGPL).For more information check out the git repo at https://github.com/AlexNisnevich/ECM
Achine
ecmachine:/ guest$ (save 'blah.txt '(I am using ECMachine!))
(Saved file /blah.txt)
ecmachine:/ guest$ (read 'blah.txt)
(I am using ECMachine!)
ecmachine:/ guest$ (append 'blah.txt '(Hooray!))
(Updated file /blah.txt)
ecmachine:/ guest$ (read 'blah.txt)
(I am using ECMachine!)
(Hooray!)
```

<a name="otherfilesystemcommands"></a>

### 3.3\. Other File System Commands

You can move, copy, and delete files and directories with the `mv`, `cp`, and `rm` commands, respectively.

The predicates `file?` and `dir?` can be used to find if a path points to a file, directory, or neither.

<a name="review:higher-orderfunctions"></a>

4\. Review: Higher-Order Functions
-----

Before we continue into scripts and processes, let's define two functions that we'll be using a lot later: `map` and `filter`. (In fact, these functions are so important that in ECMAchine, they're both defined in `/startup/mapreduce.lsp` and loaded at startup.)

<a name="map"></a>

### 4.1\. Map

`map` takes another function and maps it to a list, executing it for every element of the list and combining the results into a new list:

```
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

<a name="filter"></a>

### 4.2\. Filter

`filter` takes a predicate (that is, a function that returns a boolean value) and filters all the elements of a list that satisfy the predicate:

```
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

<a name="afewmorefunctions"></a>

5\. A Few More Functions
-----

We're almost at the fun part of the tutorial, but before we get there, I should briefly mention a few more important functions that are included as primitives in ECMAchine.

<a name="js-apply"></a>

### 5.1\. js-apply

```(js-apply func [obj] args)``` is one of the most powerful primitives in ECMAchine, because it allows you to use almost any function in JavaScript's standard library. In its two-argument form, it evaluates ```func(args)``` in JavaScript - in this case, ```func``` is usually a the name of a Javascript library method like ```Math.sin```. In its three-argument form, it evaluates ```obj.func(args)```, where ```obj``` is generally a string or a list.

Some examples:

```
ecmachine:/ guest$ (js-apply 'alert '(Hello!))
{displays a dialog}
ecmachine:/ guest$ (js-apply 'replace 'the-quick-brown-fox '(fox lisp))
the-quick-brown-lisp
ecmachine:/ guest$ (js-apply 'split '/usr/usr2/file '/)
( usr usr2 file)
ecmachine:/ guest$ (js-apply 'Math.sqrt 100)
10
```

That last use case is so common that we can simplify it a little with a helper function:

```lisp
(define (math func args)
	 (js-apply (+ 'Math. func) args))
```

Now we can call JavaScript mathematical commands like this:

```
ecmachine:/ guest$ (math 'sin 1)
0.8414709848078965
```

<a name="length"></a>

### 5.2\. length

```
ecmachine:/ guest$ (inspect-primitive length)
function (args) { 
        return args[0].length; 
    }
ecmachine:/ guest$ (length 'thisIsAString)
13
ecmachine:/ guest$ (length '(this is a list))
4
ecmachine:/ guest$ (length (read 'readme.txt))
201
```

```(length obj)``` returns the length of an array or string, using JavaScript's `length` property. One common use for it is to retrieve the length of a file.

<a name="sort"></a>

### 5.3\. sort

```
ecmachine:/ guest$ (inspect-primitive sort)
function (args) {
        // This doesn't NEED to be a primitive, but it's a pain to implement,
        // and I'd rather use JavaScript's underlying sort
        // Usage: (sort lst [keyfunc])
        return args[0].sort(function (a, b) {
            var keyA = (args.length > 1) ? lispApply(args[1], [a]) : a;
            var keyB = (args.length > 1) ? lispApply(args[1], [b]) : b;
            if (keyA < keyB)
                return -1;
            if (keyA > keyB)
                return 1;
            return 0;
        });
    }
ecmachine:/ guest$ (sort '(5 3 8 2 4 1))
(1 2 3 4 5 8)
ecmachine:/ guest$ (sort '(the quick brown fox))
(brown fox quick the)
ecmachine:/ guest$ (sort '(5 3 8 2 4 1) (lambda (x) (- x)))
(8 5 4 3 2 1)
ecmachine:/ guest$ (sort '((5 apples) (3 bananas) (8 oranges) (2 pears) (4 peaches) (1 watermelon)) (lambda (x) (car x)))
((1 watermelon) (2 pears) (3 bananas) (4 peaches) (5 apples) (8 oranges))
```

```(sort lst [keyfunc])``` sorts a list in ascending order. If a second parameter is specified, it is a key function that is applied to the list's elements before sorting.

<a name="time"></a>

### 5.4\. time

```
ecmachine:/ guest$ (inspect-primitive time)
function (args) {
        var date = new Date();
        if (args[0] == null) {
            return date.getTime();            
        } else {
            return args[0].map(function (str) {
                switch (str) {
                    case 'h':
                        return date.getHours();
                    case 'm':
                        return date.getMinutes();
                    case 's':
                        return date.getSeconds();
                    default:
                        return str;
                }
            });
        }
    }
ecmachine:/ guest$ (time)
1332744204185
ecmachine:/ guest$ (time '(h : m : s))
(23 : 43 : 41)
```

```(time)``` returns the current time, either in the default JavaScript time format (that is, Unix time in milliseconds), or formatted with an optional format string. We will use it later to build clocks.

<a name="newline"></a>

### 5.5\. newline

```(newline)``` inserts a newline character. One use of it is in the Process Manager application described later, where it's interspersed within a list to put each list element in a new line.

<a name="scripts"></a>

6\. Scripts
-----

A **script** is an executable Lisp file. Scrips are created like any other file, and by convention end in the file extension `.s`. The `exec` command is used to execute scripts.

For example, `/cleanup.s` is a script that cleans the contents of the `usr` directory. Let's take a look at it and then run it.

```
ecmachine:/ guest$ (read 'cleanup.s)
(rm (path '/ 'usr))
(mkdir (path '/ 'usr))
ecmachine:/ guest$ (ls 'usr)
()
ecmachine:/ guest$ (save (path 'usr 'myFile) 'blahblahblah)
(Saved file /usr/myFile)
ecmachine:/ guest$ (ls 'usr)
(myFile)
ecmachine:/ guest$ (exec 'cleanup.s)
(Directory /usr created)
ecmachine:/ guest$ (ls 'usr)
()
```

Fairly straight-forward: `/cleanup.s` removes the `/usr` directory, clearing its contents, and then recreates the directory.

_As an aside, could we delete the contents of a directory without deleting the entire directory? We could, but it would be a little more complicated:_

```lisp
(map rm (map (lambda (x) (path 'usr x)) (ls 'usr)))
```

<a name="specialtypesofscripts"></a>

### 6.1\. Special Types of Scripts

There are two more important types of scripts, that have their own extensions by convention.

- **Libraries** (by convention ending in the extension `.lsp`) consist only of function definitions - for example, `map` and `filter` are defined in the library file `/startup/mapreduce.lsp`. 
- **Shortcuts** (by convention ending in the extension `.lnk`) consist only of `exec` or `start` function calls to other scripts or applications (more on those later).

Why would we need shortcuts? One big reason has to do with the `/startup` directory: when ECMAsystem launches, every script located within the `/startup` directory is executed. Generally, the startup directory is expected to consist of only libraries and shortcuts.

Now, scripts are cool, but wouldn't it be cooler if we could somehow run a script continuously in the background? This is where processes come in.

<a name="processes"></a>

7\. Processes
-----

A **process** is created when a script is called with the `start` function. `(start path interval)` begins running the script at _path_ with a refresh rate of _interval_ milliseconds. By convention, scripts that are meant to be run as processes are called **applications** and end in the file extension `.app`.

<a name="overlays"></a>

### 7.1\. Overlays

Since processes run in the background, the terminal generally isn't the best place for them to send their output - ideally, a process would be able to output directly to some part of the screen. Fortunately, **overlays** exist for just this purpose.

An overlay is created with the `overlay` command as follows:

```lisp
(overlay '(text to display) x-pos y-pos 'nameOfOverlay)
```

The overlay is placed at position `(x-pos, y-pos)`, where `x-pos` counts pixels from the left if positive and from the right if negative and likewise `y-pos` counts pixels from the top if positive and from the bottom if negative. For example, if `(x-pos,y-pos) = (-30, -30)`, the overlay would be placed . In all of my examples, `x-pos` is always negative, and I don't really see much use for left-aligned overlays in general because they would get in the way of the terminal.

Only one overlay with a given name can exist at any time - creating a new overlay with the same name as an existing one has the effect of replacing the old overlay with the new one. This means that if a processs continually calls the `overlay` function with the same name parameter, the overlay is continually updated.

Let's use what we've learned about processes and overlays to make a clock application.

<a name="example:asimpleclock"></a>

### 7.2\. Example: A Simple Clock

All it takes to make a clock is:

```lisp
(overlay (time (list 'h ': 'm ': 's)) -30 -30 'clock)
```

This gets the current time (in `HH:MM:SS` format) and displays it in the lower-right corner, 30 pixels from each side. If we save this as `/apps/clock.app` we can run it as a process like this:

```
ecmachine:/ guest$ (start (path 'apps 'clock.app) 1000)
(Starting process at apps/clock.app with PID 4)
```

<a name="processmanagement"></a>

### 7.3\. Process Management

Now that we've know how to start processes, let's see what we can do with running processes.

```
ecmachine:/ guest$ (processes)
((-1 Terminal) (4 clock.app))
ecmachine:/ guest$ (peek 4)
(overlay (time (list 'h ': 'm ': 's)) -30 -30 'clock)
ecmachine:/ guest$ (kill 4)
(Process with PID 4 [clock.app] terminated)
```

There are three commands for dealing with processes, that are illustrated above:

- `(processes)` returns a list of running processes, with each process represented as a `(pid name)` pair
- `(peek pid)` shows the application code for the given process
- `(kill pid)` kills the given process and hides all overlays that were used by it

Note that the terminal itself is always represented as process -1, but cannot be killed. As we will see later, this means that something like `(map kill (map car (processes)))` won't work, while `(map kill (filter (λ (x) (> x -1)) (map car (processes))))` will.

<a name="processperformance"></a>

### 7.4\. Process Performance

ECMAchine keeps track of the performance of every process (including the terminal) with a metric called _evals per second_. Every time a call to `lispEval` is made by a process, its eval count is incremented, and the evals/sec of a process is simply the amount of evals made divided by the number of seconds it has been running.

The `(performance pid)` function returns the evals/sec of the given process:

```
ecmachine:/ guest$ (start (path 'apps 'clock.app) 1000)
(Starting process at apps/clock.app with PID 4)
ecmachine:/ guest$ (processes)
((-1 Terminal) (4 clock.app))
ecmachine:/ guest$ (performance 4)
15.22
```

Of course, the performance of a process depends both on the application and on the refresh interval. If we set the clock to update every millisecond, we get:

```
ecmachine:/ guest$ (start (path 'apps 'clock.app) 1)
(Starting process at apps/clock.app with PID 5)
ecmachine:/ guest$ (performance 5)
3196.686
```

We will use the `performance` function later to write a simple task manager application.

<a name="recipes"></a>

8\. Recipes
-----

So, now that we have all of these tools, what can we do with them? Here are some functions and processes that I've come up with. Many of them are included in ECMAchine as scripts, processes, or library functions.

<a name="filesystemrecipes"></a>

### 8.1\. File System Recipes

<a name="directorycleanup"></a>

#### 8.1.1\. Directory Cleanup

Let's start with file and folder manipulation. Here's one that was already mentioned above: a function to delete all of the elements of a directory. 

```lisp
(define (clean dir)
	(map rm 
	     (map (lambda (x) (path dir x)) 
	          (ls dir))))
```

Why is the `(lambda (x) (path dir x))` mapping necessary? Let's say that we want to delete the contents of subfolder `/usr` and `(ls 'usr)` gives `(a b)`. We really want to execute `(rm (path 'usr 'a))` and `(rm (path 'usr 'b))`, rather than `(rm 'a)` and `(rm 'b)`. In other words, this mapping is used to preserve filepaths when traversing directory contents.

<a name="file/foldersize"></a>

#### 8.1.2\. File/Folder Size

Now let's try a more complicated example: calculating the size of a file or directory.

Suppose that we're trying to find the size of `item`. If `item` is a file, then this is pretty easy - we can just read `item` and take its length. If `item` is a directory, then we need to take the sum of the sizes of its contents, using a similar technique to what we used above. Writing a function to sum a list is straightforward:

```lisp
(define (sum lst)
	(if (null? lst)
	    0
	    (+ (car (lst)
	       (sum (cdr lst))))))
```

And now we're ready to write `size`:

```lisp
(define (size item)
        (cond ((file? item) (length (read item)))
              ((dir? item) 
               (sum (map size 
               		 (map (lambda (x) (path item x)) 
               		      (ls item)))))
              (#t 0)))
```

Note the many similarities between `clean` and `size`: the existence of higher-order functions like `map` allows us to write different file manipulation functions in a similar style.

Now that we have `size` written, we can write a simple application that periodically calculates the size of the entire filesystem and displays it. Calculating the filesystem size is simply done with `(size '/)`. This application is saved under `/apps/memoryMonitor.app` and is loaded at startup:

```lisp
(overlay (list 'Filesystem 'size: (/ (size '/) 1000) 'KB) -30 30 'memMon)
```

<a name="filesearch"></a>

#### 8.1.3\. File Search

Now let's try using a similar structure to write a `search` function that searches for a file with a given name recursively within a directory, returning the file's path if it is found and `#f` otherwise.

As a first step, we need to be able to obtain a filename from a file path - there's no primitive for this, but we can write a quick one-liner to do this: since paths are represented as `/dir1/dir2/file`, calling `split('/')` on a path (via `js-apply`) results in `(dir1 dir2 file)`, and we can take the `car` of the reverse (`reverse` is implemented in terms of `append` in `/startup/utility.lsp`) of this list to get the filename:

```lisp
(define (get-name path)
	(car (reverse (js-apply 'split path '/))))
```

Now that we have this out of the way, suppose we're given a path and we're trying to find a file within that path. If the path is pointing to a file, we just need to compare names to see if we've found what we're looking for. If the path is pointing to a directory, it gets trickier. Let's take all of the contents of the directory and run the same search on them, then filter out all of the false results. If there is anything left, then we have found our file; if not, then the directory does not contain that file. Our final function:

```lisp
(define (search dir name)
	(cond ((file? dir) 
	       (if (= (get-name dir) name)
	       	   dir
	       	   #f))
	      ((dir? dir)
	       (let ((results
	              (filter (lambda (x) (!= x #f))
			       (map (lambda (x) (search x name))
			             (map (lambda (x) (path dir x))
			                  (ls dir))))))
                    (if (> (length results) 0)
                        (car results)
                        #f)))
	      (#t #f)))
```

Alternatively, we can search for both files and folders with just a slight modification:

```lisp
(define (search dir name)
	(cond ((= (get-name dir) name) dir)
	      ((dir? dir)
	       (let ((results
	              (filter (lambda (x) (!= x #f))
			       (map (lambda (x) (search x name))
			             (map (lambda (x) (path dir x))
			                  (ls dir))))))
                    (if (> (length results) 0)
                        (car results)
                        #f)))
	      (#t #f)))
```

Let's see it in action!

```
ecmachine:/ guest$ (define (search dir name)
..    (cond ((file? dir) 
..           (if (= (get-name dir) name)
..                  dir
..                  #f))
..          ((dir? dir)
..           (let ((results
..                  (filter (lambda (x) (!= x #f))
..                   (map (lambda (x) (search x name))
..                         (map (lambda (x) (path dir x))
..                              (ls dir))))))
..                    (if (> (length results) 0)
..                        (car results)
..                        #f)))
..          (#t #f)))
ecmachine:/ guest$ (search '/ 'mapreduce.lsp)
/startup/mapreduce.lsp
ecmachine:/ guest$ (search 'apps 'mapreduce.lsp)
#f
```

What if we want to find all filenames that _contain_ a certain string, rather than just exact matches? We can write a `contains` function for strings using JavaScript's `String.indexOf()` method:

```lisp
(define (contains haystack needle)
	(!= -1 (js-apply 'indexOf haystack needle)))
```

and now we can just replace `(= (get-name dir) name)` with `(contains (get-name dir) name)`.

We can even search file bodies rather than filenames, by replacing `(= (get-name dir) name)` with `(contains (read dir) name)`.

<a name="processmanipulationrecipes"></a>

### 8.2\. Process Manipulation Recipes

<a name="processcleanup"></a>

#### 8.2.1\. Process Cleanup

Manipulating processes is not so different from manipulating files. For starters, how would we use the `kill` function to kill all running processes at once?

Note that `(processes)` returns a list of pid-name pairs:

```
ecmachine:/ guest$ (processes)
((-1 Terminal) (0 clock.app) (1 analogclock.app) (2 processmonitor.app) (3 memorymonitor.app))
```

Attempting to kill the Terminal process would give an error, so what we really want to do is map `kill` onto `(0 1 2 3)`. We can do that as follows:

```lisp
(define (kill-all)
	(map kill 
	     (filter (lambda (x) (>= x 0)) 
	     	     (map car 
	     	          (processes)))))
```

Does it work?

```
ecmachine:/ guest$ (kill-all)
((Process with PID 0 [clock.app] terminated) (Process with PID 1 [analogclock.app] terminated) (Process with PID 2 [processmonitor.app] terminated) (Process with PID 
3 [memorymonitor.app] terminated))
```

<a name="asimpletaskmanager"></a>

#### 8.2.2\. A Simple Task Manager

Let's say we want to constantly keep track of the performance of our running processes, so that we can tell if any particular application is hogging up our resources.

First things first, we'll have to get the performance of every process - we can do this by taking the above recipe and replacing `kill` with `performance`. Of course, just having a list of numbers with no context wouldn't be very helpful - it would be better to get a list of pairs of the form `(name performance)`. We can do that as follows:

```lisp
(map (lambda (proc) (list (cadr proc) (performance (car proc))))
     (processes))
```

and if we want to sort the processes by performance, in descending order of evals/sec, we can use `sort`:

```lisp
(sort
	(map (lambda (proc) (list (cadr proc) (performance (car proc))))
	     (processes))
	(lambda (proc) (- (cadr proc))))
```

This gives us:

```
((processmonitor.app 538.13) (memorymonitor.app 340.33) (analogclock.app 195.025) (clock.app 14.959) (Terminal 0.145))
```

We're almost there! Now let's say we want to put each pair on its own line - that is, we want to insert a newline between each pair. One way to do this is to write an `intersperse` function that inserts a given item between every pair of elements in a list:

```lisp
(define (intersperse lst elt)
	(if (= (length lst) 1)
	    x 
	    (cons (car x) 
	          (cons y (intersperse (cdr x) y)))))
```

Now that we have these pieces, we can put it all together to make an application that sorts the running processes by performance and displays the results as an overlay:

```lisp
(let ((header (list 'Processes '{evals/sec})) 
      (perfInfo (sort
                   (map (lambda (proc) (list (cadr proc) (performance (car proc))))
       			(processes))
   		   (lambda (proc) (- (cadr proc))))))
     (overlay (intersperse (cons header perfInfo) (newline)) -30 70 'procMon))
```

This application is saved in `/apps/processmonitor.app` and can be run at 1-second intervals with:

```lisp
(start (path 'apps 'processmonitor.app) 1000)
```

<a name="miscellaneousrecipes"></a>

### 8.3\. Miscellaneous Recipes

<a name="analogclock"></a>

#### 8.3.1\. Analog Clock

Who says that you can't have fancy graphics in a text-based environment? By setting up multiple overlays and performing some trigonometry, we can build an analog clock widget, just like the kind you can get in your fancy operating system of choice:

```lisp
(let* ((center-x -120) (center-y -160)
       (pi 3.141592653589793)
       (s-angle (* (/ pi 30) (+ (car (time '(s))) 15)))
       (m-angle (* (/ pi 30) (+ (car (time '(m))) 15 (/ (+ (car (time '(s))) 15) 60))))
       (h-angle (* (/ pi 6) (+ (car (time '(h))) 15 (/ (+ (car (time '(m))) 15) 60))))
       (x-pos-s (- center-x (* 90 (math 'cos s-angle))))
       (y-pos-s (- center-y (* 90 (math 'sin s-angle))))
       (x-pos-m (- center-x (* 60 (math 'cos m-angle))))
       (y-pos-m (- center-y (* 60 (math 'sin m-angle))))
       (x-pos-h (- center-x (* 30 (math 'cos h-angle))))
       (y-pos-h (- center-y (* 30 (math 'sin h-angle)))))
      (begin 
         (overlay '(O) center-x center-y 'analogclockcenter)
         (overlay '(h) x-pos-h y-pos-h 'analogclockhours)
         (overlay '(m) x-pos-m y-pos-m 'analogclockminutes)
         (overlay '(s) x-pos-s y-pos-s 'analogclockseconds)))
```

(`let*` is a special binding construct that evaluates each binding in an environment containing the previous bindings. We can't use the regular `let` construct here because some of the bindings depend on each other.)

This application is saved in `/apps/analogclock.app` and can be run with:

```lisp
(start (path 'apps 'analogclock.app) 1000)
```

<a name="otherideas"></a>

#### 8.3.2\. Other Ideas

Some other ideas for applications that I've been playing around with but haven't yet finalized code for include:

- Automated backup
- Simple anti-virus
- Notes manager

<a name="edasimpletexteditor"></a>

9\. ed, a Simple Text Editor
-----

We've seen some simple examples of recipes using ECMAchine's system methods, but can we build a more sophisticated application? For example, let's try taking a look at how to build a simple text editor, which I am going to give the remarkably unoriginal name of **ed**. It's not very fancy, but it does its job and can respond to 15 different commands. And, not counting a few simple helper functions, the whole editor takes up less than 100 lines of Lisp.

_Note: `ed` is not currently defined in ECMAchine by default. To use it, first copy all of the code at the [end of this section](#puttingitalltogether) into the console and run it._

<a name="introducinged"></a>

### 9.1\. Introducing ed

Before we get into implementation details, let's first try running **ed**. You can start the program with the `(ed)` function, at which point an overlay appears in the top-left corner of the screen. Operations are performed by running commands of the form `(ed 'command <params>)`, and editing is line-by-line: a cursor denotes the current line and lines can be created, modified, or deleted.

For example:

```scheme
ecmachine:/ guest$ (ed 'new)
ecmachine:/ guest$ (ed 'write "line1")
ecmachine:/ guest$ (ed 'insert-after "line2")
ecmachine:/ guest$ (ed 'insert-after "line3")
ecmachine:/ guest$ (ed 'up) % moves the cursor up
ecmachine:/ guest$ (ed 'delete)
ecmachine:/ guest$ (ed 'write " :-}")
ecmachine:/ guest$ (ed 'save-as 'test.txt)
(Saved file /test.txt)
ecmachine:/ guest$ (read 'test.txt)
line1
line3 :-}
```

ed supports the following commands:

- **new**: `(ed 'new)` opens a new blank file
- **open**: `(ed 'open path)` opens the file at _path_
- **save**: `(ed 'save)` saves the contents of the text buffer to the current file location (if any)
- **save-as**: `(ed 'save-as path)` saves the contents of the text buffer to _path_
- **exit**: `(ed 'exit)` exits ed
- **up**: `(ed 'up)` moves the cursor up one line
- **down**: `(ed 'down)` moves the cursor down one line
- **write**: `(ed 'write text)` appends `text` to the current line
- **edit**: `(ed 'edit text)` replaces the current line with `text`
- **delete**: `(ed 'delete)` deletes the current line
- **insert-before**: `(ed 'insert-before text)` inserts `text` into a new line before the current line
- **insert-after**: `(ed 'insert-after text)` inserts `text`
- **map**: `(ed 'map func)` maps every line of the text buffer with `func` (that is, every line `L` is replace with `func(L)`
- **run**: `(ed 'run)` runs the contents of the text buffer as Lisp in the global environment 

<a name="designoverview"></a>

### 9.2\. Design Overview

The two big questions that needed to be answered when designing ed were:

1. Is ed going to be an application or a function?
2. How is the text buffer stored?
3. How is the text buffer displayed?

While implementing ed as an application would have allowed it to run persistently, a text editor doesn't really need to perform any operations on its own, because everything that it does (e.g. load/save files, modify text, move the cursor) is done directly in response to user input. As such, I decided that it would be more straightforward to simply implement ed as a function `(ed)`, that takes a string literal representing the operation to perform as an argument, as well as an optional second argument for some operations.

*ed* stores the current text buffer as a newline-delimited string in the `ed.contents` variable. Storing the text as a string rather than a list of lines makes it easy to save and load files, since text files are stored as strings as well. On the other hand, line-by-line manipulation requires a linked-list structure to work, so `ed.contents` needs to be converted into a list of lines before every edit command and then be converted back into a string. Fortunately, this isn't too hard to implement, as will be shown in the next section.

ed displays the current text buffer in an top-left-aligned overlay containing the filename and then a list of lines. The current position of the cursor is displayed as a caret **'> '** before the line it's on.

The current filename is stored in `ed.filename` and the current line number is stored in `ed.cursor`.

<a name="helperfunctions"></a>

### 9.3\. Helper Functions

Now we can get started on writing code, but before we can start ed itself, we need to make a few more general helper functions.

First of, we need to be able to switch the contents of text files between a single newline-separated string (for storage and display) and a list of lines (for line-by-line manipulation). This is pretty easy to accomplish using JavaScript's `String.split` and `Array.join` methods:

```scheme
(define (join-lines arr)
  (js-apply 'join arr "\n"))
(define (split-lines str)
  (js-apply 'split str "\n"))
```

Storing files as lists of strings makes it easy to create, modify, and delete lines, using generic methods for modifying/creating/deleting the nth element of a list:

```scheme
(define (map-at-index func lst i)
  (if (= i 0)
      (cons (func (car lst)) (cdr lst))
      (cons (car lst) (map-at-index func (cdr lst) (- i 1)))))
(define (insert-at-index elt lst i)
  (if (= i 0)
      (cons elt lst)
      (cons (car lst) (insert-at-index elt (cdr lst) (- i 1)))))
(define (remove-at-index lst i)
  (if (= i 0)
      (cdr lst)
      (cons (car lst) (remove-at-index (cdr lst) (- i 1)))))
```

Finally, for some tasks (such as rendering the cursor), it's helpful to have a more powerful `map` function whose functions take both the list element and its index in the list as arguments. Let's call it `mapi`. Unfortunately, I can't think of any elegant way to do this that doesn't involve a separate helper function:

```scheme
(define (mapi proc items)
  (mapi-helper proc items 0))
(define (mapi-helper proc items i)
  (if (null? items)
    nil
    (cons (proc (car items) i)
      (mapi-helper proc (cdr items) (+ i 1)))))
```

<a name="theskeletonofed"></a>

### 9.4\. The Skeleton of ed

The `(ed)` function has a fairly simple overall structure. It can take between 0 and 2 parameters (via the construct `(define (ed . args) ...)` ). Calling `(ed)` without any parameters defaults to `(ed 'refresh)`, which renders the contents of the text buffer onto the screen. If `(> 0 (length args))`, then the arguments are separated into `func` and `params` (which is either a list of a single element or an empty list) - some commands take a parameter and some don't. The rest of `(ed)` is taken up by a single `cond` block that selects the appropriate command:

```scheme
(define (ed . args)
  (if (= 0 (length args))
      (ed 'refresh)
      (let ((func (car args))
            (params (cdr args)))
                (cond ((= func <command>) <implementation>)
                      ((= func <command>) <implementation>)
                          
                         . . .

                ))))
```

<a name="displayingtheeditorcontents"></a>

### 9.5\. Displaying the Editor Contents

Given that the contents of current text buffer are stored as a string in `ed.contents`, how would we go about displaying them?

Well, we could just display them directly:

```scheme
(overlay ed.contents 30 30 'ed)
```

Ah, that wasn't too bad. How about if we want to indicate the position of the cursor at the line whose number is `ed.cursor`? Let's do this by prefixing the current line with "`>`" and prefixing all other lines with "`. `". To do this, we'd have to (1) convert the string in `ed.contents` into a list of lines, (2) transform each line based on whether its number equals `ed.cursor`, and (3) convert back to a newline-delimited string for display:

```scheme
(join-lines
 (mapi (lambda (x i) 
        (if (= i ed.cursor)
            (+ "> " x)
            (+ ". " x)))
        (split-lines ed.contents)))
```

We're almost there! One last thing that'd be nice would be displaying the name of the current file (or `"New File"` if the file has not been loaded/saved yet). And while we're add it, let's add a nice title bar of sorts:

```scheme
((= func 'refresh)
  (let ((display-contents 
         (join-lines 
          (append 
                 (list "::: ECMAchine Text Editor (v0.1) :::"
                       (+ "== " (if (= ed.filename "") "New File" ed.filename) " =="))
                 (mapi (lambda (x i) 
                         (if (= i ed.cursor)
                             (+ "> " x)
                             (+ ". " x)))
                      (split-lines ed.contents))))))
       (overlay display-contents 30 30 'ed)))
```

Not too shabby. And since there's no process running in the background for **ed**, exiting is just a matter of clearing this overlay:

```scheme
((= func 'exit) (clear-overlay 'ed))
```

Most of the commands that follow use Scheme's `begin` construct, which executes a sequence of functions in order. We'll need to use `begin` because a lot of the time we'll want to modify the contents of **ed** and then refresh the display. In other words, the general template for most editing commands will be:

```scheme
(begin
  % do something
  (ed 'refresh))
```

Ok, let's begin defining commands!

<a name="filecommands"></a>

### 9.6\. File Commands

**ed** has four commands that deal directly with files: `new`, `open`, `save`, and `save-as`.

When `(ed 'new)` is executed, the three variables used by **ed** (`ed.filename`, `ed.contents`, and `ed.cursor`) are all reset:

```scheme
((= func 'new)
 (begin
   (set! ed.filename "")
   (set! ed.contents "")
   (set! ed.cursor 0)
   (ed 'refresh)))
```

`(ed 'open <file>)` sets `ed.filename` and `ed.contents` to be those of _<file>_ (where _<file>_ is a valid path):

```scheme
((or (= func 'open) (= func 'load))
 (begin
   (set! ed.filename (car params))
   (set! ed.contents (read ed.filename))
   (ed 'refresh)))
```

`(ed 'save)` and `(ed 'save-as <file>)` save `ed.contents` to either the current file (if any) or a specified _<file>_:

```scheme
((= func 'save)
 (if (= ed.filename "")
     '(No filename chosen - use the 'save-as command instead)
     (save ed.filename ed.contents)))
((= func 'save-as)
 (begin
   (set! ed.filename (car params))
   (ed 'refresh)
   (save ed.filename ed.contents)))
```

<a name="editcommands"></a>

### 9.7\. Edit Commands

The `up` and `down` command generally move the cursor by decrementing or incrementing, respectively, the value stored in `ed.cursor`. Special care must be taken to make sure that `ed.cursor` stays within the bounds of the file:

```scheme
((= func 'up)
 (begin
   (set! ed.cursor (math 'max (list 0 (- ed.cursor 1))))
   (ed 'refresh)))
((= func 'down)
 (begin
   (set! ed.cursor (math 'min (list (+ ed.cursor 1) (- (length (split-lines ed.contents)) 1))))
   (ed 'refresh)))
```

To append text to the current line for the `write` command: we can (1) split `ed.contents` into a list of lines, (2) find the current line and append the given text to it via `map-at-index`, and (3) join the resulting list back into a newline-delimited string:

```scheme
((= func 'write)
 (begin
   (set! ed.contents (join-lines (map-at-index (lambda (str) (+ str (car params))) (split-lines ed.contents) ed.cursor)))
   (ed 'refresh)))
```

`edit` functions almost identically to `write`, except that it replaces the current line outright rather than appending to it, by mapping `(λ (str) (car params))` rather than `(λ (str) (+ str (car params)))`:

```scheme
((= func 'edit)
 (begin
   (set! ed.contents (join-lines (map-at-index (lambda (str) (car params)) (split-lines ed.contents) ed.cursor)))
   (ed 'refresh)))
```

To remove the current line for the `delete` command, we can (1) split `ed.contents` into a list of lines, (2) find the current line and remove it via `remove-at-index`, and (3) join the resulting list back into a newline-delimited string:

```scheme
((= func 'delete)
 (begin
   (set! ed.contents (join-lines (remove-at-index (split-lines ed.contents) ed.cursor)))
   (ed 'down)))
```

To insert new lines before or after the cursor, we can (1) split `ed.contents` into a list of lines, (2) find either the current line (for `insert-before`) or the following line (for `insert-after`) and insert a new line at its index via `insert-at-index`, and (3) join the resulting list back into a newline-delimited string:

```scheme
((= func 'insert-before)
 (begin
   (set! ed.contents (join-lines (insert-at-index (car params) (split-lines ed.contents) ed.cursor) ))
   (ed 'refresh)))
((= func 'insert-after)
 (begin
   (set! ed.contents (join-lines (insert-at-index (car params) (split-lines ed.contents) (+ ed.cursor 1))))
   (ed 'down)))
```

<a name="coolthings"></a>

### 9.8\. Cool Things

Now that we can perform all of the basic commands expected of a simple file editor, let's try our hand at some features that are a little more interesting.

<a name="map"></a>

#### 9.8.1\. Map

Sometimes we want to apply an arbitrary function to every line in a file.

To this end, `map` takes an argument of the form `(λ (x i) ...)`, where `x` represents the contents of a given line and `i` its line number, and maps this function onto every line in the text buffer:

```scheme
((= func 'map)
 (begin
   (set! ed.contents (join-lines (mapi (car params) (split-lines ed.contents))))
   (ed 'refresh)))
```

When can this come in handy? Well, let's say that we're reading a file and want to be able to see line numbers for it. This can be accomplished very easily using `(ed 'map)`, as can be seen below. (Note, however, that this edits the contents of the text buffer directly, so make sure not to save the file after using this!)

```scheme
(ed 'map (lambda (x i) (+ (+ i 1) ". " x)))
```

Alternatively, if you want a more sophisticated line numbering, with all numbers right aligned, you can use this monstrosity of a function (bonus points to anyone who can accomplish the same effect with less horrendous code):

```scheme
(ed 'map 
  (lambda (x i) 
    (+ 
      (js-apply 'join (js-apply "new Array" (math 'floor (- 5 (/ (math 'log (+ i 1.1)) (math 'log 10))))) " ")
      (+ i 1)
      "| "
      x)))
```

<a name="run"></a>

#### 9.8.2\. Run

Given that ECMAchine is an OS based around S-expressions, it stands to reason that a lot of the files that we're be editing will be Lisp files. Could we run them from within **ed**?

We can, but it's a little tricky - since the only system function that allows us to execute arbitrary code is `exec`, we'll have to save `ed.contents` to a temporary file, then execute the file, a finally remove the file and display the results, in that order:

```scheme
((= func 'run)
 (begin
   (save 'tmp.txt ed.contents)
   (let ((result (exec 'tmp.txt)))
        (begin (rm 'tmp.txt) result))))
```

Let's see it in action!

```
ecmachine:/ guest$ (ed 'new)
ecmachine:/ guest$ (ed 'write "(+ 2 3)")
ecmachine:/ guest$ (ed 'run)
5
```

<a name="andmore"></a>

#### 9.8.3\. And more

What other features could we add to the editor? 

Macros are certainly possible - a `macro-record` command could store all subsequent `(ed)` commands to a list, while `macro-run` could execute the recorded commands an arbitrary number of times.

An undo/redo feature could be simply implemented, by keeping a list of past states that's updated with every command run. A trickier but more memory-efficient solution would be to store only a list of operations (like for macros) - the tricky bit is that the undo command would have to be able to `reverse` a command (for instance, turning an insert into a delete, and vice versa).

The `map` command could easily be turned into a `filter` command - this could be handy for something like file search. What about replace?

Feeling especially adventurous? Try using the ECMAchine's `(ajax)` function to integrate **ed** with some online file hosting/editing system. In this case, a background process would probably be needed to notify **ed** of changes to the file.

On a more practical note, **ed** currently suffers from an immense usability issue: it always tries to display every line of the current file and thus is impractical for large files. A better approach would be to only display perhaps the 20 lines closest to the cursor at any given time.

<a name="puttingitalltogether"></a>

### 9.9\. Putting It All Together

Here is all of the code needed to be able to run **ed** on ECMAchine:

```scheme
(define (join-lines arr)
  (js-apply 'join arr "\n"))
(define (split-lines str)
  (js-apply 'split str "\n"))
(define (map-at-index func lst i)
  (if (= i 0)
      (cons (func (car lst)) (cdr lst))
      (cons (car lst) (map-at-index func (cdr lst) (- i 1)))))
(define (insert-at-index elt lst i)
  (if (= i 0)
      (cons elt lst)
      (cons (car lst) (insert-at-index elt (cdr lst) (- i 1)))))
(define (remove-at-index lst i)
  (if (= i 0)
      (cdr lst)
      (cons (car lst) (remove-at-index (cdr lst) (- i 1)))))
(define (mapi proc items)
  (mapi-helper proc items 0))
(define (mapi-helper proc items i)
  (if (null? items)
    nil
    (cons (proc (car items) i)
      (mapi-helper proc (cdr items) (+ i 1)))))

(define ed.contents "")
(define ed.filename "")
(define ed.cursor 0)
(define (ed . args)
  (if (= 0 (length args))
      (ed 'refresh)
      (let ((func (car args))
            (params (cdr args)))
           (cond ((= func 'refresh)
                   (let ((display-contents 
                          (join-lines 
                           (append 
                                  (list "::: ECMAchine Text Editor (v0.1) :::"
                                        (+ "== " (if (= ed.filename "") "New File" ed.filename) " =="))
                                  (mapi (lambda (x i) 
                                          (if (= i ed.cursor)
                                              (+ "> " x)
                                              (+ ". " x)))
                                       (split-lines ed.contents))))))
                         (overlay display-contents 30 30 'ed)))
                 ((= func 'exit) (clear-overlay 'ed))
                 
                 ((= func 'new)
                  (begin
                    (set! ed.filename "")
                    (set! ed.contents "")
                    (set! ed.cursor 0)
                    (ed 'refresh)))
                 ((or (= func 'open) (= func 'load))
                  (begin
                    (set! ed.filename (car params))
                    (set! ed.contents (read ed.filename))
                    (ed 'refresh)))
                 ((= func 'save)
                  (if (= ed.filename "")
                      '(No filename chosen - use the 'save-as command instead)
                      (save ed.filename ed.contents)))
                 ((= func 'save-as)
                  (begin
                    (set! ed.filename (car params))
                    (ed 'refresh)
                    (save ed.filename ed.contents)))
                 
                 ((= func 'up) 
                  (begin
                    (set! ed.cursor (math 'max (list 0 (- ed.cursor 1))))
                    (ed 'refresh)))
                 ((= func 'down)
                  (begin
                    (set! ed.cursor (math 'min (list (+ ed.cursor 1) (- (length (split-lines ed.contents)) 1))))
                    (ed 'refresh)))
                 
                 ((= func 'write)
                  (begin
                    (set! ed.contents (join-lines (map-at-index (lambda (str) (+ str (car params))) (split-lines ed.contents) ed.cursor)))
                    (ed 'refresh)))
                 ((= func 'edit)
                  (begin
                    (set! ed.contents (join-lines (map-at-index (lambda (str) (car params)) (split-lines ed.contents) ed.cursor)))
                    (ed 'refresh)))
                 ((= func 'delete)
                  (begin
                    (set! ed.contents (join-lines (remove-at-index (split-lines ed.contents) ed.cursor)))
                    (ed 'down)))
                 ((= func 'insert-before)
                  (begin
                    (set! ed.contents (join-lines (insert-at-index (car params) (split-lines ed.contents) ed.cursor)))
                    (ed 'refresh)))
                 ((= func 'insert-after)
                  (begin
                    (set! ed.contents (join-lines (insert-at-index (car params) (split-lines ed.contents) (+ ed.cursor 1))))
                    (ed 'down)))
                 
                 ((= func 'map)
                  (begin
                    (set! ed.contents (join-lines (mapi (car params) (split-lines ed.contents))))
                    (ed 'refresh)))
                 ((= func 'run)
                  (begin
                    (save 'tmp.txt ed.contents)
                    (let ((result (exec 'tmp.txt)))
                         (begin (rm 'tmp.txt) 
                                result))))
                    
                ))))
```

<a name="what'snext?"></a>

10\. What's Next??
-----

Here are some things I'd like to see in ECMAchine:

- Language 
 - Comments
 - Newlines should be preserved when:
     - writing to files
     - displaying function contents
- Library
 - (accumulate), other higher-order functions?
 - wrappers for the `ajax` function
- Processes
 - Should processes (and scripts) have their own environments rather than using global environment?
 - Allow passing arguments to a process/script
 - Refactor processes into a new class (like Filesystem)
 - More accurate performance measurement (rather than just adding up all evals and dividing by total time)
 - Bonus: come up with a simple time-sharing system that works in JavaScript? 
     - For example, processes could continually adjust their interval in an attempt to reach ~1000 evals/sec systemwide 
     - Could timesharing apply to scripts (one-time operations) in addition to processes (recurring operations)?
 - Allow overlays to be draggable?
- Programs
 - It would be very cool to actually get some more involved programs working: e.g. a web browser
 - This involves lots of challenges: e.g. how to get user input in a program
- Sessions
	- User accounts?
	- Some sort of import/export support?

<a name="acknowledgements"></a>

11\. Acknowledgements
-----

I'd like to thank:

- Jakub Jankiewicz's [jQuery Terminal Emulator](http://terminal.jcubic.pl/), without which I wouldn't have such a realistic-looking terminal
- Kris Kowal's [es5-shim](https://github.com/kriskowal/es5-shim), for improving compatibility with older browsers
- John Reese's [Markdown Preprocessor](https://github.com/jreese/markdown-pp), which I use to preprocess this readme file
- Abelson and Sussman, whose [wizardly textbook](http://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Computer_Programs) was an invaluable resource in designing the REPL
- [Tikhon Jelvis](https://github.com/TikhonJelvis), for his help in implementing the REPL
