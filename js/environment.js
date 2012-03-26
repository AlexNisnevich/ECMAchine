
var fileSystemFrame = {
	'__fileSystem': {
		toString: function () { return '#<FileSystem>'},
		'/': {
			'readme.txt': {
				'type': 'file',
				'contents': 'ECMAchine by Alex Nisnevich. Thanks to Jakub Jankiewicz for his jQuery Terminal plugin (distributed under LGPL).' +
					'For more information check out the git repo at https://github.com/AlexNisnevich/ECMAchine'
			},
			'cleanup.s': {
				'type': 'file',
				'contents': "(rm (path '/ 'usr))" +
									"\n(mkdir (path '/ 'usr))"
			},
			'apps': {
				'type': 'dir'
			},
			'startup': {
				'type': 'dir'
			},
			'usr': {
				'type': 'dir'
			}
		},
		'/apps': {
			'clock.app': {
				'type': 'file',
				'contents': "(overlay (time (list 'h ': 'm ': 's)) -30 -30 'clock)"
			},
			'unixclock.app': {
				'type': 'file',
				'contents': "(overlay (time) -30 -70 'unixclock)"
			},
			'analogclock.app': {
				'type': 'file',
				'contents': "(let* ((center-x -120) (center-y -160)" +
									"\n       (pi 3.141592653589793)" +
									"\n       (s-angle (* (/ pi 30) (+ (car (time '(s))) 15)))" +
									"\n       (m-angle (* (/ pi 30) (+ (car (time '(m))) 15 (/ (+ (car (time '(s))) 15) 60))))" +
									"\n       (h-angle (* (/ pi 6) (+ (car (time '(h))) 15 (/ (+ (car (time '(m))) 15) 60))))" +
									"\n       (x-pos-s (- center-x (* 90 (math 'cos s-angle))))" +
	      					"\n       (y-pos-s (- center-y (* 90 (math 'sin s-angle))))" +
	     						"\n       (x-pos-m (- center-x (* 60 (math 'cos m-angle))))" +
	      					"\n       (y-pos-m (- center-y (* 60 (math 'sin m-angle))))" +
	     						"\n       (x-pos-h (- center-x (* 30 (math 'cos h-angle))))" +
	      					"\n       (y-pos-h (- center-y (* 30 (math 'sin h-angle)))))" +
	     						"\n      (begin " +
	     						"\n         (overlay '(O) center-x center-y 'analogclockcenter)" +
	     						"\n         (overlay '(h) x-pos-h y-pos-h 'analogclockhours)" +
	     						"\n         (overlay '(m) x-pos-m y-pos-m 'analogclockminutes)" +
	     						"\n         (overlay '(s) x-pos-s y-pos-s 'analogclockseconds)))"
			},
			'virushunter.app': {
				'type': 'file',
				'contents': "(if (file? (path '/ 'virus))" +
					      "\n      (begin (cd '/)" +
					      "\n             (rm 'virus)" +
					      "\n             (quote (Virus removed!)))" +
					      "\n      (do-nothing))"
			},
			'processmonitor.app': {
				'type': 'file',
				'contents': "(let ((header (list 'Processes '{evals/sec})) " +
						  "\n      (perfInfo (sort" +
						  "\n                   (map (lambda (proc) (list (cadr proc) (performance (car proc))))" +
						  "\n       			     (processes))" +
						  "\n   				(lambda (proc) (- (cadr proc))))))" +
					      "\n       (overlay (intersperse (cons header perfInfo) (newline)) -30 70 'procMon))"
			},
			'memorymonitor.app': {
				'type': 'file',
				'contents': "(overlay (list 'Filesystem 'size: (/ (size '/) 1000) 'KB) -30 30 'memMon)"
			}
		},
		'/startup': {
			// Lisp functions
			'_utility.lsp': { // utility.lsp must be loaded first
				'type': 'file',
				'contents': "(define (cadr x) (car (cdr x)))" +
							"\n(define else #t)" +
						    "\n(define nil '())" +
							"\n(define (null? lst) (= (length lst) 0))" +
							"\n(define (append ls1 ls2)" +
							"\n   (if (null? ls1)" +
							"\n     ls2" +
							"\n     (cons (car ls1) (append (cdr ls1) ls2))))" +
							"\n(define (reverse lst)" +
							"\n	  (if (null? lst)" +
							"\n	      nil" +
							"\n	      (append (reverse (cdr lst)) (list (car lst)))))" +
							"\n(define (intersperse x y)" +
							"\n    (if (= (length x) 1)" +
							"\n        x" +
							"\n        (cons (car x) (cons y (intersperse (cdr x) y)))))" +
							"\n(define (sum lst)" +
							"\n    (if (null? lst)" +
							"\n        0" + 
							"\n        (+ (car lst) (sum (cdr lst)))))" +
							"\n(define (contains haystack needle)" +
							"\n    (!= -1 (js-apply 'indexOf haystack needle)))"
			},
			'mapreduce.lsp': {
				'type': 'file',
				'contents': "(define (map proc items)" +
									"\n    (if (null? items)" +
									"\n        nil" +
									"\n        (cons (proc (car items))" +
									"\n            (map proc (cdr items)))))" +
									"\n(define (filter pred seq)" +
									"\n	  (cond ((null? seq) nil)" +
									"\n	  			((pred (car seq))" +
									"\n		  		 (cons (car seq)" +
									"\n		  		 			 (filter pred (cdr seq))))" +
									"\n  				(else (filter pred (cdr seq)))))"
			},
			'math.lsp': {
				'type': 'file',
				'contents': "(define (abs x)" +
					        "\n    (cond ((> x 0) x)" +
				      	  "\n        (#t (- x))))" +
					        "\n(define (fact x)" +
					        "\n    (if (= x 1)" +
					        "\n        1" +
					        "\n        (* x (fact (- x 1)))))" +
					        "\n(define (math func args)" +
					        "\n    (js-apply (+ 'Math. func) args))"
			},
			'files.lsp': {
				'type': 'file',
				'contents': "(define (size item)" +
    							"\n		(cond ((file? item) (length (read item)))" +
         				 	"\n				  ((dir? item) (sum (map size (map (lambda (x) (path item x)) (ls item)))))" +
         		 			"\n					(else 0)))" + 
         		 			"\n(define (get-name path)" +
    							"\n   (car (reverse (js-apply 'split path '/))))" +
									"\n(define (kill-all) (map kill (filter (lambda (x) (>= x 0)) (map car (processes)))))"
			},
			'justforfun.lsp': {
				'type': 'file',
				'contents': "(define (smile) ':-})"
			},
			
			// Startup applications
			'clocks.lnk': {
				'type': 'file',
				'contents': "(start (path '/ 'apps 'clock.app) 1000)" +
									"\n(start (path '/ 'apps 'analogclock.app) 1000)"
			},
			'pmon.lnk': {
				'type': 'file',
				'contents': "(start (path '/ 'apps 'processmonitor.app) 1000)"
			},
			'memmon.lnk': {
				'type': 'file',
				'contents': "(start (path '/ 'apps 'memorymonitor.app) 5000)"
			}
		},
		'/usr': {
			
		}
	}
};