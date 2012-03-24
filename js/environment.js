
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
				'contents': "(let* ((center-x -90) (center-y -130)" +
									"\n       (pi (/ 314159265 100000000))" +
									"\n       (x-pos (- center-x (* 60 (math 'cos (* (/ pi 30) (+ (car (time '(s))) 15))))))" +
	      					"\n       (y-pos (- center-y (* 60 (math 'sin (* (/ pi 30) (+ (car (time '(s))) 15)))))))" +
	     						"\n      (begin " +
	     						"\n         (overlay '(x) center-x center-y 'analogclockcenter)" +
	     						"\n         (overlay '(o) x-pos y-pos 'analogclock)))"
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
								"\n				 (perfInfo (sort" +
								"\n						(map " +
						    "\n       			(lambda (proc) (list (cadr proc) (performance (car proc))))" +
						    "\n       			(processes))" +
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
			'utility.lsp': {
				'type': 'file',
				'contents': "(define (cadr x) (car (cdr x)))" +
									"\n(define else #t)" +
						      "\n(define nil '())" +
									"\n(define (null? lst) (= (length lst) 0))" +
									"\n(define (intersperse x y)" +
									"\n    (if (= (length x) 1)" +
									"\n        x" +
									"\n        (cons (car x) (cons y (intersperse (cdr x) y)))))" +
									"\n(define (sum lst)" +
									"\n    (if (null? lst)" +
									"\n        0" + 
									"\n        (+ (car lst) (sum (cdr lst)))))"
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
									"\n(define (kill-all) (map kill (filter (lambda (x) (>= x 0)) (map car (processes)))))"
			},
			'justforfun.lsp': {
				'type': 'file',
				'contents': "(define (smile) ':-})"
			},
			
			// Startup applications
			'clock.lnk': {
				'type': 'file',
				'contents': "(start (path '/ 'apps 'clock.app) 1000)"
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