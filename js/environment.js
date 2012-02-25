
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
				'contents': "(map kill" +
					        "\n     (filter (lambda (pid) (>= pid 0))" + 
					        "\n           (map (lambda (proc) (car proc)) (processes))))"
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
				'contents': "(define perfmon.perfInfo (sort" +
								"\n    (map " +
					      "\n        (lambda (proc) (list (cadr proc) (performance (car proc))))" +
					      "\n        (processes))" +
					      "\n    (lambda (proc) (- (cadr proc)))))" +
					      "\n(define perfmon.header (list 'Processes '{evals/sec}))" +
					      "\n(overlay (intersperse (cons perfmon.header perfmon.perfInfo) (newline)) -30 70 'procMon)"
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
				'contents': "(define cadr (lambda (x) (car (cdr x))))" +
									"\n(define else #t)" +
						      "\n(define nil '())" +
									"\n(define null? (lambda (lst) (= (length lst) 0)))" +
									"\n(define intersperse (lambda (x y)" +
									"\n    (if (= (length x) 1)" +
									"\n        x" +
									"\n        (cons (car x) (cons y (intersperse (cdr x) y))))))" +
									"\n(define sum (lambda (lst)" +
									"\n    (if (null? lst)" +
									"\n        0" + 
									"\n        (+ (car lst) (sum (cdr lst))))))"
			},
			'mapreduce.lsp': {
				'type': 'file',
				'contents': "(define map (lambda (proc items)" +
									"\n    (if (null? items)" +
									"\n        nil" +
									"\n        (cons (proc (car items))" +
									"\n            (map proc (cdr items))))))" +
									"\n(define filter (lambda (pred seq)" +
									"\n	  (cond ((null? seq) nil)" +
									"\n	  			((pred (car seq))" +
									"\n		  		 (cons (car seq)" +
									"\n		  		 			 (filter pred (cdr seq))))" +
									"\n  				(else (filter pred (cdr seq))))))"
			},
			'math.lsp': {
				'type': 'file',
				'contents': "(define abs (lambda (x)" +
					        "\n    (cond ((> x 0) x)" +
				      	  "\n        (#t (- x)))))" +
					        "\n(define fact (lambda (x)" +
					        "\n    (if (= x 1)" +
					        "\n        1" +
					        "\n        (* x (fact (- x 1))))))" +
					        "\n(define math (lambda (func args)" +
					        "\n    (js-apply (+ 'Math. func) args)))"
			},
			'files.lsp': {
				'type': 'file',
				'contents': "(define size (lambda (item)" +
									"\n   (cond ((file? item) (length (read item)))" +
									"\n   ((dir? item) (begin" +
									"\n       (cd item)" +
									"\n       (define temp (sum (map size (ls))))" + 
									"\n       (cd '..)" +
									"\n       temp))" +
									"\n   (else 0))))"
			},
			'justforfun.lsp': {
				'type': 'file',
				'contents': "(define smile (lambda () ':-}))"
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