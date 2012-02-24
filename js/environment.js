
var fileSystemFrame = {
	'__fileSystem': {
		toString: function () { return '#<FileSystem>'},
		'/': {
			'README': {
				'type': 'file',
				'contents': 'ECMAchine by Alex Nisnevich. Thanks to Jakub Jankiewicz for his jQuery Terminal plugin (distributed under LGPL).'
			},
			'musings': {
				'type': 'file',
				'contents': 'Biggest hackathon is best hackathon.'
			},
			'twoplusthree': {
				'type': 'file',
				'contents': '(+ 2 3)'
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
				'contents': "(overlay (time) -30 -65 'unixclock)"
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
				'contents': "(define perfInfo (sort" +
								"\n    (map " +
					      "\n        (lambda (proc) (list (cadr proc) (performance (car proc))))" +
					      "\n        (processes))" +
					      "\n    (lambda (proc) (- (cadr proc)))))" +
					      "\n(define header (list 'Processes '{evals/sec}))" +
					      "\n(overlay (intersperse (cons header perfInfo) (newline)) -30 30 'procMon)"
			},
			'killeverything.app': {
				'type': 'file',
				'contents': "(begin" +
						  "\n     (cd '/)" +
						  "\n     (map rm (ls)))" +
					      "\n     (map kill (map car (processes))))"
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
									"\n        (cons (car x) (cons y (intersperse (cdr x) y))))))"
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
			}
		},
		'/usr': {
			
		}
	}
};