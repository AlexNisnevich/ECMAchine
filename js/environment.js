var processes = [];

var globalEnvironment = {
	'__fileSystem': {
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
			'virushunter.app': {
				'type': 'file',
				'contents': "(if (file? (path '/ 'virus))" +
					      "\n      (begin (cd '/)" +
					      "\n             (rm 'virus)" +
					      "\n             (quote (Virus removed!)))" +
					      "\n      (do-nothing))"
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
			/*'clock.lnk': {
				'type': 'file',
				'contents': "(start (path '/ 'apps 'clock.app) 1000)"
			},*/
			'utility.lsp': {
				'type': 'file',
				'contents': "(begin" +
					      "\n       (define nil ())" +
						  "\n	    (define null? (lambda (lst) (= (length lst) 0))))"
			},
			'math.lsp': {
				'type': 'file',
				'contents': "(begin" +
					      "\n       (define abs (lambda (x)" +
					      "\n             (cond ((> x 0) x)" +
				      	  "\n                   (#t (- x)))))" +
					      "\n       (define fact (lambda (x)" +
					      "\n             (if (= x 1)" +
					      "\n                 1" +
					      "\n                 (* x (fact (- x 1)))))))" +
					      "\n       (define math (lambda (func args)" +
					      "\n             (js-apply (+ 'Math. func) args)))"
			},
			'justforfun.lsp': {
				'type': 'file',
				'contents': "(define smile (lambda () ':-}))"
			}
		},
		'/usr': {
			
		}
	},
	'__currentDir': '/'
};