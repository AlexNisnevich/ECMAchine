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
						  "\n     (map rm (ls)))"
					   // "\n     (map kill (processes)))" // doesn't work atm
			}
		},
		'/startup': {
			'clock.lnk': {
				'type': 'file',
				'contents': "(start (path '/ 'apps 'clock.app) 1000)"
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