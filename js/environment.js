var processes = [];

var globalEnvironment = {
	'__fileSystem': {
		'/': {
			'README': {
				'type': 'file',
				'contents': 'ECMAchine by Alex Nisnevich. Thanks to Jakub Jankiewicz for his jQuery Terminal plugin (distributed under LGPL).'
			},
			'LICENSE': {
				'type': 'file',
				'contents': 'ECMAchine by Alex Nisnevich.'
			},
			'twoplusthree': {
				'type': 'file',
				'contents': '(+ 2 3)'
			},
			'apps': {
				'type': 'dir'
			},
			'usr': {
				'type': 'dir'
			}
		},
		'/apps': {
			'clock.app': {
				'type': 'file',
				'contents': "(overlay (time) -30 -30 'clock)"
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
						  "\n     (map rm (ls))" +
						  "\n     (map kill (processes)))"
			}
		},
		'/usr': {
			
		}
	},
	'__currentDir': '/'
};