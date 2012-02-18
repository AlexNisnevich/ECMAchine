var processes = [];

var globalEnvironment = {
	'__fileSystem': {
		'/': {
			'README': {
				'type': 'file',
				'contents': 'blahblahblah'
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
			}
		},
		'/usr': {
			
		}
	},
	'__currentDir': '/'
};