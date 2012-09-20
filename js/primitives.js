/*
 * Primitive procedure definitions
 */

var primitiveProcedures = {
	// Hooks to underlying environment
	'environment': function() {
		var variables = [];
		for (var i = 0; i < globalEnvironment.length; i++) {
			for (var variable in globalEnvironment[i]) {
				variables.push(variable);
			}
		}
		variables.sort();
		return variables;
	},
	'inspect-primitive': function(args) {
		if (args.length == 1 && args[0].primitive !== undefined) {
			return args[0].body;
		} else {
			throw 'js-inspect Error: JavaScript function required, but got ' + args;
		}
	},
	'js-apply': function(args) {
		function prepareArg(arg, isObj) {
			if (typeof arg == 'string' || arg.isString) {
				return arg.toEvalString();
			} else if (typeof arg == 'object') {
				arg = arg.map(function (elt) {
					return prepareArg(elt);
				}).join(',');
				if (isObj) {
					arg = '[' + arg + ']';
				}
				return arg;
			} else {
				return arg;
			}
		}

		var jsFunc = args[0];
		if (args.length == 2) {
			var jsArgs = prepareArg(args[1]);
		} else if (args.length == 3) {
			var jsObj = prepareArg(args[1], true);
			jsFunc = jsObj + '.' + jsFunc;
			var jsArgs = prepareArg(args[2]);
		} else {
			throw 'js-apply error: Expected 2 or 3 arguments, but got ' + args.length;
		}

		return eval(jsFunc + '(' + jsArgs + ')');
	},

	// Arithmetic
	'+': function(args) {
		var string = false;
		args = args.map(function (arg) {
			if (arg.isString) { // string concatenation
				string = true;
				return arg.toEvalString();
			} else if (typeof arg == 'string') { // quoted literal concatenation
				return arg.toEvalString();
			} else {
				return arg;
			}
		});

		result = eval(args.join('+'));
		if (string) {
			result = constructString(result);
		}
		return result;
	},
	'-': function(args) {
		if (args.length == 1) {
			return (- args[0]);
		} else {
			return (args[0] - args[1]);
		}
	},
	'*': function(args) {
		return eval(args.join('*'));
	},
	'/': function(args) {
		if (args.length == 1) {
			return (1 / args[0]);
		} else {
			return (args[0] / args[1]);
		}
	},

	// Comparisons
	'=': function (args) {
		if (args[0].isString && args[1].isString) {
			return (args[0].toString() == args[1].toString());
		} else {
			return (args[0] == args[1]);
		}
	},
	'!=': function (args) {return (args[0] != args[1])},
	'>': function (args) {return (args[0] > args[1])},
	'<': function (args) {return (args[0] < args[1])},
	'>=': function (args) {return (args[0] >= args[1])},
	'<=': function (args) {return (args[0] <= args[1])},

	// Logical
	'not': function (args) {return !(args[0]);},
	'and': function (args) {return eval(args.join('&&'));},
	'or': function (args) {return eval(args.join('||'));},

	// List operations
	'cons': function (args) {
		if (args[1].isList) {
			// (1 (2 3)) => (1 2 3), since we represent everything as lists (not pairs)
			// in the underlying environment
			newList = clone(args[1]);
			newList.unshift(args[0]);
			return newList;
		} else {
			return new Pair(args[0], args[1]);
		}
	},
	'car': function (args) {
		var arg = args[0];
		return arg.car();
	},
	'cdr': function (args) {
		var arg = args[0];
		return arg.cdr();
	},
	'list': function (args) {
		return args;
	},
	'length': function (args) {
		return args[0].length;
	},

	// Misc Lisp
	'do-nothing': function () {
		return;
	},
	'newline': function () {
		return '\n';
	},
	'display': function (args) {
		Display.echo(args[0]);
		return;
	},
	'sort': function (args) {
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
	},

	// ECMAchine general
	'help': function () {
		return 'The following LISP commands are supported as primitives:' +
				'\n\t +, -, *, /, >, <, =, and, begin, car, cdr, cond, cons, define, if, lambda, length, let, let*, list, not, or, quote' +
			'\nThe following LISP commands are among those defined in the standard library (located in /startup):' +
				'\n\t abs, cadr, filter, map, null?, sum' +
			'\nEnvironment commands:' +
				'\n\t (environment)              Lists the currently bound variables' +
				'\n\t (inspect-primitive [[i;;]func])   Shows the JavaScript code of a primitive function' +
				'\n\t (js-apply [[i;;]func] [[[i;;]obj]] [[i;;]args]) Executes a JavaScript function' +
			'\nFile system commands:' +
				'\n\t (ls)                       Lists the contents of the current directory' +
				'\n\t (cd [[i;;]path])                  Navigates to another directory' +
				'\n\t (path [[i;;]dir1 dir2] [...])     Constructs a path string [[i;;](e.g. dir1/dir2)] from a list of subdirectories' +
				'\n\t (read [[i;;]filepath])            Displays the contents of a file' +
				'\n\t (exec [[i;;]filepath])            Executes a LISP file' +
				'\n\t (mkdir [[i;;]name])               Creates a new directory' +
				'\n\t (new [[i;;]path])                 Creates a new file' +
				'\n\t (save [[i;;]path text])           Saves text to a file, replacing current contents if the file already exists' +
				'\n\t (appnd [[i;;]path text])          Appends text to an existing file' +
				'\n\t (mv [[i;;]oldpath newpath])       Moves a file or directory to a new location' +
				'\n\t (cp [[i;;]oldpath newpath])       Copies a file or directory to a new location' +
				'\n\t (rm [[i;;]path])                  Removes a file or directory' +
				'\n\t (file? [[i;;]path])               Returns whether there is a file at the given path' +
				'\n\t (dir? [[i;;]path])                Returns whether there is a directory at the given path' +
			'\nPower commands:' +
				'\n\t (shutdown)                 Saves the filesystem and closes ECMAchine' +
				'\n\t (restart)                  Saves the filesystem and restarts ECMAchine' +
				'\n\t (reset-to-default)         Resets the filesystem to default configuration and restarts ECMAchine' +
			'\nProcess commands:' +
				'\n\t (processes)                Lists the PIDs and filenames of the currently running processes' +
				'\n\t (start [[i;;]path interval])      Starts a LISP program from a file, with the specified refresh rate (in ms)' +
				'\n\t (peek [[i;;]pid])                 Shows the code for the process with the specified PID' +
				'\n\t (kill [[i;;]pid])                 Kills the process with the specified PID' +
			  '\n\t (performance [[i;;]pid])          Shows the performance of the process with the specified PID (in evals per sec)' +
			'\nMiscellaneous commands:' +
				'\n\t (time [[[i;;]format]])            Displays the current time' +
				'\n\t (overlay [[i;;]txt x y id])       Creates or refreshes an overlay with text at position [[i;;](x,y)] on the screen' +
				'\n\t (sort [[[i;;]lst]] [[[i;;]keyfunc]])     Sorts a list in ascending order, optionally using the specified key function' +
				'\n\t (newline)                  Returns a newline character' +
				'\n\t (do-nothing)               Dummy command' +
				'\n\t (help)                     Displays this help screen' +
			'\nFor more help go to https://github.com/AlexNisnevich/ECMAchine'
			;
	},
	'shutdown': function () {
		OS.saveState();
		OS.close();
	},
	'restart': function () {
		OS.saveState();
		OS.reboot();
	},
	'reset-to-default': function () {
		OS.deleteSavedState();
		OS.reboot();
	},
	'time': function (args) {
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
	},

	// Filesystem
	'ls': function (args) {
		return Filesystem.listFiles(args[0]);
	},
	'cd': function (args) {
		var newPath = Filesystem.navigate(args[0]);
		Display.terminal.set_prompt('ecmachine:' + newPath + ' guest$');
		return;
	},
	'read': function (args) {
		return Filesystem.readFile(args[0]);
	},
	'exec': function (args) {
		var contents = Filesystem.readFile(args[0]);
		return evaluate(contents);
	},
	'mkdir': function (args) {
		var path = Filesystem.makeDir(args[0]);
		return new Array('Directory ' + path + ' created');
	},
	'new': function (args) {
		var path = Filesystem.newFile(args[0]);
		return new Array('File ' + path + ' created');
	},
	'save': function (args) {
		var path = Filesystem.saveFile(args[0], args[1]);
		return new Array('Saved file ' + path);
	},
	'appnd': function (args) {
		var contents = Filesystem.readFile(args[0]);
		var newContents = contents ? (contents + '\n' + args[1]) : args[1];
		var path = Filesystem.saveFile(args[0], newContents);
		return new Array('Updated file ' + path);
	},
	'mv': function (args) {
		var paths = Filesystem.copyItem(args[0], args[1]);
		Filesystem.removeItem(args[0]);
		return new Array('Moved ' + paths.oldPath + ' to ' + paths.newPath);
	},
	'cp': function (args) {
		var paths = Filesystem.copyItem(args[0], args[1]);
		return new Array('Copied ' + paths.oldPath + ' to ' + paths.newPath);
	},
	'rm': function (args) {
		var path = Filesystem.removeItem(args[0]);
		return new Array('Removed ' + path);
	},
	'file?': function (args) {
		var file = Filesystem.getFile(args[0]);
		return (file !== undefined && file.type == 'file');
	},
	'dir?': function (args) {
		var folderPath = Filesystem.calculatePath(args[0]);
		return (Filesystem.getDir(folderPath) !== undefined);
	},
	'path': function (args) {
		return args.join('/').replace('//','/');
	},

	// Processes
	'processes': function (args) {
		return Processes.listProcesses().map(function (proc) {
			return new Array(proc.pid, proc.name);
		});
	},
	'start': function (args) {
		var contents = Filesystem.readFile(args[0]);
		var pid = Processes.startProcess(args[0], contents, args[1]);
		Display.echo(new Array('Starting process at ' + args[0] + ' with PID ' + pid));
		return evaluate(contents, pid);
	},
	'peek': function (args) {
		var process = Processes.getProcessByID(args[0]);
		if (process.isTerminal) {
			return '#<Terminal>';
		} else {
			return process.code;
		}
	},
	'performance': function (args) {
		return Processes.getPerformance(args[0]);
	},
	'kill': function (args) {
		return Processes.killProcess(args[0]);
	},
	'overlay': function (args) {
		// (overlay txt x y id)
		var name = args[3];
		var txt = args[0].toString().replace(/ /g, '&nbsp;').replace(/\n/g, '<br />');
		var x = args[1], y = args[2];

		$('#overlays #' + name).remove(); // remove existing overlay w/ same id, if any
		var overlay = $('<div>').attr('id', name).appendTo('#overlays');
		overlay.html(txt);
		if (x >= 0) {
			overlay.addClass('overlayLeft');
			overlay.css('left', x);
		} else {
			overlay.addClass('overlayRight');
			overlay.css('right', -x);
		}
		if (y >= 0) {
			overlay.css('top', y);
		} else {
			overlay.css('bottom', -y);
		}

		Processes.registerOverlay(name); // if called from process, attach overlay name to PID
		return;
	},
	'clear-overlay': function (args) {
		$('#overlays #' + args[0]).remove();
	},

	// experimental

	'ajax': function (args) {
		var url = 'lib/ba-simple-proxy.php?url=' + args[0];

		var data_arr = args[1];
		var data = data_arr.map(function(elt) {
			return elt.car() + '=' + elt.cdr().toString();
		}).join('&'); // send data as string rather than object, so that it's not preprocessed

		var callback = args[2];

		$.post(url, data, function (result) {
			var contents = result.contents.split('\r\n\r\n')[1];
			Display.echo(lispApply(callback, new Array(contents)));
		});
		return;
	},

	'$': function (args) {
		// preprocess args
		args = args.map(function(arg) {
			if (arg.isString) {
				return arg.toString();
			} else {
				return arg;
			}
		})

		// prepare function
		var func = null;
		if (args.length > 1) {
			var func = args[0];
			args = args.slice(1);
		}
		console.log(func);
		console.log(args);

		// run function
		if (func) {
			var result = $[func].apply(this, args);
		} else {
			var result = $.apply(this, args);
		}
		console.log(result);

		// process result
		if (typeof result == 'object') {
			result = $.makeArray(result);
		}

		return result;
	}
};
