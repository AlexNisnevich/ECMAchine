/*
 * Modify toString behavior of lists to Lisp style
 */
Array.prototype.toString = function() {
	return '(' + this.join(' ') + ')';
};

/*
 * Parses S-expression into a nested list
 */
function parse(sexp) {
	var isInt = function(value) {
	  if ((parseFloat(value) == parseInt(value)) && !isNaN(value)){
	      return true;
	  } else { 
	      return false;
	  }
	};
	
	var tokenize = function(sexp) {
        var sexp = sexp.substring(1, sexp.length-1);
        var openParens = 0;
   		var tokens = [];
		var currentToken = '';                                        
        for (var i = 0; i < sexp.length; i++) {
        	if (sexp[i] == ' ' && openParens == 0) {
        		tokens.push(currentToken);
                currentToken = '';
        	} else {
        		currentToken += sexp[i];
        		if (sexp[i] == '(') {
        			openParens++;
        		} else if (sexp[i] == ')') {
        			openParens--;
        		}
        	}
        }
		tokens.push(currentToken);        
    	return tokens;
    };
    
    sexp = sexp.replace(/\s+/g, " "); // first, trim whitespace!
	
	// do ( and ) match?
	if (sexp.split('(').length != sexp.split(')').length) { 
		throw 'Error: Parentheses do not match'; 
	} 	
	
	if (sexp[0] == '(') {
		var parsed_sexp = [];
		var tokens = tokenize(sexp);
		for (var i = 0; i < tokens.length; i++) {
			parsed_sexp.push(parse(tokens[i]));
		}
		return parsed_sexp;
	} else if (isInt(sexp)) {
		return parseInt(sexp);
	} else {
		return sexp;
	}
}

/*
 * Evaluates a parsed S-expression, Lisp-style
 */
function evaluate(sexp, environment, term) {
	var fs = environment['__fileSystem'];
	var dir = environment['__currentDir'];
	var builtInFunctions = [
		'+', '-', '*', '/', '>', '<', '=', 'and', 'begin', 'car', 'cdr', 'cond', 'cons', 
			'define', 'if', 'lambda', 'length', 'list', 'map', 'not', 'or', 'quote', 'filter',
		'ls', 'cd', 'read', 'exec', 'mkdir', 'new', 'save', 'help', 'append', 'path', 
			'rm', 'mv', 'cp', 'file?', 'dir?', 'time', 'do-nothing',
		'processes', 'start', 'peek', 'kill', 'overlay'
	];
	var controlFlowStatements = ['if', 'cond', 'quote', 'begin', 'define', 'lambda', 'map', 'filter', 'start'];
	
	if (typeof sexp != 'object') { // atom
		if (sexp == '#t') {
			return true;
		} else if (sexp == '#f') {
			return false;
		} else if (typeof sexp == 'number') { // number
			return sexp;
		} else if (sexp[0] == "'") { // string literal
			return sexp.slice(1);
		} else if (builtInFunctions.indexOf(sexp) > -1) { // built-in function
			return sexp;
		} else { // variable
			return environment[sexp];
		}
	}
	
	var func = sexp[0];
	
	if (func.lambda || controlFlowStatements.indexOf(func) > -1) {
		// lambda or control flow statement: don't evaluate arguments
		var args = sexp.slice(1);
	} else {
		// evaluate arguments
		var args = [];
		for (var i = 1; i < sexp.length; i++) {
			var evaluatedArg = evaluate(sexp[i], environment, term);
			if (evaluatedArg !== undefined) {
				args.push(evaluatedArg);
			} else {
				throw 'Error: Cannot evaluate token "' + sexp[i] + '"';
			}
		}
	}
	
	if (func.lambda) {
		// Lambda function
		
		lambdaEnvironment = func.environment;
		if (func.arguments.length > args.length) {
			throw 'Error: Not enough arguments passed to lambda: expected ' + func.arguments + ' but received ' + args;
			return 'Error';
		}
		for (var i = 0; i < func.arguments.length; i++) {
			lambdaEnvironment[func.arguments[i]] = evaluate(args[i], environment, term);
		}
		return evaluate(func.body, lambdaEnvironment);
	} else if (typeof func == 'string') {
		// Built-in function
		
		switch(func) {
			// Arithmetic
			case '+':
			case '-':
			case '*':
			case '/':
				args = args.map(function (arg) {
					if (typeof arg == 'string') { // this lets us overload + for string concatenation
						return '"' + arg + '"';
					} else {
						return arg;
					}
				})
				return eval(args.join(func));
				
			// Comparisons
			case '=':
				return (args[0] == args[1]);
			case '>':
			case '<':
			case '>=':
			case '<=':
			case '==':
			case '!=':
				return eval(args[0] + func + args[1]);
				
			// Logical
			case 'not':
				return !(args[0]);
			case 'and':
				return eval(args.join('&&'));
			case 'or':
				return eval(args.join('||'));
				
			// Conditionals
			case 'if':
				if (evaluate(args[0], environment, term)) {
					return evaluate(args[1], environment, term);
				} else {
					return evaluate(args[2], environment, term);
				}
			case 'cond':
				for (var i = 0; i < args.length; i++) {
					var condBlock = args[i];
					if (evaluate(condBlock[0], environment, term)) {
						return evaluate(condBlock[1], environment, term);
					}
				}
			
			// List operations
			case 'cons':
				return new Array(args[0], args[1]);
			case 'car':
				var arg = args[0];
				return arg[0];
			case 'cdr':
				var arg = args[0];
				return arg.slice(1);
			case 'list':
				return args;
			
			// Lambdas and scoping stuff
			case 'define':
				environment[args[0]] = args[1];
				break;
			case 'lambda':
				return {
					'lambda': true,
					'arguments': args[0],
					'body': args[1],
					'environment': environment
				}
			
			// Misc Lisp
			case 'quote':
				return args[0];
			case 'begin':
				for (var i = 0; i < args.length - 1; i++) {
					evaluate(args[i], environment, term);
				}
				return evaluate(args[args.length - 1], environment, term);
			case 'length':
				return args[0].length;
			
			// Higher-order functions
			case 'map':
				var fn = evaluate(args[0], environment, term);
				var lst = evaluate(args[1], environment, term);
				return lst.map(function (elt) {
					if (typeof elt == 'string' && elt[0] != "'") {
						elt = "'" + elt; // make sure string literals are quoted
					}
					try {
						return evaluate(new Array(fn, elt), environment, term);
					} catch (err) {
						return "'" + new Array(err);
					}
				});
			case 'filter':
				var cond = evaluate(args[0], environment, term);
				var lst = evaluate(args[1], environment, term);
				return lst.filter(function (elt) {
					if (typeof elt == 'string' && elt[0] != "'") {
						elt = "'" + elt; // make sure string literals are quoted
					}
					try {
						return evaluate(new Array(cond, elt), environment, term);
					} catch (err) {
						false;
					}
				});
			
			// Filesystem
			case 'ls':
				var fileNames = [];
				for (fname in fs[dir]) {
					fileNames.push(fname);
				}
				return fileNames;
			case 'cd':
				if (args[0][0] == "'") { args[0] = args[0].slice(1); } // remove initial quote if exists 
				var newPath = calculatePath(dir, args[0]);
				if (fs[newPath] === undefined) {
					throw 'Error: path "' + newPath + '" does not exist';
				}
				environment['__currentDir'] = newPath;
				term.set_prompt('ecmachine:' + newPath + ' guest$');
				return;
			case 'read':
				if (args[0][0] == "'") { args[0] = args[0].slice(1); } // remove initial quote if exists 
				var file = fs[dir][args[0]];
				if (file === undefined) {
					throw 'Error: file "' + args[0] + '" does not exist';
				} else if (file.type == 'dir') {
					throw 'Error: "' + args[0] + '" is a directory';
				}
				var contents = file.contents;
				return contents;
			case 'exec':
				if (args[0][0] == "'") { args[0] = args[0].slice(1); } // remove initial quote if exists 
				var file = fs[dir][args[0]];
				if (file === undefined) {
					throw 'Error: file "' + args[0] + '" does not exist';
				} else if (file.type == 'dir') {
					throw 'Error: "' + args[0] + '" is a directory';
				}
				var contents = parse(file.contents);
				return evaluate(contents, globalEnvironment, term);
			case 'mkdir':
				if (args[0][0] == "'") { args[0] = args[0].slice(1); } // remove initial quote if exists 
				var newDir = fs[dir][args[0]];
				var newDirPath = calculatePath(dir, args[0]);
				if (newDir !== undefined) {
					throw 'Error: "' + args[0] + '" already exists';
				}
				environment['__fileSystem'][dir][args[0]] = { 'type': 'dir' };
				environment['__fileSystem'][newDirPath] = {};
				return;
			case 'new':
				if (args[0][0] == "'") { args[0] = args[0].slice(1); } // remove initial quote if exists 
				var newFile = fs[dir][args[0]];
				if (newFile !== undefined) {
					throw 'Error: "' + args[0] + '" already exists';
				}
				var newDirPath = calculatePath(dir, args[0]);
				environment['__fileSystem'][dir][args[0]] = { 'type': 'file', 'contents': '' };
				return;
			case 'save':
				if (args[0][0] == "'") { args[0] = args[0].slice(1); } // remove initial quote if exists 
				var file = fs[dir][args[0]];
				if (file !== undefined && file.type == 'dir') {
					throw 'Error: "' + args[0] + '" is a directory';
				}
				environment['__fileSystem'][dir][args[0]] = { 'type': 'file', 'contents': args[1] };
				return;
			case 'append':
				if (args[0][0] == "'") { args[0] = args[0].slice(1); } // remove initial quote if exists 
				var file = fs[dir][args[0]];
				if (file === undefined) {
					throw 'Error: file "' + args[0] + '" does not exist';
				}
				environment['__fileSystem'][dir][args[0]].contents += (file.contents != '' ? '\n' : '') + args[1];
				return;
			case 'mv':
				if (args[0][0] == "'") { args[0] = args[0].slice(1); } // remove initial quote if exists
				var file = fs[dir][args[0]];
				var pathSplit = args[1].split('/');
				var newPath = calculatePath(dir, args[1]);
				var newName = pathSplit[pathSplit.length - 1];
				var newFolderPath = calculatePath(dir, pathSplit.slice(0, pathSplit.length - 1).join('/'));
				if (file === undefined) {
					throw 'Error: file/directory "' + args[0] + '" does not exist';
				} else if (fs[newFolderPath] === undefined) {
					throw 'Error: the path "' + newFolderPath + '" does not exist';
				} else {
					if (file.type == 'dir') {
						var oldPath = calculatePath(dir, args[0]);
						environment['__fileSystem'][newPath] = fs[oldPath];
						delete environment['__fileSystem'][oldPath];
					} else {
						var contents = file.contents;
						delete environment['__fileSystem'][dir][args[0]];
						environment['__fileSystem'][newFolderPath][newName] = {
							'type': 'file',
							'contents': contents
						}
					}
				}
				return;
			case 'cp':
				if (args[0][0] == "'") { args[0] = args[0].slice(1); } // remove initial quote if exists 
				var file = fs[dir][args[0]];
				var pathSplit = args[1].split('/');
				var newPath = calculatePath(dir, args[1]);
				var newName = pathSplit[pathSplit.length - 1];
				var newFolderPath = calculatePath(dir, pathSplit.slice(0, pathSplit.length - 1).join('/'));
				if (file === undefined) {
					throw 'Error: file/directory "' + args[0] + '" does not exist';
				} else if (fs[newFolderPath] === undefined) {
					throw 'Error: the path "' + newFolderPath + '" does not exist';
				} else {
					if (file.type == 'dir') {
						var oldPath = calculatePath(dir, args[0]);
						environment['__fileSystem'][newPath] = fs[oldPath];
					} else {
						var contents = file.contents;
						environment['__fileSystem'][newFolderPath][newName] = {
							'type': 'file',
							'contents': contents
						}
					}
				}
				return;
			case 'rm':
				if (args[0][0] == "'") { args[0] = args[0].slice(1); } // remove initial quote if exists
				var file = fs[dir][args[0]];
				if (file === undefined) {
					throw 'Error: file/directory "' + args[0] + '" does not exist';
				} else {
					if (file.type == 'dir') {
						var dirPath = calculatePath(dir, args[0]);
						delete environment['__fileSystem'][dirPath];
					}
					delete environment['__fileSystem'][dir][args[0]];
				}
				return;
			case 'file?':
				var pathSplit = args[0].split('/');
				var fileName = pathSplit[pathSplit.length - 1];
				var folderPath = calculatePath(dir, pathSplit.slice(0, pathSplit.length - 1).join('/'));
				return (fs[folderPath][fileName] !== undefined && fs[folderPath][fileName].type == 'file');
			case 'dir?':
				var folderPath = calculatePath(dir, args[0]);
				return (fs[folderPath] !== undefined);
			
			// Misc ECMAchine commands
			case 'help':
				return 'The following LISP commands are supported:' + 
						'\n\t +, -, *, /, >, <, =, and, begin, car, cdr, cond, cons, define, filter, if, lambda, length, list, map, not, or, quote' + 
					'\nThe following file-system commands are supported:' +
						'\n\t (ls)                   Lists the contents of the current directory' +
						'\n\t (cd [[i;;]path])              Navigates to another directory' +
						'\n\t (path [[i;;]dir1 dir2 ...])   Constructs a path string [[i;;](e.g. dir1/dir2)] from a list of subdirectories' +
						'\n\t (read [[i;;]filename])        Displays the contents of a file' +
						'\n\t (exec [[i;;]filename])        Executes a LISP file' +
						'\n\t (mkdir [[i;;]name])           Creates a new directory' +
						'\n\t (new [[i;;]name])             Creates a new file' +
						'\n\t (save [[i;;]name text])       Saves text to a file, replacing current contents if the file already exists' +
						'\n\t (append [[i;;]name text])     Appends text to an existing file' +
						'\n\t (mv [[i;;]filename newpath])  Moves a file or directory to a new location' +
						'\n\t (cp [[i;;]filename newpath])  Copies a file or directory to a new location' +
						'\n\t (rm [[i;;]filename])          Removes a file or directory' +
						'\n\t (file? [[i;;]path])           Returns whether there is a file at the given path' +
						'\n\t (dir? [[i;;]path])            Returns whether there is a directory at the given path' +
						'\n\t (time)                 Displays the current time' + 
						'\n\t (do-nothing)			 Dummy command' +
						'\n\t (help)                 Displays this help screen' +
					'\nThe following commands for dealing with processes are supported:' +
						'\n\t (processes)            Lists the PIDs and filenames of the currently running processes' +
						'\n\t (start [[i;;]name interval])  Starts a LISP program from a file, with the specified refresh rate (in ms)' +
						'\n\t (peek [[i;;]pid])             Shows the code for the process with the specified PID' +
						'\n\t (kill [[i;;]pid])             Kills the process with the specified PID' +
						'\n\t (overlay [[i;;]txt x y id])   Creates or refreshes an overlay with text at position [[i;;](x,y)] on the screen'
						;
			case 'path':
				return args.join('/');
			case 'time':
				var date = new Date();
				return new Array(date.getHours(), date.getMinutes(), date.getSeconds());
			case 'do-nothing':
				return;
			
			// For processes
			case 'processes':
				var procs = [];
				for (var pid = 0; pid < processes.length; pid++) {
					if (!processes[pid].terminated) {
						procs.push(new Array(pid, processes[pid].name));
					}
				}
				return procs;
			case 'start':
				// get program
				if (args[0][0] == "'") { args[0] = args[0].slice(1); } // remove initial quote if exists 
				var file = fs[dir][args[0]];
				if (file === undefined) {
					throw 'Error: file "' + args[0] + '" does not exist';
				} else if (file.type == 'dir') {
					throw 'Error: "' + args[0] + '" is a directory';
				}
				var contents = parse(file.contents);
				
				// start interval
				var interval = setInterval(function (term) {
					var result = evaluate(contents, globalEnvironment, term);
					if (result !== undefined) {
						term.echo(result);
						$(document).scrollTop($(document).height());
					}
				}, evaluate(args[1], environment, term), term);
				
				// add to process list
				processes.push({
					'name': args[0],
					'process': interval,
					'code': file.contents,
					'terminated': false
				});
				
				return;
			case 'peek':
				if (processes[args[0]] === undefined || processes[args[0]].terminated) {
					throw 'There is no process with PID ' + args[0];
				}
				return processes[args[0]].code;
			case 'kill':
				if (processes[args[0]] === undefined || processes[args[0]].terminated) {
					throw 'There is no process with PID ' + args[0];
				}
				clearInterval(processes[args[0]].process);
				processes[args[0]].terminated = true;
				return new Array('Process with PID ' + args[0] + ' (' + processes[args[0]].name + ') terminated');
			case 'overlay': // (overlay txt x y id)
				var name = args[3], txt = args[0], x = args[1], y = args[2];
				$('#overlays #' + name).remove(); // remove existing overlay w/ same id, if any
				var overlay = $('<div>').attr('id', name).appendTo('#overlays');
				overlay.text('' + txt);
				if (x >= 0) {
					overlay.css('left', x);
				} else {
					overlay.css('right', -x);
				}
				if (y >= 0) {
					overlay.css('top', y);
				} else {
					overlay.css('bottom', -y);
				}
				return;
			
			// Not a built-in function: find function in environment and evaluate
			default:
				sexp[0] = evaluate(environment[func], environment, term);
				return evaluate(sexp, environment, term);
		}
	} else {
		// Evaluate this function
		sexp[0] = evaluate(func, environment, term);
		return evaluate(sexp, environment, term);
	}
}

/*
 * Gets new path (e.g. for 'cd' command)
 */
function calculatePath(currentPath, dir) {
	if (dir == '/') {
		return '/';
	} else {
		var pathComponents = currentPath.split('/');
		var dirComponents = dir.split('/');
		dirComponents.forEach(function (comp) {
			if (comp == '..') {
				pathComponents.pop();
			} else {
				pathComponents.push(comp);
			}
		});
		var newPath = pathComponents.join('/').replace('//','/');
		return (newPath != '') ? newPath : '/';
	}
}
