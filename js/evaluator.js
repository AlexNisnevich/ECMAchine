
/*
 * Globals
 */

var globalEnvironment = [];
var globalFrame = {};

var processes = [];
var currentPID = null;
var terminalProcess = {
	// Performance
	'timeStarted': new Date().getTime(),
	'timeElapsed': function () { return ((new Date().getTime()) - this.timeStarted); },
	'evals': 0
};

/*
 * Modify toString behavior of lists to Lisp style
 */
Array.prototype.toString = function() {
	var sexp = '(' + this.join(' ') + ')';
	return sexp;
};
Array.prototype.toStringNoOuterBraces = function() {
	var sexp = this.join(' ');
	return sexp;
};
Array.prototype.isList = true;

function clone(obj){
    if(obj == null || typeof(obj) != 'object')
        return obj;
    var temp = new obj.constructor(); 
    for(var key in obj)
        temp[key] = clone(obj[key]);
    return temp;
}

// used by both eval and apply
function evalSequence(exps, env) {
	for (var i = 0; i < exps.length - 1; i++) {
		lispEval(exps[i], env);
	}
	return lispEval(exps[exps.length - 1], env);
}

/*
 * Evaluator
 */

function initEvaluator() {
	terminalEcho('Launching LISP evaluator ...');
  for (var primitive in primitiveProcedures) {
      globalFrame[primitive] = {
          'primitive': true,
          'name': primitive,
          'body': primitiveProcedures[primitive],
          toString: function () {
          	return "#<Function " + this.name + ">";
          }
      };
  }
  globalEnvironment.push(globalFrame);
}

function evaluate(command, pid) {
	if (pid !== undefined) {
		currentPID = pid;
	} else {
		currentPID = null;
	}
	command = '(begin ' + command + ')';
	return lispEval(parse(command), globalEnvironment);
}

/*
 * Parses S-expression into a nested list
 */
function parse(sexp) {
	// console.log("Parsing " + sexp);
	
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
    
  // do ( and ) match?
	if (sexp.split('(').length != sexp.split(')').length) { 
		throw 'Parse Error: Parentheses do not match'; 
	}
    
  // trim
  sexp = sexp.replace(/\n\.\./g, " ") // "\n.." => " "
  sexp = sexp.replace(/\s+/g, " "); // eg "  " => " "
  sexp = sexp.replace(/\s+\)/g, ")"); // eg " )" => ")"
  sexp = sexp.replace(/\(\s+/g, "("); // eg "( " => "("
    
	if (sexp == '()') {
		return [];
	} else if (sexp[0] == '(') {
		var parsed_sexp = [];
		var tokens = tokenize(sexp);
		for (var i = 0; i < tokens.length; i++) {
			parsed_sexp.push(parse(tokens[i]));
		}
		return parsed_sexp;
	} else if (isInt(sexp)) {
		return parseInt(sexp);
	} else if (!isNaN(sexp)) {
		return parseFloat(sexp);
	} else {
		return sexp;
	}
}

// Environments
// An environment is a list of frames (frame = associative array)

function extendEnvironment(vars, vals, baseEnv) {
	if (vars.length == vals.length) {
		var newFrame = {};
		for (var i = 0; i < vars.length; i++) {
			newFrame[vars[i]] = vals[i];
		}
		
		var newEnv = baseEnv.slice(0);
		newEnv.unshift(newFrame);
		return newEnv;
	} else {
		throw 'Apply Error: Incorrect number of parameters: expecting ' + vars + ' but received ' + vals;
	}
}

function lookupVariableValue(variable, env) {
	for (var i = 0; i < env.length; i++) {
		var frame = env[i];
		if (frame[variable] !== undefined) {
			return frame[variable];
		}
	}
	throw 'Eval Error: Unbound variable ' + variable;
}

function assignVariableValue(variable, val, env) {
	for (var i = 0; i < env.length; i++) {
		var frame = env[i];
		if (frame[variable] !== undefined) {
			frame[variable] = val;
			return;
		}
	}
	throw 'Eval Error: Unbound variable ' + variable;
}

function defineVariable(variable, val, env) {
	var frame = env[0];
	frame[variable] = val;
	return;
}

// Here come the eval and apply!

function lispEval(exp, env) {
	// console.log('Evaluating: ' + exp + ' (Process ' + currentPID + ')');

	if (currentPID !== null) {
		var process = processes[currentPID];
	} else {
		var process = terminalProcess;
	}
	process.evals++;
	
	// Detectors
	function isSelfEvaluating(exp) { 
		return typeof exp == 'number';
	}
	function isBoolean(exp) { 
		return exp == 'true' || exp == '#t' || exp == 'false' || exp == '#f';
	}
	function isVariable(exp) {
		return typeof exp == 'string';
	}
	function isTaggedList(exp, tag) {
		return typeof exp == 'object' && exp[0] && exp[0] == tag;
	}
	function isQuoted(exp) {
		return (typeof exp == 'string' && exp[0] == "'") || isTaggedList(exp, 'quote');
	}
	function isAssignment(exp) {
		return isTaggedList(exp, 'set!');
	}
	function isDefinition(exp) {
		return isTaggedList(exp, 'define')
	}
	function isIf(exp) {
		return isTaggedList(exp, 'if')
	}
	function isCond(exp) {
		return isTaggedList(exp, 'cond')
	}
	function isBegin(exp) {
		return isTaggedList(exp, 'begin')
	}
	function isLambda(exp) {
		return isTaggedList(exp, 'lambda') || isTaggedList(exp, 'λ');
	}
	function isLet(exp) {
		return isTaggedList(exp, 'let') || isTaggedList(exp, 'let*') || isTaggedList(exp, 'letrec');
	}
	function isApplication(exp) {
		return typeof exp == 'object' && exp.length >= 1;
	}
	
	// Parsers
	function cdr(exp) {
		return exp.slice(1);
	}
	function getBooleanValue(exp) {
		if (exp == '#t' || exp == 'true') {
			return true;
		} else {
			return false;
		}
	}
	function textOfQuotation(exp) {
		if (typeof exp == 'string') {
			return parse(cdr(exp));
		} else {
			return cdr(exp)[0];
		}
	}
	function listOfValues(exps, env) {
		return exps.map(function (exp) {
			return lispEval(exp, env);
		})
	}
	
	// Evaluators
	function evalAssignment(exp, env) {
		assignVariableValue(exp[1],
			lispEval(exp[2], env),
			env);
	}
	function evalDefinition(exp, env) {
		var name = exp[1]
		var value = exp[2]
		
		if (name.isList) {
			// (define (func a b) ...)
			defineVariable(name[0],
				makeProcedure(cdr(name), cdr(cdr(exp)), env),
				env)
		} else {
			// (define var ...)
			defineVariable(name,
				lispEval(value, env),
				env);
		}
	}
	function evalIf(exp, env) {
		if (lispEval(exp[1], env) == true) {
			return lispEval(exp[2], env);
		} else {
			return exp[3] !== undefined ? lispEval(exp[3], env) : false;
		}
	}
	function evalCond(exp, env) {
		var condBlocks = cdr(exp);
		for (var i = 0; i < condBlocks.length; i++) {
			var block = condBlocks[i];
			if (lispEval(block[0], env) == true) {
				return lispEval(block[1], env);
			}
		}
		return false;
	}
	function evalLet(exp, env) {
		if (exp[0] == 'let') {
			var vars = exp[1].map(function (x) {return x[0]});
			var vals = exp[1].map(function (x) {return lispEval(x[1], env)});
			return lispEval(exp[2], extendEnvironment(vars, vals, env));
		} else if (exp[0] == 'let*') {
			var newEnv = env;
			for (var i = 0; i < exp[1].length; i++) {
				var newVar = [exp[1][i][0]];
				var newVal = [lispEval(exp[1][i][1], newEnv)];
				newEnv = extendEnvironment(newVar, newVal, newEnv);
			}
			return lispEval(exp[2], newEnv);
		} else if (exp[0] == 'letrec') {
			throw "Eval Error: letrec control structure not yet implemented: " + exp; 
		}
	}
	function makeProcedure(parameters, body, environment) {
		var paramsList = []; for (var i = 0; i < arguments.length; i++) { paramsList.push(arguments[i]); } // a necessary evil

		return {
			'lambda': true,
			'parameters': parameters,
			'body': body,
			'environment': environment,
			toString: function () {
				return '(lambda ' + parameters + ' ' + body.toStringNoOuterBraces() + ')';
			}
		};
	}
	
	if (isSelfEvaluating(exp)) {
		return exp;
	} else if (isBoolean(exp)) {
		return getBooleanValue(exp);
	}else if (isQuoted(exp)) {
		return textOfQuotation(exp);
	} else if (isVariable(exp)) {
		return lookupVariableValue(exp, env)
	} else if (isAssignment(exp)) {
		return evalAssignment(exp, env);
	} else if (isDefinition(exp)) {
		return evalDefinition(exp, env);
	} else if (isIf(exp)) {
		return evalIf(exp, env);
	} else if (isCond(exp)) {
		return evalCond(exp, env);
	} else if (isBegin(exp)) {
		return evalSequence(cdr(exp), env);
	} else if (isLambda(exp)) {
		return makeProcedure(exp[1], cdr(cdr(exp)), env);
	} else if (isLet(exp)) {
		return evalLet(exp, env);
	} else if (isApplication(exp)) {
		return lispApply(lispEval(exp[0], env), listOfValues(cdr(exp), env));
	} else {
		throw "Eval Error: Unknown expression type: " + exp;
	}
}

function lispApply(procedure, arguments) {
	// console.log('Applying: ' + procedure + ' to ' + arguments);
	
	function isPrimitiveProcedure(procedure) {
		return procedure.primitive !== undefined;
		return primitiveProcedures.procedure !== undefined;
	}
	function isCompoundProcedure(procedure) {
		return procedure.lambda !== undefined;
	}
	function applyPrimitiveProcedure(procedure, args) {
		return procedure.body(args);
	}
	
	if (isPrimitiveProcedure(procedure)) {
		return applyPrimitiveProcedure(procedure, arguments);
	} else if (isCompoundProcedure(procedure)) {
		return evalSequence(procedure.body,
				extendEnvironment(procedure.parameters, arguments, procedure.environment));
	} else {
		throw "Apply Error: Unknown procedure type: " + procedure;
	}
}

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
			if (typeof arg == 'object') { 
				arg = arg.map(function (elt) {
					return prepareArg(elt);
				}).join(','); 
				if (isObj) {
					arg = '[' + arg + ']';
				}
			} else if (typeof arg == 'string') {
				arg = "'" + arg + "'";
			}
			return arg;
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
		args = args.map(function (arg) {
			if (typeof arg == 'string') { // this lets us overload + for string concatenation
				return '"' + arg + '"';
			} else {
				return arg;
			}
		})
		return eval(args.join('+'));
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
	'=': function (args) {return (args[0] == args[1])},
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
			return new Array(args[0], args[1]);
		}
	},
	'car': function (args) {
		var arg = args[0];
		return arg[0];
	},
	'cdr': function (args) {
		var arg = args[0];
		return arg.slice(1);
	},
	'list': function (args) {
		return args;
	},
	'length': function (args) {
		// Is length a primitive? It COULD be implemented recursively as
		// (λ (x) (if (null? x) 0 (+ 1 (length (cdr x)))))
		// but that requires null? to be defined (I choose to define null? in terms of length)
		// Paul Graham included length as a primitive, so I'll follow his precedent.
		return args[0].length;
	},
	
	// Misc Lisp
	'do-nothing': function () {
		return;
	},
	'newline': function () {
		return '\n';
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
	'help': function (args) {
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
				'\n\t (append [[i;;]path text])         Appends text to an existing file' +
				'\n\t (mv [[i;;]oldpath newpath])       Moves a file or directory to a new location' +
				'\n\t (cp [[i;;]oldpath newpath])       Copies a file or directory to a new location' +
				'\n\t (rm [[i;;]path])                  Removes a file or directory' +
				'\n\t (file? [[i;;]path])               Returns whether there is a file at the given path' +
				'\n\t (dir? [[i;;]path])                Returns whether there is a directory at the given path' +
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
				'\n\t (help)                     Displays this help screen'
			;
	},
	'time': function (args) {
		var date = new Date();
    if (args[0] == null)
    	return date.getTime();
    else 
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
	},
	
	// Filesystem
	'ls': function (args) {
		return Filesystem.listFiles(args[0]);
	},
	'cd': function (args) {
		var newPath = Filesystem.navigate(args[0]);
		terminal.set_prompt('ecmachine:' + newPath + ' guest$');
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
	'append': function (args) {
		var contents = Filesystem.readFile(args[0]);
		var newContents = contents ? (contents + '\n' + args[1]) : '';
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
		var procs = [[-1, 'Terminal']];
		for (var pid = 0; pid < processes.length; pid++) {
			if (!processes[pid].terminated) {
				procs.push(new Array(pid, processes[pid].name));
			}
		}
		return procs;
	},
	'start': function (args) {
		// get program
		var contents = Filesystem.readFile(args[0]);
		
		var pid = processes.length;
		
		// start interval
		var interval = setInterval(function () {
			var result = evaluate(contents, pid);
			if (result !== undefined) {
				terminalEcho(result);
			}
		}, args[1]);
		
		// add to process list
		processes.push({
			'name': Filesystem.getNameFromPath(args[0]),
			'process': interval,
			'code': contents,
			'terminated': false,
			'overlays': [],
			
			// Performance
			'timeStarted': new Date().getTime(),
			'timeElapsed': function () { return ((new Date().getTime()) - this.timeStarted); },
			'interval': args[1],
			'evals': 0
		});
		
		// and run it once right now
		terminalEcho(new Array('Starting process at ' + args[0] + ' with PID ' + pid));
		return evaluate(contents, pid);
	},
	'peek': function (args) {
		if (args[0] == -1) {
			return '#<Terminal>'
		} else if (processes[args[0]] === undefined || processes[args[0]].terminated) {
			throw 'There is no process with PID ' + args[0];
		}
		return processes[args[0]].code;
	},
	'performance': function (args) {
		if (args[0] == -1) {
			var proc = terminalProcess;
		} else {
			if (processes[args[0]] === undefined || processes[args[0]].terminated) {
				throw 'There is no process with PID ' + args[0];
			}
			var proc = processes[args[0]];
		}
		
		var evalsPerMS = proc.evals / (proc.timeElapsed());
		var evalsPerSec = Math.round(evalsPerMS * 1000000)/1000;
		return evalsPerSec;
	},
	'kill': function (args) {
		if (args[0] == -1) {
			throw 'Can\'t kill terminal';
		} if (processes[args[0]] === undefined || processes[args[0]].terminated) {
			throw 'There is no process with PID ' + args[0];
		}
		clearInterval(processes[args[0]].process);
		processes[args[0]].terminated = true;
		processes[args[0]].overlays.forEach(function (name) {$('#overlays #' + name).remove();}); // remove associated overlays
		return new Array('Process with PID ' + args[0] + ' [' + processes[args[0]].name + '] terminated');
	},
	'overlay': function (args) {
		// (overlay txt x y id)
		var name = args[3];
		var txt = args[0].toString().replace(/\n/g, '<br />');
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
		
		// if called from process, attach overlay name to PID
		if (currentPID != null && processes[currentPID].overlays.indexOf(name) < 0) {
			processes[currentPID].overlays.push(name);
		}
		
		return;
	}
};