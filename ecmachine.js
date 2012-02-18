
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
function evaluate(sexp, environment) {
	var controlFlowStatements = ['if', 'cond', 'quote', 'begin', 'define', 'lambda'];
	
	if (typeof sexp != 'object') { // atom
		if (sexp == '#t') {
			return true;
		} else if (sexp == '#f') {
			return false;
		} else if (typeof sexp == 'number') {
			return sexp;
		} else { // variable
			return environment[sexp];
		}
	}
	
	var func = sexp[0];
	
	if (controlFlowStatements.indexOf(func) > -1) {
		// don't evaluate arguments
		var args = sexp.slice(1);
	} else {
		// evaluate arguments
		var args = [];
		for (var i = 1; i < sexp.length; i++) {
			args.push(evaluate(sexp[i], environment));
		}
	}
	
	switch(func) {
		// Arithmetic
		case '+':
		case '-':
		case '*':
		case '/':
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
			if (evaluate(args[0], environment)) {
				return evaluate(args[1], environment);
			} else {
				return evaluate(args[2], environment);
			}
		case 'cond':
			for (var i = 0; i < args.length; i++) {
				var condBlock = args[i];
				if (evaluate(condBlock[0], environment)) {
					return evaluate(condBlock[1], environment);
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
		
		// lambdas and stuff
		case 'define':
			environment[args[0]] = args[1];
		
		// Misc
		case 'quote':
			return args[0];
		case 'begin':
			for (var i = 0; i < args.length - 1; i++) {
				evaluate(args[i], environment);
			}
			return evaluate(args[args.length - 1], environment);
		
		default:
			console.log('Unrecognized method: ' + func);
			return 'Error';
			break;
	}
}

// (+ (* 2 5) 3) => ['+', ['*', 2, 5], 3]
