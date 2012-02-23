
/*
 * Globals
 */

var globalEnvironment = [];
var globalFrame = {};
var processes = [];

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

function evaluate(command) {
	return lispEval(parse(command), globalEnvironment);
}

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
	} else {
		return sexp;
	}
}

// Environments
// An environment is a list of frames (frame = associative array)

function extendEnvironment(vars, vals, baseEnv) {
	console.log('extending environment: ' + vals + ' => ' + vars);
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
	console.log('Evaluating: ' + exp);
	
	// Detectors
	function isSelfEvaluating(exp) { 
		return typeof exp == 'number';
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
		return isTaggedList(exp, 'lambda') ||
			isTaggedList(exp, 'λ');
	}
	function isApplication(exp) {
		return typeof exp == 'object' && exp.length >= 1;
	}
	
	// Parsers
	function cdr(exp) {
		return exp.slice(1);
	}
	function textOfQuotation(exp) {
		if (typeof exp == 'string') {
			return parse(cdr(exp));
		} else {
			return cdr(exp);
		}
	}
	function listOfValues(exps, env) {
		return exps.map(function (exp) {
			return lispEval(exp, env);
		})
	}
	
	// Evaluators
	function evalAssignment(exp, env) {
		setVariableValue(exp[1],
			lispEval(exp[2], env),
			env);
	}
	function evalDefinition(exp, env) {
		defineVariable(exp[1],
			lispEval(exp[2], env),
			env);
	}
	function evalIf(exp, env) {
		if (eval(exp[1], env) == true) {
			return lispEval(exp[2], env);
		} else {
			return exp[3] !== undefined ? eval(exp[3], env) : false;
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
	} else if (isQuoted(exp)) {
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
	} else if (isApplication(exp)) {
		return lispApply(lispEval(exp[0], env), listOfValues(cdr(exp), env));
	} else {
		throw "Eval Error: Unknown expression type: " + exp;
	}
}

function lispApply(procedure, arguments) {
	console.log('Applying: ' + procedure + ' to ' + arguments);
	
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
		console.log("compound procedure");
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
	
	// Hooks to underlying environment
	'inspect-primitive': function(args) {
		if (args.length == 1 && args[0].primitive !== undefined) {
			return args[0].body;
		} else {
			throw 'js-inspect Error: JavaScript function required, but got ' + args;
		}
	},
	'environment': function() {
		var variables = [];
		for (var i = 0; i < globalEnvironment.length; i++) {
			for (var variable in globalEnvironment[i]) {
				variables.push(variable);
			}
		}
		variables.sort();
		return variables;
	}
};