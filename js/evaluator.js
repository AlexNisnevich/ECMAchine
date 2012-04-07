
/*
 * Globals
 */

var globalEnvironment = [];
var globalFrame = {};

/*
 * (used by both eval and apply)
 */
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
	Display.echo('Launching LISP evaluator ...');
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
	Processes.setCurrentPID(pid);
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
    var openQuote = false;
 		var tokens = [];
		var currentToken = '';
    for (var i = 0; i < sexp.length; i++) {
    	if (sexp[i] == ' ' && openParens == 0 && !openQuote) {
    		tokens.push(currentToken);
        currentToken = '';
    	} else {
    		currentToken += sexp[i];
    		if (sexp[i] == '(') {
    			openParens++;
    		} else if (sexp[i] == ')') {
    			openParens--;
    		} else if (sexp[i] == '"') {
    			openQuote = !openQuote;
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

/*
 * Environments
 * An environment is a list of frames (frame = associative array)
 */

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
 
/*
 * Evaluates an expression in the given environment
 */
function lispEval(exp, env) {
	// console.log('Evaluating: ' + exp + ' (Process ' + Processes.currentPID + ')');

	Processes.incrementEvals();
	
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
	function isString(exp) {
		return typeof exp == 'string' && exp[0] == '"' && exp.slice(-1) == '"';
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
			var quotedElt = parse(cdr(exp));
		} else {
			var quotedElt = cdr(exp)[0];
		}
		
		if (quotedElt.isList && quotedElt.length == 3 && quotedElt[1] == '.') {
			// quoted pair
			return new Pair(quotedElt[0], quotedElt[2]);
		} else {
			// quoted list or string literal
			return quotedElt;
		}
	}
	function getString(exp) {
		return constructString(exp.slice(1,-1));
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
		var paramsList, argsParameter;
		if (!parameters.isList) {
			// (lambda x {body})
			paramsList = [];
			argsParameter = parameters;
		} else {
			var dotIndex = parameters.lastIndexOf('.');
			if (dotIndex != -1) {
				// (lambda (a b . c) {body})
				paramsList = parameters.slice(0, dotIndex);
				argsParameter = parameters[dotIndex + 1];
			} else {
				// (lambda (a b) {body})
				paramsList = parameters;
				argsParameter = false;
			}
		}

		return {
			'lambda': true,
			'parameters': paramsList,
			'argsParameter': argsParameter,
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
	} else if (isQuoted(exp)) {
		return textOfQuotation(exp);
	} else if (isString(exp)) {
		return getString(exp);
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

/*
 * Applies a procedure to a given set of arguments
 */
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
		var params = clone(procedure.parameters);
		var args = arguments;
		
		if (procedure.argsParameter !== false) {
			// there is an args parameter - assign remaining arguments to it
			params.push(procedure.argsParameter);
			args = args.slice(0, procedure.parameters.length);
			args.push(arguments.slice(procedure.parameters.length, arguments.length))
		}
		var newEnvironment = extendEnvironment(params, args, procedure.environment);
		return evalSequence(procedure.body, newEnvironment);
	} else {
		throw "Apply Error: Unknown procedure type: " + procedure;
	}
}