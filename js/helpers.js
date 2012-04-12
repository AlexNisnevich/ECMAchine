/*
 * Clones an object
 */
function clone(obj){
  if (obj == null || typeof(obj) != 'object') {
  	return obj;
  } else if (obj.isString) {
  	return new constructString(obj.toString());
  } else {
  	var temp = new obj.constructor(); 
    for (var key in obj)
      temp[key] = clone(obj[key]);
    return temp;
  }
}

function prepareString(elt) {
	if (elt === undefined || elt === null) {
		return '#<undef>';
	} else if (elt.isString) {
		return elt.toDisplayString();
  } else {
  	return elt.toString();
  }
}

/*
 * Array class (used for linked lists)
 */
Array.prototype.isList = true;
Array.prototype.toDisplayArray = function() {
	return this.map(function (elt) {
		return prepareString(elt);
	});
}
Array.prototype.toString = function() {
	var sexp = '(' + this.toDisplayArray().join(' ') + ')';
	return sexp;
};
Array.prototype.toStringNoOuterBraces = function() {
	var sexp = this.toDisplayArray().join(' ');
	return sexp;
};
Array.prototype.car = function () {
	return this[0];
}
Array.prototype.cdr = function () {
	return this.slice(1, this.length);
}

/*
 * Pair class (used by cons when result is not a well-formed list)
 */
var Pair = function(car, cdr) {
	this.contents = [car, cdr];
	this.isPair = true;
	this.length = 2;
}
Pair.prototype.toString = function() {
	var sexp = '(' + this.contents.toDisplayArray().join(' . ') + ')';
	return sexp;
}
Pair.prototype.car = function () {
	return this.contents[0];
}
Pair.prototype.cdr = function () {
	return this.contents[1];
}

/*
 * String class
 */
String.prototype.toDisplayString = function() {
	if (this.isString) {
		return '"' + this.toString() + '"';
	} else {
		return this.toString();
	}
}
String.prototype.toEvalString = function() {
	return '"' + this.toString().replace(/[\n\r]/g, "\\n").replace(/'/g, "\\'") + '"';
}

function constructString(str) {
	var newStr = new String(str);
	newStr.isString = true;
	return newStr;
}

