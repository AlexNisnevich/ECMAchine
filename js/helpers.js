/*
 * Clones an object
 */
function clone(obj){
    if(obj == null || typeof(obj) != 'object')
        return obj;
    var temp = new obj.constructor(); 
    for(var key in obj)
        temp[key] = clone(obj[key]);
    return temp;
}

/*
 * Array class (used for linked lists)
 */
Array.prototype.isList = true;
Array.prototype.toString = function() {
	var sexp = '(' + this.join(' ') + ')';
	return sexp;
};
Array.prototype.toStringNoOuterBraces = function() {
	var sexp = this.join(' ');
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
	var sexp = '(' + this.contents.join(' . ') + ')';
	return sexp;
}
Pair.prototype.car = function () {
	return this.contents[0];
}
Pair.prototype.cdr = function () {
	return this.contents[1];
}