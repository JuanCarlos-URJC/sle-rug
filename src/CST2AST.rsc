module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;
import Boolean;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf)
	= cst2ast(sf.top); // remove layout before and after form
	
AForm cst2ast(f:(Form)`form <Id n> {<Question* qs>}`)	
	= form("<n>", [cst2ast(q) | Question q <- qs], src=f@\loc);

AQuestion cst2ast(Question q) {
	switch(q) {
		case (Question)`<Str l> <Id i> : <Type t>`: 
			return question("<l>", id("<i>", src=i@\loc), cst2ast(t), src=q@\loc);
		case (Question)`<Str l> <Id i> : <Type t> = <Expr e>`: 
			return computed("<l>", id("<i>", src=i@\loc), cst2ast(t), cst2ast(e), src=q@\loc);
		case (Question)`if (<Expr e>) {<Question* qs>}`: 
			return ifBlock(cst2ast(e), [cst2ast(q) | Question q <- qs], src=q@\loc);
		case (Question)`if (<Expr e>) {<Question* qs>} else {<Question* qes>}`: 
			return ifElse(cst2ast(e), [cst2ast(q) | Question q <- qs], [cst2ast(q) | Question q <- qes], src=q@\loc);
		default: throw "Unhandled question: <q>";
	}
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(id("<x>", src=x@\loc), src=e@\loc);
    case (Expr)`<Int n>`: return litInt(toInt("<n>"), src=e@\loc);
    case (Expr)`<Bool b>`: return litBool(fromString("<b>"), src=e@\loc);
    case (Expr)`<Str s>`: return litStr("<s>", src=e@\loc);
    case (Expr)`(<Expr ex>)`: return cst2ast(ex);
    case (Expr)`!<Expr ex>`: return not(cst2ast(ex), src=e@\loc);
    case (Expr)`<Expr lhs> * <Expr rhs>`: return mult(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> / <Expr rhs>`: return div(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> + <Expr rhs>`: return add(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> - <Expr rhs>`: return sub(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> \> <Expr rhs>`: return gt(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> \< <Expr rhs>`: return lt(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> \<= <Expr rhs>`: return let(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> \>= <Expr rhs>`: return get(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> == <Expr rhs>`: return eq(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> != <Expr rhs>`: return dif(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> && <Expr rhs>`: return and(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> || <Expr rhs>`: return or(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
	switch (t) {
		case (Type)`integer`: return integer(src=t@\loc);
		case (Type)`boolean`: return boolean(src=t@\loc);
		case (Type)`string`: return string(src=t@\loc);
		default: throw "Unhandled type: <t>";
	}
}