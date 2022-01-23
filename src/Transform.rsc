module Transform

import Syntax;
import Resolve;
import AST;
import ParseTree;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f) {
	return form(f.name, ([] | it + flatten(q, litBool(true)) | AQuestion q <- f.questions));
}

list[AQuestion] flatten(AQuestion q, AExpr e) {
	switch(q) {
		case question(_, _, _):
			return [ifBlock(e, [q])];
		case computed(_, _, _, _):
			return [ifBlock(e, [q])];
		case ifBlock(AExpr condition, list[AQuestion] questions):
			return ([] | it + flatten(p, and(e, condition)) | AQuestion p <- questions);
		case ifElse(AExpr condition, list[AQuestion] questionsIf, list[AQuestion] questionsElse):
			return ([] | it + flatten(p, and(e, condition)) | AQuestion p <- questionsIf) +
				   ([] | it + flatten(p, and(e, not(condition))) | AQuestion p <- questionsElse);
	}
	
	return [];
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
	Id newId = [Id] newName;
	set[loc] instances = {useOrDef};
	
	if (<useOrDef, def> <- useDef) {
		instances += {def} + {use | <use, def> <- useDef};
	}
	
	if (<_, useOrDef> <- useDef) {
		instances += {use | <use, useOrDef> <- useDef};
	}
	
	return visit(f) {
		case (Question)`<Str l> <Id i> : <Type t>` => 
			(Question)`<Str l> <Id newId> : <Type t>` when i@\loc in instances
		case (Question)`<Str l> <Id i> : <Type t> = <Expr e>` => 
			(Question)`<Str l> <Id newId> : <Type t> = <Expr e>` when i@\loc in instances
		case (Expr)`<Id i>` =>
			(Expr)`<Id newId>` when i@\loc in instances
	}; 
}