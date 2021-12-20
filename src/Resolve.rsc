module Resolve

import AST;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

alias UseDef = rel[loc use, loc def];

// the reference graph
alias RefGraph = tuple[
 	Use uses, 
 	Def defs, 
	UseDef useDef
]; 

RefGraph resolve(AForm f) = <us, ds, us o ds>
	when Use us := uses(f), Def ds := defs(f);

Use uses(AForm f) {
	return {<b.src, id.name> | /b:ifBlock(AId id, list[AQuestion] questions) := f} +
  		   {<b.src, id.name> | /b:ifElse(AId id, list[AQuestion] questionsIf, list[AQuestion] questionsElse) := f} +
  		   {<r.src, id.name> | /r:ref(AId id) := f};
}

Def defs(AForm f) {
	return {<id.name, q.src> | /q:question(str label, AId id, AType typ) := f} + 
  		   {<id.name, q.src> | /q:computed(str label, AId id, AType typ, AExpr expr) := f}; 
}