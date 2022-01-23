module Check
import AST;
import Resolve;
import Message;
import Prelude;

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f){
	return {<q.src, id.name, label, aType2Type(typ)> | /q:question(str label, AId id, AType typ) := f} +
		   {<q.src, id.name, label, aType2Type(typ)> | /q:computed(str label, AId id, AType typ, _) := f};
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning.
set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
	set[Message] message = {};
	set[str] labeled = {};
	rel[str name, str label, AType typ] defined = {};
	
	for (/q:question(str label, AId id, AType typ) := f) {
		message += {warning("Label used before", q.src) | label in labeled};
 		labeled += {label};
 		
 		for (def <- defined) {
			if (id.name == def.name) {
				message += {error("Duplicate question with different type", q.src) | aType2Type(typ) != aType2Type(def.typ)};
				message += {warning("Different label for occurrences of same question", q.src) | label != def.label};
			}
		}
		
		defined += <id.name, label, typ>;
	}
	
	for (/p:computed(str label, AId id, AType typ, _) := f) {
		message += {warning("Label used before", p.src) | label in labeled};
 		labeled += {label};
 		
 		for (def <- defined) {
			if (id.name == def.name) {
				message += {error("Duplicate question with different type", p.src) | aType2Type(typ) != aType2Type(def.typ)};
				message += {warning("Different label for occurrences of same question", p.src) | label != def.label};
			}
		}
		
		defined += <id.name, label, typ>;
	}
		
	for (a <- f.questions) {
		message += check(a, tenv, useDef);
	}	
	
 	return message;  
}

// - the declared type computed questions should match the type of the expression.
// - conditions must be boolean.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
	set[Message] message = {};
	
 	switch(q){
 		case computed(_, _, AType typ, AExpr expr, src = loc u): {
 			message += check(expr, tenv, useDef);
 			message += {error("The declared type of the question does not match the type of the expression", u) 
 						| typeOf(expr, tenv, useDef) !=  aType2Type(typ)};
 		}
 		case ifBlock(AExpr condition, list[AQuestion] questions, src = loc u): { 
 			message += check(condition, tenv, useDef);
 			message += {error("Condition must be boolean", u) | typeOf(condition, tenv, useDef) !=  tbool()};
 			
 			for (ques <- questions) {
 				message += check(ques, tenv, useDef);
 			}
 		}
 		case ifElse(AExpr condition, list[AQuestion] questionsIf, list[AQuestion] questionsElse, src = loc u): {
 			message += check(condition,tenv,useDef);
 			message += {error("Condition must be boolean", u) | typeOf(condition, tenv, useDef) !=  tbool()};
 			
 			for (ques <- questionsIf) {
 				message += check(ques, tenv, useDef);
 			}
 			
 			for (ques <- questionsElse){
 				message += check(ques, tenv, useDef);
 			}
 		}
 	}
 	
	return message; 
}

//Converts an AType to Type
private Type aType2Type(AType typ) {
	switch(typ) {
		case integer(): return tint();
		case boolean(): return tbool();
		default: return tstr();
	}
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
	set[Message] msgs = {};
	
	switch (e) {
		case ref(AId x): 
			msgs += {error("Undeclared question", x.src) | useDef[x.src] == {}};
    	default:
    		msgs += {error("Incorrect expression", e.src) | typeOf(e, tenv, useDef) == tunknown()};	
  	}
	
	return msgs;
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
	switch (e) {
    	case ref(id(_, src = loc u)):  
			if (<u, loc d> <- useDef, <d, _, _, Type t> <- tenv) {
        		return t;
        	}
    	case litInt(_):	return tint();
    	case litBool(_): return tbool();
    	case litStr(_): return tstr();
    	case not(AExpr arg):
    		if (typeOf(arg, tenv, useDef) == tbool()) {
    			return tbool();
    		}
    	case mult(AExpr lhs, AExpr rhs):
    		if ((typeOf(lhs, tenv, useDef) == tint()) && (typeOf(rhs, tenv, useDef) == tint())) {
    			return tint();
    		}
    	case div(AExpr lhs, AExpr rhs):
    		if ((typeOf(lhs, tenv, useDef) == tint()) && (typeOf(rhs, tenv, useDef) == tint())) {
    			return tint();
    		}
    	case add(AExpr lhs, AExpr rhs):
    		if ((typeOf(lhs, tenv, useDef) == tint()) && (typeOf(rhs, tenv, useDef) == tint())) {
    			return tint();
    		}
    	case sub(AExpr lhs, AExpr rhs):
    		if ((typeOf(lhs, tenv, useDef) == tint()) && (typeOf(rhs, tenv, useDef) == tint())) {
    			return tint();
    		}
    	case gt(AExpr lhs, AExpr rhs):
    		if ((typeOf(lhs, tenv, useDef) == tint()) && (typeOf(rhs, tenv, useDef) == tint())) {
    			return tbool();
    		}
    	case lt(AExpr lhs, AExpr rhs):
    		if ((typeOf(lhs, tenv, useDef) == tint()) && (typeOf(rhs, tenv, useDef) == tint())) {
    			return tbool();
    		}
    	case let(AExpr lhs, AExpr rhs):
    		if ((typeOf(lhs, tenv, useDef) == tint()) && (typeOf(rhs, tenv, useDef) == tint())) {
    			return tbool();
    		}
    	case get(AExpr lhs, AExpr rhs):
    		if ((typeOf(lhs, tenv, useDef) == tint()) && (typeOf(rhs, tenv, useDef) == tint())) {
    			return tbool();
    		}
    	case and(AExpr lhs, AExpr rhs):
    		if ((typeOf(lhs, tenv, useDef) == tbool()) && (typeOf(rhs, tenv, useDef) == tbool())) {
    			return tbool();
    		}
    	case or(AExpr lhs, AExpr rhs):
    		if ((typeOf(lhs, tenv, useDef) == tbool()) && (typeOf(rhs, tenv, useDef) == tbool())) {
    			return tbool();
    		}
    	default:
    		if (typeOf(e.lhs, tenv, useDef) == typeOf(e.rhs, tenv, useDef)) {
    			return tbool();
    		}
	}
	
	return tunknown(); 
}