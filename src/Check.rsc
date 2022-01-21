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
rel[loc def, str name, str label, Type \type] tenv = {};
Type t = tunknown();
	for (/q:question(str label, AId id, AType typ) := f.questions){
		switch (typ){
			case integer(): t = tint();
			case boolean(): t = tbool();
			case string(): t = tstr();
		}
		tenv += <q.src, id.name, label, t>;
	}	
  return tenv;
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
	set[Message] message = {};
	set[str] labeled = {};
	rel[str name, str label, AType typ] defined = {};
	
	for (/q:question(str label, AId id, AType typ) := f){
 		message += {warning ("Label used before", q.src)|label in labeled};
 		labeled += {q.label};
 		for(def <- defined){
			if (id.name == def.name){
				message += {error ("Duplicate question with different type", q.src)|typ != def.typ};
				message += {warning ("Different label for occurrences of same question", q.src)|label != def.label};
			}
		}
		defined += <id.name,label,typ>;
	}
	
	for(a <- f.questions){
		message += check(a, tenv, useDef);
	}	
	
 	return message;  
}

 set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
 	set[Message] message = {};


 	switch(q){
 	//case question(str label, AId id, AType typ): print(2);
 	case computed(str label, AId id, AType typ, AExpr expr):
 		message += check(expr, tenv, useDef);
 		
 	case ifBlock(AExpr condition, list[AQuestion] questions, src = loc u):{ 
 		message += {error ("Conditions must be boolean", u)|typeOf(condition,tenv,useDef) !=  tbool()};
 		for (ques <- q.questions){
 			message += check(ques, tenv, useDef);
 		}
 		message += check(condition,tenv,useDef);}
 		
 	case ifElse(AExpr condition, list[AQuestion] questionsIf, list[AQuestion] questionsElse):{
 		message += {error ("Conditions must be boolean", u)|typeOf(condition,tenv,useDef) !=  tbool()};
 		for (ques <- q.questionsIf){
 			message += check(ques, tenv, useDef);
 		}
 		for (ques <- q.questionsElse){
 			message += check(ques, tenv, useDef);
 		}
 		message += check(condition,tenv,useDef);
 		}
 	};

  return message; 
}


set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  switch (e) {
    case ref(AId x):{
    	msgs += { error("Undeclared question", x.src) | useDef[x.src] == {} };
    }
    default:
    	msgs += { error("Can not operate with diferent types",e.src)|typeOf(e.lhs,tenv,useDef) != typeOf(e.rhs,tenv,useDef)};	
  }
	return msgs;
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
	Type l;
	Type r;
  switch (e) {
    case ref(id(_, src = loc u)):  
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
        }
    case litInt(int n):{
    	return tint();}
    case litBool(bool b):{
     	return tbool();}
    case litStr(str s):{
    	return tstr();}
    default:{
    	if( typeOf(e.lhs,tenv,useDef)== typeOf(e.rhs,tenv,useDef)){
    		return l;}
    } 
  }
  return tunknown(); 
}
