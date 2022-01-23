module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
	= vint(int n)
	| vbool(bool b)
	| vstr(str s)
	;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
	= input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
	VEnv venv = ();
	
	for (/AQuestion q := f, q has label) {
		switch(q.typ) {
			case integer():
				venv += (q.id.name: vint(0));
			case boolean():
				venv += (q.id.name: vbool(false));
			case string():
				venv += (q.id.name: vstr(""));
		}
	}
	
	return venv;
}

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
	return solve (venv) {
		venv = evalOnce(f, inp, venv);
	}
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
	for (AQuestion q <- f.questions) {
		venv = eval(q, inp, venv);
	}
	
	return venv; 
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {
	// evaluate conditions for branching,
	// evaluate inp and computed questions to return updated VEnv
	switch(q) {
		case question(_, AId id, _): 
			if (id.name == inp.question) {
				venv[id.name] = inp.\value;
			}
		case computed(_, AId id, _, AExpr expr): venv[id.name] = eval(expr, venv);
		case ifBlock(AExpr condition, list[AQuestion] questions):
			if (eval(condition, venv).b) {
				for (AQuestion p <- questions) {
					venv = eval(p, inp, venv);
				}
			}
		case ifElse(AExpr condition, list[AQuestion] questionsIf, list[AQuestion] questionsElse):
			if (eval(condition, venv).b) {
				for (AQuestion p <- questionsIf) {
					venv = eval(p, inp, venv);
				}
			} else {
				for (AQuestion p <- questionsElse) {
					venv = eval(p, inp, venv);
				}
			}
	}
	
	return venv; 
}

Value eval(AExpr e, VEnv venv) {
	switch (e) {
		case ref(id(str x)): return venv[x];
		case litInt(int n): return vint(n);
		case litBool(bool b): return vbool(b);
		case litStr(str s): return vstr(s);
		case not(AExpr arg): return vbool(!eval(arg, venv).b);
		case mult(AExpr lhs, AExpr rhs): return vint(eval(lhs, venv).n * eval(rhs, venv).n);
		case div(AExpr lhs, AExpr rhs): return vint(eval(lhs, venv).n / eval(rhs, venv).n);
		case add(AExpr lhs, AExpr rhs): return vint(eval(lhs, venv).n + eval(rhs, venv).n);
		case sub(AExpr lhs, AExpr rhs): return vint(eval(lhs, venv).n - eval(rhs, venv).n);
		case gt(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv).n > eval(rhs, venv).n);
		case lt(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv).n < eval(rhs, venv).n);
		case let(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv).n <= eval(rhs, venv).n);
		case get(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv).n >= eval(rhs, venv).n);
		case eq(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv) == eval(rhs, venv));
		case dif(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv) != eval(rhs, venv));
		case and(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv).b && eval(rhs, venv).b);
		case or(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv).b || eval(rhs, venv).b);
		default: throw "Unsupported expression <e>";
	}
}