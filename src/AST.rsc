module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
	= form(str name, list[AQuestion] questions);
	
data AQuestion(loc src = |tmp:///|)
	= question(str label, AId id, AType typ)
	| computed(str label, AId id, AType typ, AExpr expr)
	| ifBlock(AExpr condition, list[AQuestion] questions)
	| ifElse(AExpr condition, list[AQuestion] questionsIf, list[AQuestion] questionsElse);
	
data AExpr(loc src = |tmp:///|)
	= ref(AId id)
	| litInt(int n)
	| litBool(bool b)
	| litStr(str s)
	| not(AExpr arg)
	| mult(AExpr lhs, AExpr rhs)
	| div(AExpr lhs, AExpr rhs)
	| add(AExpr lhs, AExpr rhs)
	| sub(AExpr lhs, AExpr rhs)
	| gt(AExpr lhs, AExpr rhs)
	| lt(AExpr lhs, AExpr rhs)
	| let(AExpr lhs, AExpr rhs)
	| get(AExpr lhs, AExpr rhs)
	| eq(AExpr lhs, AExpr rhs)
	| dif(AExpr lhs, AExpr rhs)
	| and(AExpr lhs, AExpr rhs)
	| or(AExpr lhs, AExpr rhs);
	
data AId(loc src = |tmp:///|)
	= id(str name);
	
data AType(loc src = |tmp:///|)
	= integer()
	| boolean()
	| string();