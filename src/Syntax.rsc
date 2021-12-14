module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form
	= "form" Id "{" Question* "}";

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
	= Str Id ":" Type
	| Str Id ":" Type "=" Expr
	| "{" Question* "}"
	| "if" "(" Id ")" "{" Question* "}"
	| "if" "(" Id ")" "{" Question* "}" "else" "{" Question* "}";

// TODO: +, -, , /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr
	= Id \ "true" \ "false" // true/false are reserved keywords.
	| Int
	| Bool
	| Str
	| bracket "(" Expr ")"
	| "!" Expr
	> left Expr "*" Expr
	| left Expr "/" Expr
	> left Expr "+" Expr
	| left Expr "-" Expr
	> left Expr "\>" Expr
	| left Expr "\<" Expr
	| left Expr "\<=" Expr
	| left Expr "\>=" Expr
	> left Expr "==" Expr
	| left Expr "!=" Expr
	> left Expr "&&" Expr
	> left Expr "||" Expr;

syntax Type
	= "integer"
	| "boolean"
	| "string";

lexical Str
	= @category="StringLiteral"  [\"] ![\"]* [\"];

lexical Int
	= [\-]? [1..9][0..9]+
	| [0];

lexical Bool
    = "true"
    | "false";