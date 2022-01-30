module Compile

import AST;
import Resolve;
import Transform;
import Boolean;
import IO;
import util::Math;
import lang::html5::DOM; // see standard library

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */
 


void compile(AForm f) {

  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

HTML5Node form2html(AForm f) {
  return html(
    head(title(f.name)),
    body(
      
        form(
          div(
           [block(b)|b<-f.questions]
          ),
          script(src("https://cdn.jsdelivr.net/npm/vue@2.5.21/dist/vue.min.js")),
          script(src(f.src[extension="js"].file))
          
        )
     )
 );
}
HTML5Node block(AQuestion b){
	switch(b){
	
		case ifBlock(AExpr condition, list[AQuestion] questions):
			return div(h1(
				label(\for("<toStr(condition)>"), toStr(condition)),
				h2(
				[block(q)|q<-b.questions])) ,html5attr("v-if", toStr(condition)));
		case ifElse(AExpr condition, list[AQuestion] questionsIf, list[AQuestion] questionsElse):
			return 
			div(h1(
				label(\for("<toStr(condition)>"), toStr(condition))),
				h2(
				[block(q)|q<-b.questionsIf],html5attr("v-if", toStr(condition))),h2(
				[block(q)|q<-b.questionsElse],html5attr("v-else", toStr(condition))			
			));
		case question(str label, AId id, AType typ): return div(question(b));
		case computed(str label, AId id, AType typ, AExpr expr): return div(computed(b));
	}
}
HTML5Node question(AQuestion q){
	
		switch(q.typ){
		
		case string():
			return 
			p(
				label(\for("<q.id.name>"), q.label),
				input(\type("text"),name(q.id.name))
			);
		
		case integer():
			return 
			p(
				label(\for("<q.id.name>"), q.label),
				input(\type("number"),name(q.id.name))
			);
	
		
		case boolean():
		
			return 
			p(
				label(\for("<q.id.name>"), q.label),
				label(\for("True"), "Yes"),
				input(name("true"), \type("checkbox")),
				label(\for("False"), "No"),
				input(name("false"), \type("checkbox"))
			);		
	}

}

HTML5Node computed(AQuestion q){
	
		switch(q.typ){
		
		case string():
			return 
			p(
				label(\for("<q.id.name>"), q.label),
				//label(\for("<q.id.name>"), toStr(q.expr)),
				input(name(q.id.name), readonly("readonly"))
			);
		
		case integer():
			return 
			p(
				label(\for("<q.id.name>"), q.label),
				label(\for("<q.id.name>"), resolve(q.expr)),
				input(name(q.id.name),readonly("readonly"))
			);
	
		
		case boolean():
		
			return 
			p(
				label(\for("<q.id.name>"), q.label),
				//label(\for("<q.id.name>"), toStr(q.expr)),
				input(name(q.id.name), readonly("readonly"))
			);		
	}

}
int resolve(AExpr e){
switch(e){
	case ref(AId id): return toInt(getElementById(id.name));
	case litInt(int n): return n;
	case litBool(bool b): return toString(b);
	case litStr(str s): return s;
	case not(AExpr arg): return ("!"+resolve(arg));
	case mult(AExpr lhs, AExpr rhs): return(resolve(lhs)+" * "+resolve(rhs));
	case div(AExpr lhs, AExpr rhs): return(toStr(lhs)+" / "+toStr(rhs));
	case add(AExpr lhs, AExpr rhs): return(toStr(lhs)+" + "+toStr(rhs));
	case sub(AExpr lhs, AExpr rhs): return(resolve(lhs)+" - "+resolve(rhs));
	case gt(AExpr lhs, AExpr rhs): return(toStr(lhs)+" \> "+toStr(rhs));
	case lt(AExpr lhs, AExpr rhs): return(toStr(lhs)+" \< "+toStr(rhs));
	case let(AExpr lhs, AExpr rhs): return(toStr(lhs)+" =\< "+toStr(rhs));
	case get(AExpr lhs, AExpr rhs): return(toStr(lhs)+" =\> "+toStr(rhs));
	case eq(AExpr lhs, AExpr rhs): return(toStr(lhs)+" == "+toStr(rhs));
	case dif(AExpr lhs, AExpr rhs): return(toStr(lhs)+" != "+toStr(rhs));
	case and(AExpr lhs, AExpr rhs): return(toStr(lhs)+" && "+toStr(rhs));
	case or(AExpr lhs, AExpr rhs): return(toStr(lhs)+" || "+toStr(rhs));
	};
	return "";
}

str toStr(AExpr e){
switch(e){
	case ref(AId id):{return id.name;}
	case litInt(int n): return toString(n);
	case litBool(bool b): return toString(b);
	case litStr(str s): return s;
	case not(AExpr arg): return ("!"+toStr(arg));
	case mult(AExpr lhs, AExpr rhs): return(toStr(lhs)+" * "+toStr(rhs));
	case div(AExpr lhs, AExpr rhs): return(toStr(lhs)+" / "+toStr(rhs));
	case add(AExpr lhs, AExpr rhs): return(toStr(lhs)+" + "+toStr(rhs));
	case sub(AExpr lhs, AExpr rhs): return(toStr(lhs)+" - "+toStr(rhs));
	case gt(AExpr lhs, AExpr rhs): return(toStr(lhs)+" \> "+toStr(rhs));
	case lt(AExpr lhs, AExpr rhs): return(toStr(lhs)+" \< "+toStr(rhs));
	case let(AExpr lhs, AExpr rhs): return(toStr(lhs)+" =\< "+toStr(rhs));
	case get(AExpr lhs, AExpr rhs): return(toStr(lhs)+" =\> "+toStr(rhs));
	case eq(AExpr lhs, AExpr rhs): return(toStr(lhs)+" == "+toStr(rhs));
	case dif(AExpr lhs, AExpr rhs): return(toStr(lhs)+" != "+toStr(rhs));
	case and(AExpr lhs, AExpr rhs): return(toStr(lhs)+" && "+toStr(rhs));
	case or(AExpr lhs, AExpr rhs): return(toStr(lhs)+" || "+toStr(rhs));
	};
	return "";
}

    
str val(AType typ) {
  switch (typ) {
    case integer():
      return ("0");
    case boolean():
      return "false";
    case string():
      return "\"\"";
  }
}
    str form2js(AForm f) {
 
  return "var app = new Vue({
         '  el: \'#app\',
    '    <for (/q:question(str label, AId id, AType typ) := f.questions) {>
    '    <id.name>: <val(typ)>
    '    <}>
    '  },
    '  functions: {
    '    <for (/q:computed(str label, AId id, AType typ, AExpr expr) := f.questions) {>
    '    <id.name>: function() {
    '      return <toStr(expr)>;
    '    },
    '    <}>
    '    <for (/q:ifBlock(AExpr condition, list[AQuestion] questions, src = loc u ) := f.questions) {>
    '    <toStr(condition)>: function() {
    '      return <questions2js(questions)>;
    '    },
    '    <}>
    '    <for (/q:ifElse(AExpr condition, list[AQuestion] questionsIf, list[AQuestion] questionsElse, src = loc u) := f.questions) {>
    '    <toStr(condition)>: function() {
    '      return <questions2js(questionsIf)>,<questions2js(questionsElse)>;
    '    },
    '    <}>
    '  },
    '});";
}

str questions2js(list[AQuestion] questions){
str ques = "";
	for(AQuestion q <- questions){
		ques+=q.label;
		ques+="\n";
		}
return ques;
}
