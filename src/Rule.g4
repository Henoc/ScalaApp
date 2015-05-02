grammar Rule;

/**
 * command + shift + g : javaコード生成
 *
 * ANTLRの注意
 * レクサールールに登録した文字をパーサールールでリテラルで使うとうまくパースできない
 *     ので、 OP の '-' と primary の '-' をどちらも MINUS としている
 *
 */

 expr : factor ( OP factor )* ;
 primary : '(' expr ')' | NUMBER | expandable | STRING ;
 expandable : IDENTIFIER ( postfix )* ;
 factor : (MINUS primary) | primary ;
 cluster : expr | block ;
 statement : ifStatement | whileStatement | letStatement | simple ;
 ifStatement : 'if' primary cluster ( 'else' cluster )? ;
 whileStatement : 'while' expr block ;
 letStatement : 'let' IDENTIFIER ( params )? '=' cluster ;
 block : '{' ( statement )? ( (';' | '\n' ) (statement)? )* '}' ;
 simple : expr ;
 oneLine : (statement)? (';' | '\n') ;

 param : IDENTIFIER ;
 params : (param)+ ;
 postfix : '(' expr ')' | NUMBER | IDENTIFIER | STRING ;

 NUMBER : [0-9]+ ;
 IDENTIFIER : [a-zA-Z][0-9a-zA-Z]* ;
 STRING : '"'[^"]*'"' ;
 OP  : '+'
     | MINUS
     | '*'
     | '/'
     | '%'
     | '<'
     | '>'
     | '=='
     | '!='
     | '<-'
     ;

 /**
  * BasicParserに無いルール
  */
 lines : ( oneLine )* ;
 MINUS : '-' ;
 WS : [ \t\r]+ -> skip ;

