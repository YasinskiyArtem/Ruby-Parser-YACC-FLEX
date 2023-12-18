%code {
  #include <stdio.h>
  #include <stdlib.h>
  #include "scanner.h"

  int yylex();
  void yyerror(const char *s);
  //int yylineno;
  extern int keyword_do_state;
}

%defines "parser.h"
//%define api.push-pull push

%locations
//%define parse.error verbose

%token
        keyword_class
        keyword_module
        keyword_def
        keyword_undef
        keyword_begin
        keyword_rescue
        keyword_ensure
        keyword_end
        keyword_if
        keyword_unless
        keyword_then
        keyword_elsif
        keyword_else
        keyword_case
        keyword_when
        keyword_while
        keyword_until
        keyword_for
        keyword_break
        keyword_next
        keyword_redo
        keyword_retry
        keyword_in
        keyword_do
        keyword_do_loop
        keyword_return
        keyword_yield
        keyword_super
        keyword_and
        keyword_or
        keyword_not
        keyword_alias
        keyword_format
        keyword_tLABLE
        keyword_self
        keyword_true 
        keyword_false
        keyword_nil
        keyword_lBEGIN
        keyword_lEND
%token  tIDENTIFIER tCONSTANT
%token tPOW tDSTAR
%token tCMP
%token tEQ
%token tEQQ
%token tNEQ
%token tGEQ
%token tLEQ
%token tANDOP
%token tOROP
%token tMATCH
%token tNMATCH
%token tDOT2
%token tDOT3 tBDOT3
%token tLSHFT
%token tRSHFT
%token tANDDOT
%token tCOLON2
%token tOP_ASGN
%token tASSOC
%token tLBRACK
%token tLBRACE
%token tSTAR
%token tLAMBEG
%token tFID tEQL
%token tCOMMA tCOMMA2 tBANG tLT tGT tDOT tEH tCOLON
%token tLPAREN tRPAREN tLCURLY tRCURLY tLPAREN2 tRPAREN2 tAMPER tRBRACK tTILDE tSEMI tPIPE

%right    tEH tCOLON
%left     tLSHFT tRSHFT
%%
main : program                          { puts("MAIN"); }

program : top_compstmt                  { puts("PROGRAM : top_compstmt"); }
        
top_compstmt : top_stmts mb_terms       { puts("TOP_COMPSTMT : top_stmts mb_terms"); }
        
top_stmts : top_stmt                    { puts("TOP_STMTS : top_stmt"); }
          | top_stmts terms top_stmt    { puts("TOP_STMTS : 2"); }
          | none                        { puts("TOP_STMTS : 3"); }
       
top_stmt : stmt                         { puts("TOP_STMT : stmt"); }
        | keyword_lBEGIN  top_stmt1            { puts("TOP_STMT : BEGIN stmt  "); }

top_stmt1 : tLCURLY top_compstmt tRCURLY       { puts("TOP_STMT1 : {top_comstmt}  "); }

stmts : stmts terms stmt                { puts("STMTS: stmts terms stmt"); }
      | stmt                            { puts("STMTS: stmt"); }
    
stmt : expr                             { puts("STMT: expr"); }

compstmt : stmts mb_terms               { puts("COMPSTMT"); }
        
bodystmt : compstmt mb_rescue mb_else mb_ensure  { puts("BODYSTMT"); }
        | none                          { puts("BODYSTMT NONE"); }
/////////////////////////////////////////////////////////////////////

expr : expr keyword_if expr_if          { puts("expr IF expr_if"); }
     | expr_if                          { puts("expr_if"); }

expr_if : keyword_and expr_and          { puts("AND expr_and"); }
        | expr_and                      { puts(" expr_and"); }

expr_and : expr_and keyword_or expr_or  { puts("expr_and OR expr_or"); }
         | expr_or                      { puts(" expr_or"); }

expr_or : keyword_not expr_not          { puts("expr_or NOT expr_not"); }
        | expr_not                      { puts(" expr_not"); }

expr_not : arg_eq                       { puts("arg_eq"); }
         | tTILDE arg_eq                   { puts("'~' arg_eq"); }
         | tBANG arg_eq                   { puts("'!' arg_eq"); }


arg_eq : arg_tern tEQL arg_tern          { puts("arg_tern '=' arg_tern"); }
       | arg_tern tOP_ASGN arg_tern     { puts("arg_tern tOP_ASGN arg_tern"); }
       | arg_tern                       { puts("arg_tern "); }
       //| arg_eq arg_tern            { puts("arg_eq arg_tern "); }

arg_tern: expr_pr tEH expr_pr tCOLON expr_pr { puts("expr_pr '?' atom_expr ':' expr_pr "); }
	| expr_pr                               { puts(" expr_pr "); }

        
expr_pr : expr_pr '+' mb_nl atom                { puts(" expr_pr '+' "); } 
    | expr_pr '-' mb_nl atom                          { puts(" expr_pr '-' "); }
    | expr_pr tSTAR mb_nl atom                        { puts(" expr_pr *"); }
    | expr_pr '/' mb_nl atom                          { puts(" expr_pr /"); }
    | expr_pr '%' mb_nl atom                          { puts(" expr_pr % "); }
    | expr_pr tPOW mb_nl atom                         { puts(" expr_pr **"); }
    | "+@" atom                                 { puts(" +@ atom "); }
    | "-@" atom                                 { puts(" -@ atom "); }
    | expr_pr tPIPE atom                          { puts(" expr_pr | "); }
    | expr_pr '^' atom                          { puts(" expr_pr ^"); }
    | expr_pr '&' atom                          { puts(" expr_pr & "); }
    | expr_pr tCMP atom                         { puts(" expr_pr <=> "); }
    | expr_pr tEQ atom                          { puts(" expr_pr == "); }
    | expr_pr tEQQ atom                         { puts(" expr_pr === "); }
    | expr_pr tNEQ atom                         { puts(" expr_pr != "); }
    | expr_pr tMATCH atom                       { puts(" expr_pr =~ "); }
    | expr_pr tNMATCH atom                      { puts(" expr_pr !~ "); }
    | expr_pr tLSHFT atom                       { puts(" expr_pr << "); }
    | expr_pr tRSHFT atom                       { puts(" expr_pr >> "); }
    | expr_pr tANDOP atom                       { puts(" expr_pr && "); }
    | expr_pr tOROP atom                        { puts(" expr_pr || "); }
    | expr_pr tDOT2 atom                        { puts(" expr_pr .. atom "); }
    | expr_pr tDOT3 atom                        { puts("expr_pr ... atom"); }
    | expr_pr tDOT atom                     { puts("expr_pr . atom"); }  

    //| expr_pr "=>" atom
    | expr_pr tCOMMA mb_nl atom                       { puts("expr_pr , atom"); } 
    | tDOT2 atom                                { puts(".. atom "); }
    | tDOT3 atom                                { puts("... atom "); }
//    | atom "::" tIDENTIFIER tOP_ASGN arg_rhs            { puts("atom :: tIDENTIFIER tOP_ASGN arg_rhs"); }
//    | atom "::" tCONSTANT tOP_ASGN arg_rhs              { puts("atom :: tCONSTANT tOP_ASGN arg_rhs "); }
//    | atom call_op tIDENTIFIER tOP_ASGN arg_rhs         { puts("atom call_op tIDENTIFIER tOP_ASGN arg_rhs"); }
//    | atom call_op tCONSTANT tOP_ASGN arg_rhs           { puts("atom call_op tCONSTANT tOP_ASGN arg_rhs"); }
//    | atom tLBRACK mb_call_args rbracket tOP_ASGN arg_rhs   { puts("atom [ mb_call_args rbracket tOP_ASGN arg_rhs"); }
//    | keyword_variable1 tOP_ASGN arg_rhs                { puts("keyword_variable1 tOP_ASGN arg_rhs"); }
//    | atom tLBRACK mb_call_args rbracket        { puts("atom [ mb_call_args rbracket "); }
    | expr_pr tLT atom                          { puts("expr_pr < atom "); } //relop 
    | expr_pr tGT atom                          { puts("expr_pr > atom "); } //relop               
    | expr_pr tGEQ atom                         { puts("expr_pr >= rbracket "); } //relop 
    | expr_pr tLEQ atom                         { puts("expr_pr <= rbracket "); } //relop       
    | "defined?" mb_nl atom /*добавить*/        { puts("defined? mb_nl atom"); } 
//    | expr_pr "=>"  atom            { puts(" =>"); }
    | atom                                      { puts(" atom"); }

/*пока нигде не используется*/
//arg_rhs : atom tOP_ASGN                          { puts(" arg_rhs atom top_asgn"); }
//        | atom keyword_rescue atom               { puts("atom RESCUE atom"); }

atom : atom_expr atom_tail                      { puts(" atom atom_expr atom_tail "); }
    //   | tLPAREN atom  tRPAREN                 { puts(" '(' atom ')'  "); }

        

litera : tCONSTANT                              { puts("  tCONSTANT"); }
        | tIDENTIFIER                           { puts("  tIDENTIFIER"); }
        | tFID                                  { puts("  tFID"); }
        | tOP_ASGN                              { puts("  tOP_ASGN"); }

// puts (1 or puts, 2) => 1 (wtf ruby)
atom_expr : litera                                      { puts("litera atom"); }
                | tLPAREN compstmt  tRPAREN                 { puts(" '(' atom ')'  "); } //ДОБАВИТЬ СКОБКИ
                | tLBRACK inarray tRBRACK              { puts("atom_expr '[' inarray ']'"); }
                | tLCURLY assoc_list tRCURLY            { puts("{ assoc_list }"); }
                | keyword_return                        { puts("atom_expr RETURN"); }
                | keyword_yield                         { puts("atom_expr YIELD 1 "); }
                //| keyword_yield  rparen { puts("litera keyword_yield 2 "); }
                //| keyword_yield '(' call_args rparen { puts("litera keyword_yield 3 "); }
                | keyword_not tLPAREN rparen                { puts("atom_expr NOT"); }
                //| keyword_not tLPAREN call_args rparen      { puts("atom_expr NOT '(' call_args rparen"); }
                // | "defined?" mb_nl '(' expr rparen { puts("litera defined?"); } 
                | keyword_break                         { puts("atom_expr BREAK"); }
                | keyword_next                          { puts("atom_expr NEXT"); }
                | keyword_redo                          { puts("atom_expr REDO"); }
                | keyword_retry                         { puts("atom_expr RETRY"); }
                | keyword_format                        { puts("atom_expr FORMAT"); } 
           //  | "->"           { puts("litera ->"); }
               // | method_call brace_block   { puts("litera method_call"); }
                | keyword_rescue                        { puts("atom_expr RESCUE"); } // + 2 SH R 
                | keyword_ensure                        { puts("atom_expr ENSUE"); } // + 2 SH R
                | keyword_variable1                     { puts("atom_expr TRUE|FALSE|NIL|SELF"); } 
               // | keyword_true                        { puts("atom_expr TRUE"); }
               // | keyword_false                                                 { puts("atom_expr FALSE"); }
                | litera paren_args                     {puts("litera paren_args");} //тупо потестить говно
                | keyword_begin mb_terms compstmt keyword_end                           { puts("atom expr BEGIN"); }
                | keyword_if expr then_with_term mb_terms compstmt if_tail keyword_end  { puts("litera IF"); }
	        | keyword_unless expr then_with_term mb_terms compstmt mb_else keyword_end { puts("litera keyword_unless"); }
		| keyword_while expr do_loop mb_terms compstmt keyword_end { puts("litera keyword_while"); }
		| keyword_until  expr do_loop mb_terms compstmt keyword_end { puts("litera keyword_until"); }
		| keyword_case mb_terms case_body keyword_end  { puts("litera keyword_case mb_terms" ); }
		| keyword_case expr mb_terms p_case_body keyword_end { puts("litera keyword_case expr" ); }
                | keyword_for atom keyword_in expr do_loop mb_terms compstmt keyword_end { puts("litera keyword_for " ); }
		| keyword_class classpath superclass mb_terms bodystmt keyword_end { puts("litera keyword_class classpath " ); }
		| keyword_class tLSHFT expr term bodystmt keyword_end  { puts("litera keyword_class tLSHFT " ); }
		| keyword_module classpath mb_terms bodystmt keyword_end { puts("litera keyword_module" ); }
		| keyword_def fname dop_expr f_arglist bodystmt keyword_end { puts("litera keyword_def" ); }
                | keyword_super paren_args { puts("SUPER METHODCALL" ); }
                | keyword_super

                
atom_tail : none                { puts("atom_tail : none" ); }
                //| tCONSTANT     { puts("atom_tail : tCoNSTANT" ); }
                | keyword_begin bodystmt keyword_end        { puts("atom_tail : BEGIN bodystmt END" ); }
                | brace_block /*atom*/                  { puts("brace_block " ); }
                | f_arg atom                        {puts("funcarg atom_tail " ); }
                | f_arg                        {puts("f_arg atom_tail " ); }                 
                | paren_args                   { puts("paren_args atom_tail" ); }
                | atom_tail tLBRACK mb_call_args rbracket { puts("tLBRACK mb_call_args rbracket" ); } // чета там по sh r и rr хуй знает 
                | call_op2 atom                 { puts("call_op2 atom " ); }

//new_expr : keyword_variable1                    { puts("new_expr: keyword_variable1" ); }
                //|  tLPAREN f_args rparen              { puts("new expr ( expr rparen" ); }           
                
dop_expr: tDOT                                   { puts("dop_expr ." ); }
        | tCOLON2                                  { puts("dop_expr ::" ); }
        | none

f_arglist : "(" f_args rparen                    { puts("f_arglist1" ); }
       // | f_paren_args
        | f_args term                                        { puts("f_Arglist2" ); }
/////////////////////////////////




//////////////////////////////////////////

inarray : none                                  { puts("inarray : none" ); }
        //| args { puts("inarray : args" ); }
        | args trailer /*все таки вернул пока обратно*/                         { puts(" inarray : args trailer" ); }
        | args tCOMMA assocs trailer            { puts(" inarray : args ',' assocs trailer" ); }
        | assocs trailer                        { puts(" inarray :  assocs trailer" ); }

paren_args : tLPAREN mb_call_args rparen { puts("paren_args1" ); }
                | tLPAREN tDOT3 rparen {puts ("paren_args2");} // 6rr хз где



//ДОЛЖЕН БЫТЬ В МЕТОДКОЛ
/*mb_paren_args : none    { puts("mb_paren_args1" ); }
        | paren_args    { puts("mb_paren_args2" ); }
*/

mb_call_args : none                             {puts ("mb_call_args : none  ");}
	    | call_args                         {puts ("mb_call_args call_args");}        
            | args tCOMMA                       {puts ("mb_call_args args");}
            | args tCOMMA assocs tCOMMA         {puts ("mb_call_args args ',' assocs ','");}
            | assocs tCOMMA                     {puts ("mb_call_args assocs ',");}



//command : operation call_args | atom call_op operation2 call_args | atom tCOLON2 operation2 call_args | keyword_super call_args  | keyword_return call_args | keyword_yield call_args | keyword_break call_args | keyword_next call_args

call_args : /*command
          |*/ args mb_block_arg                 { puts("call_args1 " ); }
	  | assocs mb_block_arg                 { puts("call_args2 " ); }
          | args tCOMMA assocs mb_block_arg     { puts("call_args3 " ); }
          | block_arg                           { puts("call_args4 " ); }

block_arg : tAMPER expr_pr                      { puts("block_arg1 " ); } 
                | tAMPER                        { puts("block_Arg2 " ); }

mb_block_arg : tCOMMA block_arg                 { puts("mb_block_arg1 " ); }
                | none                          { puts("mb_block_arg2 " ); }        

// mb_default_arg_prefix: default_arg_prefix | none

args : expr_pr 
        | args tCOMMA expr_pr
 

/* такая же хуйня что и  mrhs   хз норм или нет будет 
        | tSTAR expr_pr
        | args tCOMMA2 expr_pr
        | args tCOMMA2 tSTAR expr_pr
*/
//

mrhs : args tCOMMA expr_pr      { puts("args , expr_pr " ); }
        | args tCOMMA tSTAR expr_pr { puts("args , * expr_pr " ); }
        | tSTAR expr_pr         { puts("* expr_pr " ); } 

classpath : litera /*//cpath*/   { puts("classpath litera " ); }     
	  | litera tCOLON2 litera { puts("classpath litera :: litera " ); } 
          | tCOLON2 litera        { puts("classpath :: litera " ); } 


///////////////////////////////////////////////////////////
then_with_term : term    { puts("then_with_term : term"); }
     | keyword_then       { puts("keyword_then"); }
     | term keyword_then { puts("term keyword_then"); }

do_loop  : _do_loop {keyword_do_state = 0;} { puts("do_loop"); }

_do_loop : term                         { puts("do_loop term"); }
	 | keyword_do_loop              { puts("keyword_do_loop"); }

if_tail	: mb_else                       { puts("if_tail mb_else"); }
	| keyword_elsif expr then_with_term mb_terms compstmt if_tail { puts("if_tail ... BIG"); }

mb_else : none                          { puts("mb_else none"); }
	 | keyword_else mb_terms compstmt       { puts("mb_else ... BIG"); }

///////////////////////////////////////
f_marg : litera                                 { puts("f_marg : tIDENTIFIER"); }
      | tLPAREN f_margs rparen                 { puts("f_marg : 2"); }

f_marg_list : f_marg                            { puts("f_marg_list : 1"); }
           | f_marg_list tCOMMA f_marg          { puts("f_marg_list : 2"); }
        //   | f_marg tEQL f_marg                 { puts("SUKU NASHEL"); } // СОСИ ЕБАНЫЙ DEF + 1 rr

f_margs : f_marg_list                                           { puts("f_margs : 1"); } 
        | f_marg_list tEQL f_marg                 { puts("SUKU NASHEL"); } // СОСИ ЕБАНЫЙ DEF + 1 rr
        | f_marg_list tCOMMA f_rest_marg                        { puts("f_margs : 2"); } 
        | f_marg_list tCOMMA f_rest_marg tCOMMA f_marg_list     { puts("f_margs : 3"); } 
        | f_rest_marg                                           { puts("f_margs : 4"); } 
        | f_rest_marg tCOMMA f_marg_list                        { puts("f_margs : 5"); } 


f_rest_marg: tSTAR tIDENTIFIER                        { puts("f_rest_marg * tIDENTIFER"); } 
                | tSTAR                               { puts("f_rest_marg *"); } 

///////////////////////////////////////

/////////////////////////////////////////////////////////// часть говна 

block_args_tail : f_block_kwarg tCOMMA f_kwrest mb_f_block_arg  { puts("block_args_tail 1"); } 
                | f_block_kwarg mb_f_block_arg                  { puts("block_args_tail 2"); } 
                | f_kwrest mb_f_block_arg                       { puts("block_args_tail 3"); } 
                | f_no_kwarg mb_f_block_arg                     { puts("block_args_tail 4"); } 
                | f_block_arg                                   { puts("block_args_tail 5"); } 

mb_block_args_tail : tCOMMA block_args_tail                     { puts("mb_block_args_tail "); } 
                | none                                          { puts("mb_block_args_tail NONE "); }

mb_block_param	:  block_param_def mb_nl                            { puts("mb_block_param "); }
                | none                                          { puts("mb_block_param NONE "); }

block_param_def	: tPIPE mb_bv_decl tPIPE                            { puts("block_param_def |mb_bv_decl| "); }
		| tPIPE block_param_next mb_bv_decl tPIPE           { puts("block_param_def |block_param_next mb_bv_decl| "); }

block_param_next :  f_arg tCOMMA f_block_optarg tCOMMA f_rest_arg  mb_block_args_tail                   { puts("block_param_next 1 "); }
                | f_arg tCOMMA f_block_optarg tCOMMA f_rest_arg tCOMMA f_arg  mb_block_args_tail        { puts("block_param_next 2 "); } 
                | f_arg tCOMMA f_block_optarg tCOMMA f_arg mb_block_args_tail                           { puts("block_param_next 3 "); }
                | f_arg tCOMMA f_rest_arg mb_block_args_tail                                            { puts("block_param_next 4 "); }
                | f_arg tCOMMA                                                                          { puts("block_param_next 5 "); }
                | f_arg tCOMMA f_rest_arg tCOMMA f_arg mb_block_args_tail                               { puts("block_param_next 6 "); }
                | f_arg mb_block_args_tail                                                              { puts("block_param_next 7 "); }
                | f_block_optarg tCOMMA f_rest_arg mb_block_args_tail                                   { puts("block_param_next 8 "); }  
                | f_block_optarg tCOMMA f_rest_arg tCOMMA f_arg mb_block_args_tail                      { puts("block_param_next 9 "); }
                | f_block_optarg mb_block_args_tail                                                     { puts("block_param_next 10 "); }
                | f_block_optarg tCOMMA f_arg mb_block_args_tail                                        { puts("block_param_next 11 "); }
                | f_rest_arg mb_block_args_tail                                                         { puts("block_param_next 12 "); }
                | f_rest_arg tCOMMA f_arg mb_block_args_tail                                            { puts("block_param_next 13 "); }
                | block_args_tail                                                                       { puts("block_param_next 14 "); }
                

///////////////////////////////////////////////////////////

mb_bv_decl	: mb_nl                                                         { puts("mb_bv_decl mb_nil "); }
		| mb_nl tSEMI bv_decls mb_nl                                      { puts("mb_bv_decl mb_nil ; bv_decls mb_nl "); }        

bv_decls	: tIDENTIFIER                                                   { puts("bv_decl tIDENTIFER "); }
		| bv_decls tCOMMA tIDENTIFIER                                   { puts("bv_decls , tIDENTIFER "); }


//lambda

//do_block

//block_call


/*method_call :  litera paren_args
            | atom  call_op litera mb_paren_args // 
            |  atom tCOLON2 litera paren_args  // mb_paren_args !!!last
            | atom tCOLON2 litera               // callop2 atom        
            | atom call_op paren_args           // callop2 atom
            | atom tCOLON2 paren_args           // callop2 atom        
        //  | keyword_super                   типа есть такое 
         // |  '[' mb_call_args rbracket      типа есть такое
 */

brace_block	: keyword_do mb_nl do_body keyword_end     {puts("brace_block DO mb_nl do_body END "); }
	    	| tLCURLY mb_nl brace_body rbrace                       {puts("brace_block { brace_body } "); }     
        
brace_body	:
		  mb_block_param compstmt       {puts("brace_body"); } //+1 sh r за mb_nl

do_body 	:
		  mb_block_param /*mb_terms*/ bodystmt     {puts("do_body"); }     

case_args: expr_pr                                         {puts("case_args expr_pr"); }               
          | tSTAR expr_pr                                    {puts("case_args * expr_pr"); } 
          | case_args tCOMMA expr_pr                       {puts("case_args case_args , expr_pr"); } 
          | case_args tCOMMA tSTAR expr_pr                   {puts("case_args case_args , * expr_pr"); } 

case_body : keyword_when case_args then_with_term mb_terms compstmt cases {puts("case_body"); } 


cases : mb_else                                         {puts("cases mb_else"); } 
  | case_body                                           {puts("cases case_body"); } 
////////////////////////////////////////////////////////////////////////
//////////////////////* НОВАЯ ХУЙНЯ *//////////////////////
///////////////////////////////////////////////////////////////////////

p_case_body : keyword_in p_case_body_first then_with_term mb_terms compstmt p_case_body_second {puts("p_case_body"); }

p_case_body_first : p_top_expr_body {puts("p_case_body_first 1"); }
                | p_top_expr_body keyword_if expr {puts("p_case_body_first 2"); }
                | p_top_expr_body keyword_unless expr {puts("p_case_body_first 3"); }

p_top_expr_body: p_as                   {puts("P_TOP_EXPR_BODY: p_as "); }
                | p_as tCOMMA           {puts("P_TOP_EXPR_BODY: p_as , "); }
                | p_as tCOMMA p_args    {puts("P_TOP_EXPR_BODY: p_as , p_args"); }
                | p_args_tail           {puts("P_TOP_EXPR_BODY: p_args_tail "); }
                | p_kwargs              {puts("P_TOP_EXPR_BODY: p_kwargs "); }

p_as : p_as tASSOC tIDENTIFIER            {puts("P_AS : p_as => tIDENTIFER "); }
                | p_alt                 {puts("P_AS p_alt "); }

p_alt : p_alt '|' p_expr_basic          {puts("P_ALT : p_alt | p_expr_basic "); }
                | p_expr_basic          {puts("P_ALT : p_expr_basic "); }

p_expr_basic : p_expr_basic_first 
                | p_const tLPAREN p_args rparen 
                | p_const tLPAREN p_kwargs rparen 
                | p_const tLBRACK p_args rbracket 
                | p_const tLBRACK p_kwargs rbracket 
                | p_const tLBRACK rbracket 
                | tLBRACK p_args rbracket 
                | tLBRACK rbracket 
                | tLCURLY p_kwargs rbrace 
                | tLCURLY rbrace  
                | tLPAREN p_as rparen 

p_expr_basic_first : p_primitive 
                | p_primitive tDOT2 p_primitive 
                | p_primitive tDOT3 p_primitive 
                | p_primitive tDOT3 
                | '^' p_primitive 
                | p_const 
                | tDOT2 p_primitive 
                | tDOT3 p_primitive

p_primitive : tIDENTIFIER {puts("p_primitive tIDENTIFER "); }

p_args : p_as 
        | p_args_head 
        | p_args_head p_as 
        | p_args_head tSTAR tIDENTIFIER 
        | p_args_head tSTAR tIDENTIFIER tCOMMA p_args_post 
        | p_args_head tSTAR 
        | p_args_head tSTAR tCOMMA p_args_post 
        | p_args_tail 

p_args_head : p_as tCOMMA 
        | p_args_head p_as tCOMMA

p_args_tail : tSTAR tIDENTIFIER 
        | tSTAR tIDENTIFIER tCOMMA p_args_post 
        | tSTAR 
        | tSTAR tCOMMA p_args_post 

p_args_post : p_as 
        | p_args_post tCOMMA p_as 

p_kwargs : p_kwarg tCOMMA p_kwrest 
        | p_kwarg 
        | p_kwrest 
        | p_kwarg tCOMMA p_kwnorest 
        | p_kwnorest 

p_kwarg : p_kw 
        | p_kwarg tCOMMA p_kw 

p_kw : p_kw_label p_as 
        | p_kw_label 

p_kw_label : keyword_tLABLE //| tSTRING_BEG string_contents tLABEL_END 

p_kwrest : kwrest_mark tIDENTIFIER 
        | kwrest_mark 

p_kwnorest : kwrest_mark keyword_nil 

p_const : tCOLON2 tCONSTANT 
        | p_const tCOLON2 tCONSTANT 
        | tCONSTANT 

p_case_body_second: mb_else 
        | p_case_body 

/////////////////////////////////////////////////////////////////////////////
//////////////////////* НОВАЯ ХУЙНЯ КОНЕЦ *//////////////////////
/////////////////////////////////////////////////////////////////////////////

mb_rescue : keyword_rescue exc_list exc_var then_with_term mb_terms compstmt mb_rescue
           | none

exc_list : expr_pr 
         | mrhs         
	 | none

exc_var	: tASSOC lhs 
	| none


mb_ensure : keyword_ensure mb_terms compstmt
           | none

/////////////////////////////////////////////////////////////////////////////

lhs : keyword_variable1 
        //| atom tLBRACK mb_call_args rbracket 
        | atom call_op tIDENTIFIER 
        | atom tDOT2 tIDENTIFIER 
        | atom call_op tCONSTANT 
        | atom tDOT2 tCONSTANT 
        | tCOLON2 tCONSTANT 
        | none 

fname : tIDENTIFIER 
        | tCONSTANT
        | tOP_ASGN 
        | reswords


reswords : "BEGIN"
        | "END"
        | keyword_alias
        | keyword_and
        | keyword_begin
        | keyword_break
        | keyword_case
        | keyword_class
        | keyword_def
        | "defined"
        | keyword_do
        | keyword_else
        | keyword_elsif
        | keyword_end
        | keyword_ensure
        | keyword_true
        | keyword_false
        | keyword_for
        | keyword_in
        | keyword_module
        | keyword_next
        | keyword_nil
        | keyword_not
        | keyword_or
        | keyword_redo
        | keyword_rescue 
        | keyword_retry 
        | keyword_return
        | keyword_self
        | keyword_super
        | keyword_then
        | keyword_yield
        | keyword_if 
        | keyword_unless
        | keyword_while
        | keyword_until
        | keyword_when
        | keyword_undef

keyword_variable1 : keyword_nil | keyword_self | keyword_true | keyword_false
//\\\\\\\\\\\\\\\\\\\\\\\\\\\//

args_tail: f_kwarg tCOMMA f_kwrest mb_f_block_arg 
        | f_kwarg  mb_f_block_arg 
        | f_kwrest mb_f_block_arg
        | f_no_kwarg mb_f_block_arg 
        | f_block_arg 

mb_args_tail : tCOMMA args_tail                                  { puts("mb_args_tail  ',' args_tail " ); }
             | tEQL args_tail
             | none                                           { puts("mb_args_tail  none " ); } 

f_args : f_arg tCOMMA f_optarg tCOMMA f_rest_arg mb_args_tail     { puts("f_arg 1 " ); }
                | f_arg tCOMMA f_optarg tCOMMA f_rest_arg tCOMMA f_arg mb_args_tail { puts("f_arg 2" ); }
                | f_arg tCOMMA f_optarg mb_args_tail                 { puts("f_arg 3" ); }
                | f_arg tCOMMA f_optarg tCOMMA f_arg mb_args_tail     { puts("f_arg 4" ); }
                | f_arg tCOMMA f_rest_arg mb_args_tail               { puts("f_arg 5" ); }
                | f_arg tCOMMA f_rest_arg tCOMMA f_arg mb_args_tail   { puts("f_arg 6 " ); }
                | f_arg mb_args_tail                              { puts("f_arg 7" ); }
                | f_optarg tCOMMA f_rest_arg mb_args_tail              { puts("f_arg 8 " ); }
                | f_optarg tCOMMA f_rest_arg tCOMMA f_arg mb_args_tail  { puts("f_arg 9 " ); }
                | f_optarg mb_args_tail                             { puts("f_arg 10 " ); }
                | f_optarg tCOMMA f_arg mb_args_tail                 { puts("f_arg 11 " ); }
                | f_rest_arg mb_args_tail                           { puts("f_arg 12 " ); }
                | f_rest_arg tCOMMA f_arg mb_args_tail               { puts("f_arg 13 " ); }
	        | args_tail                                         { puts("f_arg args_tail " ); }
                | none                                              { puts("f_arg none " ); }

f_arg_item : litera  { puts("F_ARG_ITEM : litera" ); }
/*уже не идет в пизду*/| tLPAREN f_margs rparen  { puts("F_ARG_ITEM : ( f_margs rparen" ); }    // 7 rr var1 идет в пизду
                      // | tLPAREN f_margs tEQL f_margs  rparen


f_arg	: f_arg_item  { puts("F_ARG :  f_arg_item " ); }
	| f_arg tCOMMA f_arg_item { puts("F_ARG : ',' f_arg_item " ); }
        | f_arg tDOT f_arg_item { puts("F_ARG : '.' f_arg_item " ); }


        
f_kw : keyword_tLABLE expr_pr
        | keyword_tLABLE 

f_block_kw : keyword_tLABLE atom 
        | keyword_tLABLE

f_block_kwarg : f_block_kw 
        | f_block_kwarg tCOMMA f_block_kw 


f_kwarg : f_kw 
        | f_kwarg tCOMMA f_kw   

kwrest_mark : tPOW                             { puts("KWREST_MARK : ** "); }    
        | tDSTAR                               { puts("KWREST_MARK : **2 "); } 

f_no_kwarg : kwrest_mark keyword_nil            { puts("F_NO_KWARG : kwrest_mark NIL "); }

f_kwrest : kwrest_mark tIDENTIFIER              { puts("F_KWREST : kwrest_mark tIDENTIFIER "); }
         | kwrest_mark                          { puts("F_KWREST : kwrest_mark "); }
 
f_opt : tIDENTIFIER tEQ expr_pr                 { puts("F_OPT : tIDENTIFIER '=' atom "); } 

f_block_opt : tIDENTIFIER tEQ atom              { puts("F_BLOCK_OPT : tIDENTIFIER '=' atom "); }   
 
f_block_optarg : f_block_opt                    { puts("F_BLOCK_OPTARG : f_block_opt  "); }
        | f_block_optarg tCOMMA f_block_opt     { puts("F_BLOCK_OPTARG : f_block_optarg , f_block_opt  "); }
 
f_optarg : f_opt                        { puts("F_OPTARG : f_opt  "); }  
        | f_optarg tCOMMA f_opt         { puts("F_OPTARG	: f_optarg tCOMMA f_opt "); }  

f_rest_arg : tSTAR tIDENTIFIER          { puts("F_REST_ARG	: * tIDENTIFIER "); }  
           | tSTAR                      { puts("F_REST_ARG	: * "); }

f_block_arg : tAMPER tIDENTIFIER        { puts("F_BLOCK_ARG	: & tIDENTIFIER "); }    

mb_f_block_arg	: tCOMMA f_block_arg    { puts("MB_F_BLOCK_ARG	: , f_block_arg  "); }
		            | none      { puts("MB_F_BLOCK_ARG	: none  "); }

assoc_list : none                       { puts("ASSOC_LIST	: none  "); } 
        | assocs trailer                { puts("ASSOC_LIST	: assocs trailer  "); }
        //| args trailer 
///////////////////////////////
assocs	: assoc                         { puts("ASSOCS	: assoc"); }
	| assocs tCOMMA assoc           { puts("ASSOCS	: assocs ',' assoc"); }
        
assoc	: expr_pr tASSOC atom           { puts("ASSOC	: expr_pr => expr_pr"); } //НАДО ПРОВЕРИТЬ 
      | keyword_tLABLE expr_pr          { puts("ASSOC	: LABLE expr_pr"); }
      | tDSTAR expr_pr                  { puts("ASSOC	: ** expr_pr"); }
      | tPOW                            { puts("ASSOC	: ** "); }
////////////////////////////////////////////////////lambda ebanaya

//lambda : lambda_1 lambda_2 

//lambda_1 : '(' f_args mb_bv_decl ')' 
//         | f_args 

//lambda_2 : "->" keyword_do bodystmt keyword_end

superclass : tLT expr term      { puts("SUPERCLASS : < expr term  "); }
        | none                  { puts("SUPERCLASS : none  "); }

///////////////////////////////////////////////////////////////////
call_op : tDOT                  { puts("CALL_OP : '.'  "); } //сразу нахуй минус 4 shr
        | tANDDOT               { puts("CALL_OP : &.  "); }
        
call_op2 : call_op              { puts("CALL_OP2 : call_op  "); }
        | tCOLON2               { puts("CALL_OP2 : ::  "); }

mb_terms : none                 { puts("MB_TERMS : none "); }  
        | terms                 { puts("MB_TERMS : terms "); }

mb_nl : none                    { puts("MB_NL : none "); }
      | '\n'                    { puts("MB_NL : n"); } 

rparen : mb_nl tRPAREN          { puts("RPAREN : mb_nl ')'"); }

rbracket : mb_nl tRBRACK        { puts("RBRACKET : mb_nl ']'"); }

rbrace : mb_nl tRCURLY          { puts("RBRACE : mb_nl }"); }

trailer : mb_nl                 { puts("TRAILER : n "); }
        | tCOMMA                { puts("TRAILER : ',' "); }

term : tSEMI                    { puts("TERM : ';'"); }
     | '\n'                     { puts("TERM | 'n'"); }

terms : term                    { puts("TERMS: term "); }
      | terms '\n'              { puts("TERMS | terms 'n' "); }
      
none :                          { puts("NONE LAST"); }

//none_new : 