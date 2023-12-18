%code {
  #include <stdio.h>
  #include <stdlib.h>
  #include "scanner.h"

  int yylex();
  void yyerror(const char *s);
  extern void print(char* msg);
  extern int keyword_do_state;
}

%defines "parser.h"
%locations

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
        keyword_private
        keyword_public
        keyword_command
        keyword_raise
        keyword_defined
%token  tIDENTIFIER tCONSTANT
%token tPOW tDSTAR

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
%token tDOT3 
%token tLSHFT
%token tRSHFT
%token tANDDOT
%token tCOLON2
%token tOP_ASGN
%token tASSOC
%token tLBRACK
%token tSTAR
%token  tEQL
%token tCOMMA  tBANG tLT tGT tDOT tEH tCOLON
%token tLPAREN tRPAREN tLCURLY tRCURLY tAMPER tRBRACK tTILDE tSEMI tPIPE tREG tCARET

%right    tEH tCOLON
%left     tLSHFT tRSHFT
%%
main : program                                                                   { print(" MAIN"); }

program : top_compstmt                                                           { print(" PROGRAM : top_compstmt"); }
        
top_compstmt : top_stmts mb_terms                                                { print(" TOP_COMPSTMT : top_stmts mb_terms"); }
        
top_stmts : top_stmt                                                             { print(" TOP_STMTS : top_stmt"); }
        | top_stmts terms top_stmt                                               { print(" TOP_STMTS : top_stmts terms top_stmt"); }
        | none                                                                   { print(" TOP_STMTS : none"); }
       
top_stmt : stmt                                                                  { print(" TOP_STMT : stmt"); }
        | keyword_lBEGIN  top_stmt1                                              { print(" TOP_STMT : lBEGIN stmt  "); }

top_stmt1 : tLCURLY top_compstmt tRCURLY                                         { print(" TOP_STMT1 : { top_comstmt }  "); }

stmts : stmts terms stmt                                                         { print(" STMTS : stmts terms stmt"); }
      | stmt                                                                     { print(" STMTS : stmt"); }
    
stmt : expr                                                                      { print(" STMT : expr"); }

compstmt : stmts mb_terms                                                        { print(" COMPSTMT : stmts mb_terms  "); }
        
bodystmt : compstmt mb_rescue mb_else mb_ensure                                  { print(" BODYSTMT : compstmt mb_rescue mb_else mb_ensure"); }
        | none                                                                   { print(" BODYSTMT : none"); }
/////////////////////////////////////////////////////////////////////

expr : expr keyword_if mb_nl expr_if                                             { print(" EXPR : expr IF mb_nl expr_if"); }
     | expr_if                                                                   { print(" EXPR : expr_if"); }

expr_if : keyword_and mb_nl expr_and                                             { print(" EXPR_IF : expr_if AND mb_nl expr_and"); }
        | expr_and                                                               { print(" EXPR_IF : expr_and"); }

expr_and : expr_and keyword_or mb_nl  expr_or                                    { print(" EXPR_AND : expr_and OR expr_or"); }
         | expr_or                                                               { print(" EXPR_AND : expr_or"); }

expr_or : keyword_not mb_nl expr_not                                             { print(" EXPR_OR : NOT expr_not"); }
        | expr_not                                                               { print(" EXPR_OR : expr_not"); }

expr_not : arg_eq                                                                { print(" EXPR_NOT : arg_eq"); }
         | tTILDE arg_eq                                                         { print(" EXPR_NOT : '~' arg_eq"); }
         | tBANG arg_eq                                                          { print(" EXPR_NOT : '!' arg_eq"); }


arg_eq : arg_tern tEQL arg_tern                                                  { print(" ARG_EQ : arg_tern '=' arg_tern"); }
       | arg_tern tOP_ASGN arg_tern                                              { print(" ARG_EQ : arg_tern tOP_ASGN arg_tern"); }
       | arg_tern                                                                { print(" ARG_EQ : arg_tern "); }


arg_tern : expr_pr tEH expr_pr tCOLON expr_pr                                    { print(" ARG_TERN : expr_pr '?' atom_expr ':' expr_pr "); }
         | expr_pr                                                               { print(" ARG_TERN : expr_pr "); }

        
expr_pr : expr_pr '+' mb_nl atom                                                 { print(" EXPR_PR '+' "); } 
        | expr_pr '-' mb_nl atom                                                 { print(" EXPR_PR '-' "); }
        | expr_pr tSTAR mb_nl atom                                               { print(" EXPR_PR *"); }
        | expr_pr '/' mb_nl atom                                                 { print(" EXPR_PR /"); }
        | expr_pr '%' mb_nl atom                                                 { print(" EXPR_PR % "); }
        | expr_pr tPOW mb_nl atom                                                { print(" EXPR_PR **"); }
        | "+@" atom                                                              { print(" +@ atom "); }
        | "-@" atom                                                              { print(" -@ atom "); }
        | expr_pr tPIPE atom                                                     { print(" EXPR_PR | "); }
        | expr_pr '^' atom                                                       { print(" EXPR_PR ^"); }
        | expr_pr '&' atom                                                       { print(" EXPR_PR & "); }
        | expr_pr tOP_ASGN atom                                                  { print(" EXPR_PR <=> "); } //2 sh r +
        | expr_pr tEQ atom                                                       { print(" EXPR_PR == "); }
        | expr_pr tEQQ atom                                                      { print(" EXPR_PR === "); }
        | expr_pr tNEQ atom                                                      { print(" EXPR_PR != "); }
        | expr_pr tMATCH atom                                                    { print(" EXPR_PR =~ "); }
        | expr_pr tNMATCH atom                                                   { print(" EXPR_PR !~ "); }
        | expr_pr tLSHFT atom                                                    { print(" EXPR_PR << "); }
        | expr_pr tRSHFT atom                                                    { print(" EXPR_PR >> "); }
        | expr_pr tANDOP atom                                                    { print(" EXPR_PR && "); }
        | expr_pr tOROP atom                                                     { print(" EXPR_PR || "); }
        | expr_pr tDOT2 atom                                                     { print(" EXPR_PR .. atom "); }
        | expr_pr tDOT3 atom                                                     { print(" EXPR_PR ... atom"); }
        | expr_pr tDOT atom                                                      { print(" EXPR_PR . atom"); }  
        | expr_pr tCOMMA atom                                                    { print(" EXPR_PR , atom"); }
        | tDOT2 atom                                                             { print(" .. atom "); }
        | tDOT3 atom                                                             { print(" ... atom "); }
        | expr_pr tLT atom                                                       { print(" EXPR_PR < atom "); } //relop 
        | expr_pr tGT atom                                                       { print(" EXPR_PR > atom "); } //relop               
        | expr_pr tGEQ atom                                                      { print(" EXPR_PR >= rbracket "); } //relop 
        | expr_pr tLEQ atom                                                      { print(" EXPR_PR <= rbracket "); } //relop
        | atom                                                                   { print(" EXPR_PR atom"); }
//////////////////////////////////////////////////////////////////////////////////////////

atom : atom_expr atom_tail                                                       { print(" ATOM atom_expr atom_tail "); }

litera : tCONSTANT                                                               { print(" tCONSTANT"); }
        | tIDENTIFIER                                                            { print(" tIDENTIFIER"); }
        | tOP_ASGN                                                               { print(" tOP_ASGN"); }
        | keyword_variable1                                                      { print(" t_VARIABLE");}
        | tREG                                                                   { print(" tREG");}

atom_expr : litera                                                               { print(" ATOM_EXPR -> litera"); }
        | tLPAREN compstmt tRPAREN mb_terms2                                     { print(" ATOM_EXPR -> '(' atom ')' mb_terms2 "); } //ДОБАВИТЬ СКОБКИ
        | tLBRACK inarray tRBRACK                                                { print(" ATOM_EXPR -> '[' inarray ']'"); }
        | tLCURLY assoc_list tRCURLY                                             { print(" ATOM_EXPR -> { assoc_list }"); }
        | keyword_return                                                         { print(" ATOM_EXPR -> RETURN"); }
        | keyword_yield                                                          { print(" ATOM_EXPR -> YIELD 1 "); }
        | keyword_not tLPAREN rparen                                             { print(" ATOM_EXPR -> NOT"); }
        | keyword_break                                                          { print(" ATOM_EXPR -> BREAK"); }
        | keyword_next                                                           { print(" ATOM_EXPR -> NEXT"); }
        | keyword_redo                                                           { print(" ATOM_EXPR -> REDO"); }
        | keyword_retry                                                          { print(" ATOM_EXPR -> RETRY"); }
        | keyword_format                                                         { print(" ATOM_EXPR -> FORMAT"); } 
        | keyword_command                                                        { print(" ATOM_EXPR -> COMMAND"); } 
        | keyword_raise litera                                                   { print(" ATOM_EXPR -> RAISE"); } 
        | keyword_rescue mb_terms litera                                         { print(" ATOM_EXPR -> RESCUE"); } 
        | litera keyword_ensure                                                  { print(" ATOM_EXPR -> ENSUE"); } 
        | keyword_alias                                                          { print(" ATOM_EXPR -> ALIAS");}
        | litera keyword_unless                                                  { print(" ATOM_EXPR -> UNLESS");}
        | litera paren_args                                                      { print(" ATOM_EXPR -> litera paren_args");} 
        | keyword_begin mb_terms compstmt keyword_end                            { print(" ATOM_EXPR -> BEGIN compstmt END "); }
        | keyword_if expr then_with_term mb_terms compstmt if_tail keyword_end   { print(" ATOM_EXPR -> IF ... END"); }
	| keyword_unless expr then_with_term compstmt mb_else keyword_end        { print(" ATOM_EXPR -> UNLESS ... END"); }  
        | keyword_while expr do_loop mb_terms compstmt keyword_end               { print(" ATOM_EXPR -> WHILE ... END"); }
	| keyword_until  expr do_loop mb_terms compstmt keyword_end              { print(" ATOM_EXPR -> UNTIL ... END"); }
	| keyword_case expr mb_terms case_body keyword_end                       { print(" ATOM_EXPR -> CASE case_body END" ); }
	| keyword_case expr mb_terms p_case_body keyword_end                     { print(" ATOM_EXPR -> CASE p_case_body END" ); }
        | keyword_for atom keyword_in expr do_loop mb_terms compstmt keyword_end { print(" ATOM_EXPR -> FOR ... END " ); }
	| keyword_class classpath superclass mb_terms bodystmt keyword_end       { print(" ATOM_EXPR -> CLASS superclass END " ); }
	| keyword_class tLSHFT expr term bodystmt keyword_end                    { print(" ATOM_EXPR -> CLASS << ... END " ); }
	| keyword_module classpath mb_terms bodystmt keyword_end                 { print(" ATOM_EXPR -> MODULE ... END" ); }
	| keyword_def fname dop_expr f_arglist bodystmt keyword_end              { print(" ATOM_EXPR -> DEF fname f_arglist END " ); }
        | keyword_super paren_args                                               { print(" ATOM_EXPR -> SUPER paren_args " ); }
        | keyword_super                                                          { print(" ATOM_EXPR -> SUPER " ); }
                
atom_tail : none                                                                 { print(" ATOM_TAIL -> none" ); }
        | keyword_begin bodystmt keyword_end                                     { print(" ATOM_TAIL -> BEGIN bodystmt END" ); }
        | brace_block                                                            { print(" ATOM_TAIL -> brace_block " ); }
        | litera brace_block                                                     { print(" ATOM_TAIL -> litera brace_block " ); }
        | f_arg atom                                                             { print(" ATOM_TAIL -> f_arg atom " ); }
        | f_arg                                                                  { print(" ATOM_TAIL -> f_arg " ); } // 9 sh r но без него жизнь не та                 
        | atom_tail paren_args                                                   { print(" ATOM_TAIL -> paren_args" ); }
        | atom_tail tLBRACK mb_call_args rbracket                                { print(" ATOM_TAIL -> '[' mb_call_args rbracket" ); } // 2 shr из-за atom_tail
        | call_op2 atom                                                          { print(" ATOM_TAIL -> call_op2 atom " ); } //есть шифт из-за litera
        | litera call_op2 atom    /* 3 sh r*/                                    { print(" ATOM_TAIL -> call_op2 atom " ); }
        | keyword_return paren_args                                              { print(" ATOM_TAIL -> RETURN paren_args " ); }
///////////////////////////////////////////////////////////////////////////////////////////////////////                

dop_expr : dop_expr tDOT litera                                                  { print(" DOP_EXPR '.'" ); }
        | tCOLON2                                                                { print(" DOP_EXPR '::'" ); }
        | none                                                                   { print(" DOP_EXPR none" ); }

f_arglist : f_arg_item                                                           { print(" F_ARGLIST : f_arg_item " ); }
        | f_args term                                                            { print(" F_ARGLIST : f_args term" ); }

inarray : none                                                                   { print(" INARRAY : none" ); }
        | args trailer                                                           { print(" INARRAY : args trailer" ); }
        | args tCOMMA assocs trailer                                             { print(" INARRAY : args ',' assocs trailer" ); }
        | assocs trailer                                                         { print(" INARRAY : assocs trailer" ); }


paren_args : tLPAREN mb_call_args rparen                                         { print(" PAREN_ARGS : ( mb_call_args rparen" ); }
        | tLPAREN tDOT3 rparen                                                   { print(" PAREN_ARGS : ( ... rparen");} 

mb_call_args : none                                                              { print(" MB_CALL_ARGS : none  ");}
	| call_args                                                              { print(" MB_CALL_ARGS : call_args");}        
        | args tCOMMA                                                            { print(" MB_CALL_ARGS : args");}
        | args tCOMMA assocs tCOMMA                                              { print(" MB_CALL_ARGS : args ',' assocs ','");}
        | assocs tCOMMA                                                          { print(" MB_CALL_ARGS : assocs ',");}

call_args :  args mb_block_arg                                                   { print(" CALL_ARGS : args mb_block_arg " ); }
	| assocs mb_block_arg                                                    { print(" CALL_ARGS : assocs mb_block_arg " ); }
        | args tCOMMA assocs mb_block_arg                                        { print(" CALL_ARGS : args ',' assocs mb_block_arg" ); }
        | block_arg                                                              { print(" CALL_ARGS : block_arg " ); }

block_arg : tAMPER expr_pr                                                       { print(" BLOCK_ARG : '&' expr_pr " ); } 
        | tAMPER                                                                 { print(" BLOCK_ARG : '&' " ); }

mb_block_arg : tCOMMA block_arg                                                  { print(" MB_BLOCK_ARG : ',' block_arg " ); }
        | none                                                                   { print(" MB_BLOCK_ARG : none" ); } 
     
args : expr_pr                                                                   { print(" ARGS : expr_pr " ); } 
        | args tCOMMA expr_pr                                                    { print(" ARGS : args ',' expr_pr " ); }      

mrhs : args tCOMMA expr_pr                                                       { print(" MRHS : args ',' expr_pr " ); }
        | args tCOMMA tSTAR expr_pr                                              { print(" MRHS : args ',''*' expr_pr " ); }
        | tSTAR expr_pr                                                          { print(" MRHS : '*' expr_pr " ); } 

classpath : litera                                                               { print(" CLASSPATH : litera " ); }     
	| litera tCOLON2 litera                                                  { print(" CLASSPATH : litera '::' litera " ); } 
        | tCOLON2 litera                                                         { print(" CLASSPATH : '::' litera " ); } 


///////////////////////////////////////////////////////////
then_with_term : term                                                            { print(" THEN_WITH_TERM : term"); }
        | keyword_then                                                           { print(" THEN_WITH_TERM : THEN"); }
        | term keyword_then                                                      { print(" THEN_WITH_TERM : term THEN"); }
 
do_loop  : _do_loop {keyword_do_state = 0;}                                      { print(" DO_LOOP"); }

_do_loop : term                                                                  { print(" _DO_LOOP : term"); }
	| keyword_do_loop                                                        { print(" _DO_LOOP : DO"); }

if_tail	: mb_else                                                                { print("IF_TAIL : mb_else"); }
	| keyword_elsif expr then_with_term mb_terms compstmt if_tail            { print("IF_TAIL : ELSIF ... if_tail"); }

mb_else : none                                                                   { print(" MB_ELSE : none"); }
	| keyword_else mb_terms compstmt                                         { print(" MB_ELSE : ELSE compstmt"); }

//////////////////////////////////////////////////////////////////////////////
f_marg : litera                                                                  { print(" F_MARG : litera"); }
        | tLPAREN f_margs rparen                                                 { print(" F_MARG : '(' f_margs rparen "); }
        | none                                                                   { print(" F_MARG : none"); }

f_marg_list : f_marg                                                             { print(" F_MARG_LIST : f_marg"); }
        | f_marg_list tCOMMA f_marg                                              { print(" F_MARG_LIST : f_marg_list ',' f_marg"); }
        | f_marg_list tEQL f_marg                                                { print(" F_MARG_LIST : f_marg_list '=' f_marg"); } 

f_margs : f_marg_list                                                            { print(" F_MARGS : f_marg_list"); } 
        | f_marg_list tCOMMA f_rest_marg                                         { print(" F_MARGS : f_marg_list ',' f_rest_marg"); } 
        | f_marg_list tCOMMA f_rest_marg tCOMMA f_marg_list                      { print(" F_MARGS : f_marg_list ',' f_rest_marg ',' f_marg_list"); } 
        | f_rest_marg                                                            { print(" F_MARGS : f_rest_marg"); } 
        | f_rest_marg tCOMMA f_marg_list                                         { print(" F_MARGS : f_rest_marg ',' f_marg_list "); } 


f_rest_marg: tSTAR tIDENTIFIER                                                   { print(" F_REST_MARG : '*' tIDENTIFER"); } 
        | tSTAR                                                                  { print(" F_REST_MARG : '*' "); } 

///////////////////////////////////////////////////////////

block_args_tail : f_block_kwarg tCOMMA f_kwrest mb_f_block_arg                   { print(" BLOCK_ARGS_TAIL : f_block_kwarg ',' f_kwrest mb_f_block_arg"); } 
        | f_block_kwarg mb_f_block_arg                                           { print(" BLOCK_ARGS_TAIL : f_block_kwarg mb_f_block_arg"); } 
        | f_kwrest mb_f_block_arg                                                { print(" BLOCK_ARGS_TAIL : f_kwrest mb_f_block_arg"); } 
        | f_no_kwarg mb_f_block_arg                                              { print(" BLOCK_ARGS_TAIL : f_no_kwarg mb_f_block_arg "); } 
        | f_block_arg                                                            { print(" BLOCK_ARGS_TAIL : f_block_arg"); } 

mb_block_args_tail : tCOMMA block_args_tail                                      { print(" MB_BLOCK_ARGS_TAIL : ',' block_args_tail "); } 
        | none                                                                   { print(" MB_BLOCK_ARGS_TAIL : NONE "); }

mb_block_param	:  block_param_def mb_nl                                         { print(" MB_BLOCK_PARAM : block_param_def mb_nl "); }
        | none                                                                   { print(" MB_BLOCK_PARAM : NONE "); }

block_param_def	: tPIPE mb_bv_decl tPIPE                                         { print(" BLOCK_PARAM_DEF : '|' mb_bv_decl '|' "); }
	| tPIPE block_param_next mb_bv_decl tPIPE                                { print(" BLOCK_PARAM_DEF :  '|' block_param_next mb_bv_decl '|' "); }

block_param_next :  f_arg tCOMMA  f_block_optarg tCOMMA f_rest_arg  mb_block_args_tail          { print(" BLOCK_PARAM_NEXT : f_arg ','  f_block_optarg ',' f_rest_arg  mb_block_args_tail "); }
        | f_arg tCOMMA f_block_optarg tCOMMA f_rest_arg tCOMMA f_arg  mb_block_args_tail        { print(" BLOCK_PARAM_NEXT : f_arg ',' f_block_optarg ',' f_rest_arg ',' f_arg  mb_block_args_tail "); } 
        | f_arg tCOMMA f_block_optarg tCOMMA f_arg mb_block_args_tail                           { print(" BLOCK_PARAM_NEXT : f_arg ',' f_block_optarg ',' f_arg mb_block_args_tail  "); }
        | f_arg tCOMMA f_rest_arg mb_block_args_tail                                            { print(" BLOCK_PARAM_NEXT : f_arg ',' f_rest_arg mb_block_args_tail "); }
        | f_arg tCOMMA                                                                          { print(" BLOCK_PARAM_NEXT : f_arg ',' "); }
        | f_arg tCOMMA f_rest_arg tCOMMA f_arg mb_block_args_tail                               { print(" BLOCK_PARAM_NEXT : f_arg ',' f_rest_arg ',' f_arg mb_block_args_tail "); }
        | f_arg mb_block_args_tail                                                              { print(" BLOCK_PARAM_NEXT : f_arg mb_block_args_tail "); }
        | f_block_optarg tCOMMA f_rest_arg mb_block_args_tail                                   { print(" BLOCK_PARAM_NEXT : f_block_optarg ',' f_rest_arg mb_block_args_tail "); }  
        | f_block_optarg tCOMMA f_rest_arg tCOMMA f_arg mb_block_args_tail                      { print(" BLOCK_PARAM_NEXT : f_block_optarg ',' f_rest_arg ',' f_arg mb_block_args_tail "); }
        | f_block_optarg mb_block_args_tail                                                     { print(" BLOCK_PARAM_NEXT : f_block_optarg mb_block_args_tail  "); }
        | f_block_optarg tCOMMA f_arg mb_block_args_tail                                        { print(" BLOCK_PARAM_NEXT : f_block_optarg ',' f_arg mb_block_args_tail "); }
        | f_rest_arg mb_block_args_tail                                                         { print(" BLOCK_PARAM_NEXT : f_rest_arg mb_block_args_tail "); }
        | f_rest_arg tCOMMA f_arg mb_block_args_tail                                            { print(" BLOCK_PARAM_NEXT : f_rest_arg ',' f_arg mb_block_args_tail  "); }
        | block_args_tail                                                                       { print(" BLOCK_PARAM_NEXT : block_args_tail "); }
                

///////////////////////////////////////////////////////////

mb_bv_decl : mb_nl                                                               { print(" MB_BV_DECL : mb_nl "); }
	| mb_nl tSEMI bv_decls mb_nl                                             { print(" MB_BV_DECL : mb_nil ';' bv_decls mb_nl "); }        

bv_decls : tIDENTIFIER                                                           { print(" BV_DECL : tIDENTIFER "); }
	| bv_decls tCOMMA tIDENTIFIER                                            { print(" BV_DECLS : bv_decls ',' tIDENTIFER "); }


//lambda

brace_block : keyword_do mb_nl do_body keyword_end                               { print(" BRACE_BLOCK : DO mb_nl do_body END "); }
	| tLCURLY mb_nl brace_body rbrace                                        { print(" BRACE_BLOCK : '{' brace_body rbrace "); }     
        
brace_body : mb_block_param compstmt                                             {print(" BRACE_BODY : mb_block_param compstmt"); } //+1 sh r за mb_nl

do_body : mb_block_param  bodystmt                                               { print(" DO_BODY : mb_block_param bodystmt"); }     

case_args : expr_pr mb_terms2    /*1shr*/                                        { print(" CASE_ARGS : : expr_pr mb_terms2"); }              
        | tSTAR expr_pr                                                          { print(" CASE_ARGS : '*' expr_pr"); } 
        | case_args tCOMMA expr_pr                                               { print(" CASE_ARGS : case_args ',' expr_pr"); } 
        | case_args tCOMMA tSTAR expr_pr                                         { print(" CASE_ARGS : case_args ',' '*' expr_pr"); }  

case_body : keyword_when case_args then_with_term compstmt cases                 { print(" CASE_BODY : WHEN case_args then_with_term compstmt cases"); } 


cases : mb_else                                                                  { print(" CASES : mb_else"); } 
        | case_body                                                              { print(" CASES : case_body"); } 
////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////

p_case_body : keyword_in p_case_body_first then_with_term mb_terms compstmt p_case_body_second { print(" P_CASE_BODY : IN case_body_first then_with_term mb_terms compstmt p_case_body_second"); }

p_case_body_first : p_top_expr_body                                              { print(" P_CASE_BODY_FIRST : p_top_expr_body"); }
        | p_top_expr_body keyword_if expr                                        { print(" P_CASE_BODY_FIRST : p_top_expr_body IF expr"); }
        | p_top_expr_body keyword_unless expr                                    { print(" P_CASE_BODY_FIRST : p_top_expr_body UNLESS expr"); }

p_top_expr_body: p_as                                                            { print(" P_TOP_EXPR_BODY : p_as "); }
        | p_as tCOMMA                                                            { print(" P_TOP_EXPR_BODY : p_as ',' "); }
        | p_as tCOMMA p_args                                                     { print(" P_TOP_EXPR_BODY : p_as ',' p_args"); }
        | p_args_tail                                                            { print(" P_TOP_EXPR_BODY : p_args_tail "); }
        | p_kwargs                                                               { print(" P_TOP_EXPR_BODY : p_kwargs "); }

p_as : p_as tASSOC tIDENTIFIER                                                   { print(" P_AS : p_as '=>' tIDENTIFER "); }
        | p_alt                                                                  { print(" P_AS : p_alt "); }

p_alt : p_alt tPIPE p_expr_basic                                                 { print(" P_ALT : p_alt '|' p_expr_basic "); }
        | p_expr_basic                                                           { print(" P_ALT : p_expr_basic "); }

p_expr_basic : p_expr_basic_first                                                { print(" P_EXPR_BASIC : p_expr_basic_first "); }
        | p_const tLPAREN p_args rparen                                          { print(" P_EXPR_BASIC : p_const '(' p_args rparen "); }
        | p_const tLPAREN p_kwargs rparen                                        { print(" P_EXPR_BASIC : p_const '(' p_kwargs rparen "); }
        | p_const tLBRACK p_args rbracket                                        { print(" P_EXPR_BASIC : p_const '[' p_args rbracket "); }
        | p_const tLBRACK p_kwargs rbracket                                      { print(" P_EXPR_BASIC : p_const '[' p_kwargs rbracket "); }
        | p_const tLBRACK rbracket                                               { print(" P_EXPR_BASIC : p_const '[' rbracket "); }
        | tLBRACK p_args rbracket                                                { print(" P_EXPR_BASIC : '[' p_args rbracket "); }
        | tLBRACK rbracket                                                       { print(" P_EXPR_BASIC : '[' rbracket "); }
        | tLCURLY p_kwargs rbrace                                                { print(" P_EXPR_BASIC : '{' p_kwargs rbrace "); }
        | tLCURLY rbrace                                                         { print(" P_EXPR_BASIC : '{' rbrace "); }
        | tLPAREN p_as rparen                                                    { print(" P_EXPR_BASIC : '(' p_as rparen "); }

p_expr_basic_first : p_primitive                                                 { print(" P_EXPR_BASIC_FIRST : p_primitive"); }
        | p_primitive tDOT2 p_primitive                                          { print(" P_EXPR_BASIC_FIRST : p_primitive '..' p_primitive "); }
        | p_primitive tDOT3 p_primitive                                          { print(" P_EXPR_BASIC_FIRST : p_primitive '...' p_primitive "); }
        | p_primitive tDOT3                                                      { print(" P_EXPR_BASIC_FIRST : p_primitive '...' "); }
        | tCARET p_primitive                                                     { print(" P_EXPR_BASIC_FIRST : '^' p_primitive  "); }
        | p_const                                                                { print(" P_EXPR_BASIC_FIRST : p_const "); }
        | tDOT2 p_primitive                                                      { print(" P_EXPR_BASIC_FIRST : '..' p_primitive "); }
        | tDOT3 p_primitive                                                      { print(" P_EXPR_BASIC_FIRST : '...' p_primitive "); }

p_primitive : tIDENTIFIER                                                        { print("P_PRIMITIVE : tIDENTIFER "); }

p_args : p_as                                                                    { print(" P_ARGS : p_as "); }
        | p_args_head                                                            { print(" P_ARGS : p_args_head "); }
        | p_args_head p_as                                                       { print(" P_ARGS : p_args_head p_as "); }
        | p_args_head tSTAR tIDENTIFIER                                          { print(" P_ARGS : p_args_head '*' tIDENTIFIER "); }
        | p_args_head tSTAR tIDENTIFIER tCOMMA p_args_post                       { print(" P_ARGS : p_args_head '*' tIDENTIFIER ',' p_args_post"); }
        | p_args_head tSTAR                                                      { print(" P_ARGS : p_args_head '*' "); }
        | p_args_head tSTAR tCOMMA p_args_post                                   { print(" P_ARGS : p_args_head '*' ',' p_args_post "); }
        | p_args_tail                                                            { print(" P_ARGS : p_args_tail "); }

p_args_head : p_as tCOMMA                                                        { print(" P_ARGS_HEAD : p_as ',' "); }
        | p_args_head p_as tCOMMA                                                { print(" P_ARGS_HEAD : p_args_head p_as ',' "); }

p_args_tail : tSTAR tIDENTIFIER                                                  { print(" P_ARGS_TAIL : '*' tIDENTIFIER "); }
        | tSTAR tIDENTIFIER tCOMMA p_args_post                                   { print(" P_ARGS_TAIL : '*' tIDENTIFIER ',' p_args_post "); }
        | tSTAR                                                                  { print(" P_ARGS_TAIL : '*' "); }
        | tSTAR tCOMMA p_args_post                                               { print(" P_ARGS_TAIL : '*' ',' p_args_post "); }

p_args_post : p_as                                                               { print(" P_ARGS_POST : p_as "); }
        | p_args_post tCOMMA p_as                                                { print(" P_ARGS_POST : p_args_post ',' p_as "); }

p_kwargs : p_kwarg tCOMMA p_kwrest                                               { print(" P_KWARGS : p_kwarg ',' p_kwrest "); }
        | p_kwarg                                                                { print(" P_KWARGS : p_kwarg "); }
        | p_kwrest                                                               { print(" P_KWARGS : p_kwrest "); }
        | p_kwarg tCOMMA p_kwnorest                                              { print(" P_KWARGS : p_kwarg ',' p_kwnorest "); }
        | p_kwnorest                                                             { print(" P_KWARGS : p_kwnorest "); }

p_kwarg : p_kw                                                                   { print(" P_KWARG : p_kw "); }
        | p_kwarg tCOMMA p_kw                                                    { print(" P_KWARG : p_kwarg ',' p_kw"); }

p_kw : p_kw_label p_as                                                           { print(" P_KW : p_kw_label p_as "); }
        | p_kw_label                                                             { print(" P_KW : p_kw_label "); }

p_kw_label : keyword_tLABLE                                                      { print(" P_KW_LABLE : LABLE "); }   

p_kwrest : kwrest_mark tIDENTIFIER                                               { print(" P_KWREST : kwrest_mark tIDENTIFIER "); }
        | kwrest_mark                                                            { print(" P_KWREST : kwrest_mark "); }

p_kwnorest : kwrest_mark keyword_nil                                             { print(" P_KWNOREST : kwrest_mark NIL "); }

p_const : tCOLON2 tCONSTANT                                                      { print(" P_CONST : '::' tCONSTANT "); }
        | p_const tCOLON2 tCONSTANT                                              { print(" P_CONST : p_const '::' tCONSTANT "); }
        | tCONSTANT                                                              { print(" P_CONST : tCONSTANT "); }

p_case_body_second: mb_else                                                      { print(" P_CASE_BODY_SECOND : mb_else "); }
        | p_case_body                                                            { print(" P_CASE_BODY_SECOND : p_case_body "); }

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

mb_rescue : keyword_rescue exc_list exc_var then_with_term mb_terms compstmt mb_rescue { print(" MB_RESCUE : RESCUE exc_list exc_var then_with_term mb_terms compstmt RESCUE "); }
        | none                                                                         { print(" MB_RESCUE : none"); }

exc_list : expr_pr                                                                     { print(" EXC_LIST : expr_pr "); }
        | mrhs                                                                         { print(" EXC_LIST : mrhs "); }
	| none                                                                         { print(" EXC_LIST : none "); }

exc_var	: tASSOC lhs                                                             { print(" EXC_VAR : '=>' lhs "); }
	| none                                                                   { print(" EXC_VAR : none "); }


mb_ensure : keyword_ensure mb_terms compstmt                                     { print(" MB_ENSURE : ENSURE mb_terms compstmt "); }
        | none                                                                   { print(" MB_ENSURE : none "); }

/////////////////////////////////////////////////////////////////////////////

lhs : keyword_variable1                                                          { print(" LHS : keyword_variable1 "); }
        | atom call_op tIDENTIFIER                                               { print(" LHS : atom call_op tIDENTIFIER"); }
        | atom tDOT2 tIDENTIFIER                                                 { print(" LHS : atom '..' tIDENTIFIER"); }
        | atom call_op tCONSTANT                                                 { print(" LHS : atom call_op tCONSTANT "); }
        | atom tDOT2 tCONSTANT                                                   { print(" LHS : atom '..' tCONSTANT "); }
        | tCOLON2 tCONSTANT                                                      { print(" LHS : '::' tCONSTANT"); }
        | none                                                                   { print(" LHS : none "); }

fname : tIDENTIFIER                                                              { print(" FNAME : tIDENTIFIER  "); }
        | tCONSTANT                                                              { print(" FNAME : tCONSTANT  "); }
        | tOP_ASGN                                                               { print(" FNAME : tOP_ASGN  "); }
        | reswords                                                               { print(" FNAME : reswords  "); }


reswords : keyword_lBEGIN                                                        { print(" reswords_BIG_BEGIN "); }
        | keyword_lEND                                                           { print(" reswords_BIG_END"); }
        | keyword_alias                                                          { print(" reswords_ALIAS "); }
        | keyword_and                                                            { print(" reswords_AND "); }
        | keyword_begin                                                          { print(" reswords_BEGIN "); }
        | keyword_break                                                          { print(" reswords_BREAK "); }
        | keyword_case                                                           { print(" reswords_CASE "); }
        | keyword_class                                                          { print(" reswords_CLASS "); }
        | keyword_def                                                            { print(" reswords_DEF "); }
        | keyword_defined                                                        { print(" reswords_DEFINED "); }
        | keyword_do                                                             { print(" reswords_DO "); }
        | keyword_else                                                           { print(" reswords_ELSE "); }
        | keyword_elsif                                                          { print(" reswords_ELSIF "); }
        | keyword_end                                                            { print(" reswords_END "); }
        | keyword_ensure                                                         { print(" reswords_ENSURE "); }
        | keyword_true                                                           { print(" reswords_TRUE "); }
        | keyword_false                                                          { print(" reswords_FALSE "); }
        | keyword_for                                                            { print(" reswords_FOR "); }
        | keyword_in                                                             { print(" reswords_IN "); }
        | keyword_module                                                         { print(" reswords_MODULE "); }
        | keyword_next                                                           { print(" reswords_NEXT "); }
        | keyword_nil                                                            { print(" reswords_NIL "); }
        | keyword_not                                                            { print(" reswords_NOT "); }
        | keyword_or                                                             { print(" reswords_OR "); }
        | keyword_redo                                                           { print(" reswords_REDO "); }
        | keyword_rescue                                                         { print(" reswords_RESCUE "); }
        | keyword_retry                                                          { print(" reswords_RETRY "); }
        | keyword_return                                                         { print(" reswords_RETURN "); }
        | keyword_self                                                           { print(" reswords_SELF "); }
        | keyword_super                                                          { print(" reswords_SUPER "); }
        | keyword_then                                                           { print(" reswords_THEN "); }
        | keyword_yield                                                          { print(" reswords_YIELD "); }
        | keyword_if                                                             { print(" reswords_IF "); }
        | keyword_unless                                                         { print(" reswords_UNLESS "); }
        | keyword_while                                                          { print(" reswords_WHILE "); }
        | keyword_until                                                          { print(" reswords_UNTIL "); }
        | keyword_when                                                           { print(" reswords_WHEN "); }
        | keyword_undef                                                          { print(" reswords_UNDEF "); }
        | keyword_command                                                        { print(" reswords_COMMAND "); }

keyword_variable1 : keyword_nil                                                  { print(" keyword_variable1: NIL "); }
        | keyword_self                                                           { print(" keyword_variable1: SELF "); }
        | keyword_true                                                           { print(" keyword_variable1: TRUE "); }
        | keyword_false                                                          { print(" keyword_variable1: FALSE "); }
        | keyword_public                                                         { print(" keyword_variable1: PUBLIC "); }
        | keyword_private                                                        { print(" keyword_variable1: PRIVATE "); }
//\\\\\\\\\\\\\\\\\\\\\\\\\\\//

args_tail: f_kwarg tCOMMA f_kwrest mb_f_block_arg                                { print(" ARGS_TAIL : f_kwarg ',' f_kwrest mb_f_block_arg "); }
        | f_kwarg  mb_f_block_arg                                                { print(" ARGS_TAIL : f_kwarg  mb_f_block_arg"); }
        | f_kwrest mb_f_block_arg                                                { print(" ARGS_TAIL : f_kwrest mb_f_block_arg"); }
        | f_no_kwarg mb_f_block_arg                                              { print(" ARGS_TAIL : f_no_kwarg mb_f_block_arg"); }
        | f_block_arg                                                            { print(" ARGS_TAIL : f_block_arg"); }

mb_args_tail : tCOMMA args_tail                                                  { print(" MB_ARGS_TAIL : ',' args_tail " ); }
        | tEQL args_tail                                                         { print(" MB_ARGS_TAIL : '=' args_tail " ); }
        | none                                                                   { print(" MB_ARGS_TAIL : none " ); } 

f_args : f_arg tCOMMA f_optarg tCOMMA f_rest_arg mb_args_tail                    { print(" F_ARGS : f_arg ',' f_optarg ',' f_rest_arg mb_args_tail " ); }
        | f_arg tCOMMA f_optarg tCOMMA f_rest_arg tCOMMA f_arg mb_args_tail      { print(" F_ARGS : f_arg ',' f_optarg ',' f_rest_arg ',' f_arg mb_args_tail" ); }
        | f_arg tCOMMA f_optarg mb_args_tail                                     { print(" F_ARGS : f_arg ',' f_optarg mb_args_tail" ); }
        | f_arg tCOMMA f_optarg tCOMMA f_arg mb_args_tail                        { print(" F_ARGS : f_arg ',' f_optarg ',' f_arg mb_args_tail" ); }
        | f_arg tCOMMA f_rest_arg mb_args_tail                                   { print(" F_ARGS : f_arg ',' f_rest_arg mb_args_tail" ); }
        | f_arg tCOMMA f_rest_arg tCOMMA f_arg mb_args_tail                      { print(" F_ARGS : f_arg ',' f_rest_arg ',' f_arg mb_args_tail " ); }
        | f_arg mb_args_tail                                                     { print(" F_ARGS : f_arg mb_args_tail" ); }
        | f_optarg tCOMMA f_rest_arg mb_args_tail                                { print(" F_ARGS : f_optarg ',' f_rest_arg mb_args_tail " ); }
        | f_optarg tCOMMA f_rest_arg tCOMMA f_arg mb_args_tail                   { print(" F_ARGS : f_optarg ',' f_rest_arg ',' f_arg mb_args_tail " ); }
        | f_optarg mb_args_tail                                                  { print(" F_ARGS : f_optarg mb_args_tail " ); }
        | f_optarg tCOMMA f_arg mb_args_tail                                     { print(" F_ARGS : f_optarg ',' f_arg mb_args_tail " ); }
        | f_rest_arg mb_args_tail                                                { print(" F_ARGS : f_rest_arg mb_args_tail " ); }
        | f_rest_arg tCOMMA f_arg mb_args_tail                                   { print(" F_ARGS : f_rest_arg ',' f_arg mb_args_tail   " ); }
	| args_tail                                                              { print(" F_ARGS : args_tail " ); }
        | none                                                                   { print(" F_ARGS : none " ); }

f_arg_item : litera                                                              { print(" F_ARG_ITEM : litera" ); }
        | tLPAREN f_margs rparen                                                 { print(" F_ARG_ITEM : '(' f_margs rparen" ); }


f_arg : f_arg_item                                                               { print(" F_ARG :  f_arg_item " ); }
	| f_arg tCOMMA f_arg_item                                                { print(" F_ARG : ',' f_arg_item " ); }
        | f_arg tDOT f_arg_item                                                  { print(" F_ARG : '.' f_arg_item " ); }


        
f_kw : keyword_tLABLE expr_pr                                                    { print(" F_KW : LABLE expr_pr " ); }
        | keyword_tLABLE                                                         { print(" F_KW : LABLE " ); }

f_block_kw : keyword_tLABLE atom                                                 { print(" F_BLOCK_KW : LABLE atom " ); }
        | keyword_tLABLE                                                         { print(" F_BLOCK_KW : LABLE " ); }

f_block_kwarg : f_block_kw                                                       { print(" F_BLOCK_KWARG : f_block_kw " ); }
        | f_block_kwarg tCOMMA f_block_kw                                        { print(" F_BLOCK_KWARG : f_block_kwarg ',' f_block_kw " ); }


f_kwarg : f_kw                                                                   { print(" F_KWARG : f_kw " ); }
        | f_kwarg tCOMMA f_kw                                                    { print(" F_KWARG : f_kwarg ',' f_kw  " ); }

kwrest_mark : tPOW                                                               { print(" KWREST_MARK : '**' "); }    

f_no_kwarg : kwrest_mark keyword_nil                                             { print(" F_NO_KWARG : kwrest_mark NIL "); }

f_kwrest : kwrest_mark tIDENTIFIER                                               { print(" F_KWREST : kwrest_mark tIDENTIFIER "); }
        | kwrest_mark                                                            { print(" F_KWREST : kwrest_mark "); }
 
f_opt : tIDENTIFIER tEQ expr_pr                                                  { print(" F_OPT : tIDENTIFIER '=' atom "); } 

f_block_opt : tIDENTIFIER tEQ atom                                               { print(" F_BLOCK_OPT : tIDENTIFIER '=' atom "); }   
 
f_block_optarg : f_block_opt                                                     { print(" F_BLOCK_OPTARG : f_block_opt  "); }
        | f_block_optarg tCOMMA f_block_opt                                      { print(" F_BLOCK_OPTARG : f_block_optarg ',' f_block_opt  "); }
 
f_optarg : f_opt                                                                 { print(" F_OPTARG : f_opt  "); }  
        | f_optarg tCOMMA f_opt                                                  { print(" F_OPTARG : f_optarg ',' f_opt "); }  

f_rest_arg : tSTAR tIDENTIFIER                                                   { print(" F_REST_ARG : '*' tIDENTIFIER "); }  
        | tSTAR                                                                  { print(" F_REST_ARG : '*' "); }

f_block_arg : tAMPER tIDENTIFIER                                                 { print(" F_BLOCK_ARG : '&' tIDENTIFIER "); }    

mb_f_block_arg	: tCOMMA f_block_arg                                             { print(" MB_F_BLOCK_ARG : ',' f_block_arg  "); }
	| none                                                                   { print(" MB_F_BLOCK_ARG : none  "); }

assoc_list : none                                                                { print(" ASSOC_LIST : none  "); } 
        | assocs trailer                                                         { print(" ASSOC_LIST : assocs trailer  "); }
 
/////////////////////////////////////////////////////////////////////////////////////////////
assocs : assoc                                                                   { print(" ASSOCS : assoc"); }
        | assocs tCOMMA assoc                                                    { print(" ASSOCS : assocs ',' assoc"); }
        
assoc : expr_pr tASSOC atom                                                      { print(" ASSOC : expr_pr '=>' expr_pr"); }
        | keyword_tLABLE expr_pr                                                 { print(" ASSOC : LABLE expr_pr"); }
        | tDSTAR expr_pr                                                         { print(" ASSOC : '**' expr_pr"); }
        | tPOW                                                                   { print(" ASSOC : '**' "); }
        | assoc tASSOC expr_pr
////////////////////////////////////////////////////////////////////////////////////////////////////////

superclass : tLT expr term                                                       { print(" SUPERCLASS : '<' expr term  "); }
        | none                                                                   { print(" SUPERCLASS : none  "); }

///////////////////////////////////////////////////////////////////
call_op : tDOT                                                                   { print(" CALL_OP : '.'  "); }
        | tANDDOT                                                                { print(" CALL_OP : '&.'  "); }
        
call_op2 : call_op                                                               { print(" CALL_OP2 : call_op  "); }
        | tCOLON2                                                                { print(" CALL_OP2 : '::'  "); }

mb_terms2 : none                                                                 { print(" MB_TERMS2 : none "); } 
        | tCOLON2                                                                { print(" MB_TERMS2 : '::' "); } 
        | tDOT                                                                   { print(" MB_TERMS2 : '.' "); } 
        | tCOLON                                                                 { print(" MB_TERMS2 : ':' "); } 

mb_terms : none                                                                  { print(" MB_TERMS : none "); }  
        | terms                                                                  { print(" MB_TERMS : terms "); }

mb_nl : none                                                                     { print(" MB_NL : none "); }
        | '\n'                                                                   { print(" MB_NL : 'n'"); } 

rparen : mb_nl tRPAREN                                                           { print(" RPAREN : mb_nl ')'"); }

rbracket : mb_nl tRBRACK                                                         { print(" RBRACKET : mb_nl ']'"); }

rbrace : mb_nl tRCURLY                                                           { print(" RBRACE : mb_nl '}' "); }

trailer : mb_nl                                                                  { print(" TRAILER : 'n' "); }
        | tCOMMA                                                                 { print(" TRAILER : ',' "); }

term : tSEMI                                                                     { print(" TERM : ';'"); }
        | '\n'                                                                   { print(" TERM | 'n'"); }

terms : term                                                                     { print(" TERMS: term "); }
        | terms '\n'                                                             { print(" TERMS | terms 'n' "); }
      
none :                                                                           { print(" NONE LAST"); }
//////////////////////////////////////////////////////////////////////////////////////////////////////////////