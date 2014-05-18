%{
  /* Prologue */

  #pragma warning(disable : 4065)

  #include "common.h"
  #include "parser.h"

  #define YYSTYPE Term

  int yylex (void);
  void yyerror (char const *);

%}

//%glr-parser

%token LOWER_CASE_ID
%token SYMBOL
%token OP_FN_NAME
%token BUILTIN
%token LABEL
%token NUMBER
%token CHAR
%token STRING
%token MIXED_CASE_ID
%token UPPER_CASE_ID

%token RIGHT_ARROW          "->"
%token LEFT_ARROW           "<-"

%token DOUBLE_RIGHT_ARROW   "=>"

%token DOUBLE_DOT           ".."
%token ELLIPSIS             "..."

%token ASSIGNMENT           ":="

%token LE                   "<="
%token GE                   ">="
%token EQ                   "=="
%token NEQ                  "/="
%token IN                   "::"

%token OR                   "\\/"
%token CROSSED_LEFT_ARROW   "</-"

%token KW_AND               "and"
%token KW_AS                "as"
%token KW_ASSERT            "assert"
%token KW_BREAK             "break"
%token KW_CASE              "case"
%token KW_DEF               "def"
%token KW_DISABLED          "disabled"
%token KW_DO                "do"
%token KW_ELSE              "else"
%token KW_ELIF              "elif"
%token KW_END               "end"
%token KW_FAIL              "fail"
%token KW_FALSE             "false"
%token KW_FOR               "for"
%token KW_FROM              "from"
%token KW_IF                "if"
%token KW_IN                "in"
%token KW_IS                "is"
%token KW_ITSELF            "itself"
%token KW_LET               "let"
%token KW_LOOP              "loop"
%token KW_MATCH             "match"
%token KW_NIL               "nil"
%token KW_NOT               "not"
%token KW_OR                "or"
%token KW_PRINT             "print"
%token KW_REPLACE           "replace"
%token KW_RETRIEVE          "retrieve"
%token KW_RETURN            "return"
%token KW_SELECT            "select"
%token KW_SUBTYPECHECK      "subtypecheck"
%token KW_THEN              "then"
%token KW_TESTCASES         "testcases"
%token KW_TRACE             "trace"
%token KW_TRUE              "true"
//%token KW_TRY               "try"
%token KW_TYPE              "type"
%token KW_USING             "using"
%token KW_WHEN              "when"
%token KW_WHERE             "where"
%token KW_WHILE             "while"
%token KW_WITH              "with"


// Java operator precedence
// unary:                   + - ++ -- (what about <!> ?)
// arithmetic (and shift):  * / % + -
// relational:              > < >= <= == !=
// logical (and bitwise):   && || & | ^
// conditional:             (? :)
// assignments:             =


%nonassoc "let"
%nonassoc "where"
%nonassoc "is"

%nonassoc '@'

%left "\\/"

%left "<-"

%left KW_AND KW_OR

%left '=' EQ NEQ

%left '<' '>' LE GE
%left '+' '-' '&'
%left '*' '/'
%left NEG KW_NOT

%right '^'

%left IN

%left '.'


%%  /* Grammar rules */


/**********************************************************************/

src_file:             decl                                  {consume_decl($1);                                }
                    | src_file decl                         {consume_decl($2);                                }
                    ;

decl:                 typedef
                    | fndef
                    | using_block
                    | test_block
                    | subtype_decl
                    ;

/************************ SUBTYPE DECLARATIONS ************************/

subtype_decl:
    "subtypecheck" type '<' type ';'                        {$$ = mk_subtype_decl($2, $4);                    }

/***************************** TESTCASES ******************************/

test_block:
    "testcases" test_instrs "end"                           {$$ = mk_test_block(true, $2);                    }
  | "testcases" "disabled" test_instrs "end"                {$$ = mk_test_block(false, $3);                   }
  ;

test_instrs:
    test_instr                                              {$$ = mk_seq($1);                                 }
  | test_instrs test_instr                                  {$$ = mk_seq($1, $2);                             }
  ;

test_instr:
    expr ';'                                                {$$ = mk_test_instr_bool($1);                     }
  | "print" expr ';'                                        {$$ = mk_test_instr_print($2);                    }
  | "for" '(' for_iters ')' test_instrs ';'                 {$$ = mk_test_instr_loop($3, $5);                 }
  | vid ":=" expr ';'                                       {$$ = mk_test_instr_assignment($1, $3);           }
  ;

/******************************* BLOCKS *******************************/

using_block:
    "using" signatures '{' fndefs '}'                       {$$ = mk_using_block($2, $4);                     }
  | "using" '{' signatures ';' fndefs '}'                   {$$ = mk_using_block($3, $5);                     }
  ;

signature:
    type fnid                                               {$$ = mk_signature($2, $1);                       }
  | type fnid '(' types ')'                                 {$$ = mk_signature($2, $4, $1);                   }

signatures:
    signature                                               {$$ = mk_seq($1);                                 }
  | signatures ',' signature                                {$$ = mk_seq($1, $3);                             }
  ;

/****************************** TYPEDEFS ******************************/

typedef:
    "type" tname '=' pretypes ';'                           {$$ = mk_typedef($2, $4);                         }
  | "type" tname '[' tvars ']' '=' pretypes ';'             {$$ = mk_typedef_par($2, $4, $7);                 }
  ;

tvars:
    tvar                                                    {$$ = mk_seq($1);                                 }
  | tvars ',' tvar                                          {$$ = mk_seq($1, $3);                             }
  ;

/***************************** FUNCTIONS ******************************/

fndef:
    type fnid '=' expr ';'                                    {$$ = mk_fndef($1, $2, $4);                       }
  | type fnid '(' fnargs ')' '=' expr ';'                     {$$ = mk_fndef($1, $2, $4, $7);                   }
  | type fnid '(' fnargs ')' '=' expr "let" stmts ';'         {$$ = mk_fndef($1, $2, $4, mk_expr_let($7, $9));  }
  | type fnid '(' fnargs ')' '{' stmts '}'                    {$$ = mk_fndef_proc($1, $2, $4, $7);              }
  | type fnid '(' fnargs ')' '{' stmts fndefs '}'             {$$ = mk_fndef_proc($1, $2, $4, $7, $8);          }
  | type fnid '(' fnargs ')' ':' match_branches ';'           {$$ = mk_fndef_switch($1, $2, $4, $7);            }
  | type fnid '(' fnargs ')' ':' match_branches "where" fndefs ';'  {$$ = mk_fndef_switch($1, $3, $6, $8, $9);  }

  | fnid '=' expr ';'                                         {$$ = mk_fndef(null(), $1, $3);                   }
  | fnid '(' fnargs ')' '=' expr ';'                          {$$ = mk_fndef(null(), $1, $3, $6);               }
  | fnid '(' fnargs ')' '{' stmts '}'                         {$$ = mk_fndef_proc(null(), $1, $3, $6);          }
  | fnid '(' fnargs ')' '{' stmts fndefs '}'                  {$$ = mk_fndef_proc(null(), $1, $3, $6, $7);      }
  | fnid '(' fnargs ')' ':' match_branches ';'                {$$ = mk_fndef_switch(null(), $1, $3, $6);        }
  | fnid '(' fnargs ')' ':' match_branches "where" fndefs';'  {$$ = mk_fndef_switch(null(), $1, $3, $6, $8);    }
  | fnid '(' fnargs ')' '=' expr "let" stmts ';'              {$$ = mk_fndef(null(), $1, $3, mk_expr_let($6, $8));}
  ;

fnarg:
    '_'                                                     {$$ = mk_fnarg(null(), null());                   }
  | vid                                                     {$$ = mk_fnarg(null(), $1);                       }
  | type                                                    {$$ = mk_fnarg($1,     null());                   }
  | type vid                                                {$$ = mk_fnarg($1,     $2);                       }
  ;

fnargs:
    fnarg                                                   {$$ = mk_seq($1);                                 }
  | fnargs ',' fnarg                                        {$$ = mk_seq($1, $3);                             }
  ;

fndefs:
    fndef                                                   {$$ = mk_seq($1);                                 }
  | fndefs fndef                                            {$$ = mk_seq($1, $2);                             }
  ;

/***************************** STATEMENTS *****************************/

stmt:
    "return" expr ';'                                 {$$ = mk_stmt_return($2);                    }
  | "return" expr "if" expr ';'                       {$$ = mk_stmt_return_if($2, $4);             }

  | vid ":=" expr ';'                                 {$$ = mk_stmt_assignment($1, $3);            }
  //| vid ':' type ":=" expr ';'                        {$$ = mk_stmt_typed_assignment($1, $3, $5);  }

  | vid ":=" expr "if" expr ';'                       {$$ = mk_stmt_assignment_if($1, $3, $5);     }

  | "if" '(' expr ')' stmts elifs ';'                 {$$ = mk_stmt_if($3, $5, $6);                }
  | "if" '(' expr ')' stmts elifs "else" stmts ';'    {$$ = mk_stmt_if($3, $5, $6, $8);            }

  | "loop" stmts ';'                                  {$$ = mk_stmt_loop($2);                      }
  | "loop" stmts "while" '(' expr ')' ';'             {$$ = mk_stmt_loop($2, $5);                  }
  | "while" '(' expr ')' stmts ';'                    {$$ = mk_stmt_while($3, $5);                 }

  | "let" '(' let_fndefs ')' stmts ';'                {$$ = mk_stmt_let($3, $5);                   }
  //| "let" let_fndefs ';'                              {$$ = mk_stmt_let($2);                       }

  | "break" ';'                                       {$$ = mk_stmt_break();                       }
  | "break" "if" expr ';'                             {$$ = mk_stmt_break_if($3);                  }

  | "for" '(' for_iters ')' stmts ';'                 {$$ = mk_stmt_for($3, $5);                   }

  | "fail" ';'                                        {$$ = mk_stmt_fail();                        }
  | "fail" "if" expr ';'                              {$$ = mk_stmt_fail_if($3);                   }

  | "assert" expr ';'                                 {$$ = mk_stmt_assert($2);                    }

  | "print" expr ';'                                  {$$ = mk_stmt_print($2);                     }
  | "print" expr "if" expr ';'                        {$$ = mk_stmt_print_if($2, $4);              }
  ;

elifs:
                                                      {$$ = mk_seq_empty();                        }
  | elifs "elif" '(' expr ')' stmts                   {$$ = mk_seq($1, mk_elif($4, $6));           }
  ;

for_iter:
    vid ':' expr                                      {$$ = mk_for_iter($1, $3);                   }
  | vid ',' vid ':' expr                              {$$ = mk_for_iter($1, $3, $5);               }
  | vid '=' expr ".." expr                            {$$ = mk_for_iter_range($1, $3, $5);         }
  ;

for_iters:
    for_iter                                          {$$ = mk_seq($1);                            }
  | for_iters ';' for_iter                            {$$ = mk_seq($1, $3);                        }
  ;

stmts:
    stmt                                              {$$ = mk_seq($1);                            }
  | stmts stmt                                        {$$ = mk_seq($1, $2);                        }
  ;

/****************************** PATTERNS ******************************/

// WHAT ABOUT true, false AND nil AS PATTERNS?

pattern:
    type                                    {$$ = mk_ptrn_type($1);                               }
  | vid                                     {$$ = mk_ptrn_var($1);                                }
  | pattern vid                             {$$ = mk_ptrn_ptrn_var($1, $2);                       }
  | ctor                                    {$$ = mk_ptrn_ctor($1);                               }
  | snum                                    {$$ = mk_ptrn_num($1);                                }
  | '_'                                     {$$ = mk_ptrn_jolly();                                }
  | '#' vid                                 {$$ = mk_ptrn_expr($2);                               }
  //| '(' labptrns ')'                        {$$ = mk_ptrn_tuple($2, false);                       }
  //| '(' labptrns ',' "..." ')'              {$$ = mk_ptrn_tuple($2, true);                        }
  | pid '(' ')'                             {$$ = mk_ptrn_tag_ptrn($1, mk_ptrn_jolly());          }
  | pid '(' pattern ')'                     {$$ = mk_ptrn_tag_ptrn($1, $3);                       }
  //| pid '(' labptrns ')'                    {$$ = mk_ptrn_tag_ptrn($1, mk_ptrn_tuple($3, false)); }
  //| pid '(' labptrns ',' "..." ')'          {$$ = mk_ptrn_tag_ptrn($1, mk_ptrn_tuple($3, true));  }
  | vid '@' vid                             {$$ = mk_ptrn_tag_obj($1, $3);                        }
  ;

//labptrns:
//    lab pattern                             {$$ = mk_seq(mk_lab_ptrn($1, $2));                    }
//  | labptrns ',' lab pattern                {$$ = mk_seq($1, mk_lab_ptrn($3, $4));                }
//  ;

patterns:
    pattern                                 {$$ = mk_seq($1);                                     }
  | patterns ',' pattern                    {$$ = mk_seq($1, $3);                                 }
  ;

/******************************* TYPES ********************************/

type:
    ntltype
  | type '*'                                      {$$ = mk_type_set($1, false);                     }
  | type '+'                                      {$$ = mk_type_set($1, true);                      }
  ;

ntltype:
    tname                                         {$$ = mk_type_ref($1);                            }
  | tvar                                          {$$ = mk_type_var($1);                            }
  | tname '[' types ']'                           {$$ = mk_type_ref($1, $3);                        }
  | '<' pretypes '>'                              {$$ = mk_type_inline($2);                         }
  | '<' '+' '>'                                   {$$ = mk_type_symb();                             }

  | '[' '*' ".." '*' ']'                          {$$ = mk_type_int();                              }
  | '[' snum ".." '*' ']'                         {$$ = mk_type_low_bounded_int($2);                }
  | '[' '*' ".." snum ']'                         {$$ = mk_type_up_bounded_int($4);                 }
  | '[' snum ".." snum ']'                        {$$ = mk_type_bounded_int($2, $4);                }

  | '[' ntltype '*' ']'                           {$$ = mk_type_seq($2, false);                     }
  | '[' ntltype '+' ']'                           {$$ = mk_type_seq($2, true);                      }
//  | '[' ntltypes ']'                              {$$ = mk_type_fixed_seq($2);                      }

  | '(' type '*' ')'                              {$$ = mk_type_set($2, false);                     }
  | '(' type '+' ')'                              {$$ = mk_type_set($2, true);                      }

  | '(' type "=>" type ')'                        {$$ = mk_type_map($2, $4);                        }
  | '(' labtypes ')'                              {$$ = mk_type_tuple($2);                          }

  | '(' ntltype '@' type ')'                      {$$ = mk_type_tagged_obj($2, $4);                 }
  ;

pretype:
    type                                          {$$ = mk_pretype_type($1);                        }
  | '{' '}'                                       {$$ = mk_pretype_empty_set();                     }
  | '[' ']'                                       {$$ = mk_pretype_empty_seq();                     }
  | '(' ')'                                       {$$ = mk_pretype_empty_map();                     }
  | pid                                           {$$ = mk_pretype_sing($1);                        }
  | pid '(' pretype ')'                           {$$ = mk_pretype_tagged_obj($1, $3);              }
  | pid '(' labtypes ')'                          {$$ = mk_pretype_tagged_tuple($1, $3);            }
  ;

labtype:
    lab pretype                                   {$$ = mk_labtype($1, $2, false);                  }
  | lab pretype '?'                               {$$ = mk_labtype($1, $2, true);                   }
  ;

types:
    type                                          {$$ = mk_seq($1);                                 }
  | types ',' type                                {$$ = mk_seq($1, $3);                             }
  ;

//ntltypes:
//    ntltype                                       {$$ = mk_seq($1);                                 }
//  | ntltypes ',' ntltype                          {$$ = mk_seq($1, $3);                             }
//  ;

pretypes:
    pretype                                       {$$ = mk_seq($1);                                 }
  | pretypes ',' pretype                          {$$ = mk_seq($1, $3);                             }
  ;

labtypes:
    labtype                                       {$$ = mk_seq($1);                                 }
  | labtypes ',' labtype                          {$$ = mk_seq($1, $3);                             }
  ;

/**************************** EXPRESSIONS *****************************/

expr:
    num                                                     {$$ = mk_expr_num($1);                            }
  | ctor                                                    {$$ = mk_expr_ctor($1);                           }
  | str                                                     {$$ = mk_expr_str($1);                            }

  | "true"                                                  {$$ = mk_expr_ctor($1);                           }
  | "false"                                                 {$$ = mk_expr_ctor($1);                           }
  | "nil"                                                   {$$ = mk_expr_ctor($1);                           }

  | '{' '}'                                                 {$$ = mk_expr_set(mk_seq_empty());                }
  | '{' subexprs '}'                                        {$$ = mk_expr_set($2);                            }

  | '(' ')'                                                 {$$ = mk_expr_map(mk_seq_empty());                }
  | '(' map_entries ')'                                     {$$ = mk_expr_map($2);                            }
  | '(' labexprs ')'                                        {$$ = mk_expr_tuple($2);                          }

  | '[' ']'                                                 {$$ = mk_expr_seq(mk_seq_empty());                }
  | '[' exprs ']'                                           {$$ = mk_expr_seq($2);                            }
  | '[' exprs '|' expr ']'                                  {$$ = mk_expr_seq($2, $4);                        }

  | expr '@' expr                                           {$$ = mk_expr_tag_obj($1, $3);                    }
  | ctor '(' expr ')'                                       {$$ = mk_expr_tag_obj(mk_expr_ctor($1), $3);      }
  //| expr '(' expr ')'                                       {$$ = mk_expr_tag_obj($1, $3);                    }
  | ctor '(' labexprs ')'                                   {$$ = mk_expr_tag_map($1, $3);                    }
  | rid '(' labexprs ')'                                    {$$ = mk_expr_tag_map($1, $3);                    }

  /*** IF THE NAME IS "in" IT CANNOT BE A VARIABLE ***/
  | rid                                                     {$$ = mk_expr_const_or_var($1);                   }
  | rid '(' exprs ')'                                       {$$ = mk_expr_fn_call($1, $3);                    }
  | rid '(' exprs ';' let_fndefs ')'                        {$$ = mk_expr_fn_call_impl($1, $3, $5);           }
  | rid '(' ';' let_fndefs ')'                              {$$ = mk_expr_fn_call_impl($1, mk_seq_empty(), $4); }
  | op '(' exprs ')'                                        {$$ = mk_expr_fn_call($1, $3);                    }
  | builtin '(' exprs ')'                                   {$$ = mk_expr_builtin_call($1, $3);               }

  | '(' expr ')'                                            {$$ = $2;                                         }

  | '-' expr                        %prec NEG               {$$ = mk_expr_op($1, $2);                         }

  | expr '+' expr                                           {$$ = mk_expr_op($2, $1, $3);                     }
  | expr '-' expr                                           {$$ = mk_expr_op($2, $1, $3);                     }
  | expr '*' expr                                           {$$ = mk_expr_op($2, $1, $3);                     }
  | expr '/' expr                                           {$$ = mk_expr_op($2, $1, $3);                     }
  | expr '^' expr                                           {$$ = mk_expr_op($2, $1, $3);                     }
  | expr '&' expr                                           {$$ = mk_expr_op($2, $1, $3);                     }
  | expr '<' expr                                           {$$ = mk_expr_op($2, $1, $3);                     }
  | expr '>' expr                                           {$$ = mk_expr_op($2, $1, $3);                     }
  | expr "<=" expr                                          {$$ = mk_expr_op($2, $1, $3);                     }
  | expr ">=" expr                                          {$$ = mk_expr_op($2, $1, $3);                     }

  | expr "and" expr                                         {$$ = mk_expr_and($1, $3);                        }
  | expr "or" expr                                          {$$ = mk_expr_or($1, $3);                         }
  | "not" expr                                              {$$ = mk_expr_not($2);                            }

  | expr "==" expr                                          {$$ = mk_expr_eq($1, $3);                         }
  | expr "/=" expr                                          {$$ = mk_expr_neq($1, $3);                        }

  | expr "::" type                                          {$$ = mk_expr_type_test($1, $3);                  }

  | expr '.' uqctor                                         {$$ = mk_expr_dot_acc($1, $3);                    }
  | expr '.' uqctor '?'                                     {$$ = mk_expr_dot_acc_test($1, $3);               }
  //| expr '.' '*'                                            {$$ = mk_expr_dot_acc_star($1);                   }

  | vid '[' exprs ']'                                       {$$ = mk_expr_idx($1, $3);                        }
  | expr '.' uqctor '[' exprs ']'                           {$$ = mk_expr_idx_member($1, $3, $5);             }

  | '(' '?' clauses ')'                                     {$$ = mk_expr_ex_qual($3);                        }
  | '(' '?' clauses ':' exprs ')'                           {$$ = mk_expr_ex_qual($3, $5);                    }

  | '{' expr ':' clauses '}'                                {$$ = mk_expr_sc($2, $4);                         }
  | '{' expr ':' clauses ';' exprs '}'                      {$$ = mk_expr_sc($2, $4, $6);                     }

  | "for" '(' clauses ')' '{' expr '}'                      {$$ = mk_expr_sc($6, $3);                         }
  | "for" '(' clauses ')' "if" '(' exprs ')' '{' expr '}'   {$$ = mk_expr_sc($10, $3, $7);                    }

  | '(' expr "=>" expr ':' clauses ')'                      {$$ = mk_expr_mc($2, $4, $6);                    }
  | '(' expr "=>" expr ':' clauses ';' exprs ')'            {$$ = mk_expr_mc($2, $4, $6, $8);                }

  | "for" '(' clauses ')' '(' expr "=>" expr ')'            {$$ = mk_expr_mc($6, $8, $3);                    }
  | "for" '(' clauses ')' "if" '(' exprs ')'
    '(' expr "=>" expr ')'                                  {$$ = mk_expr_mc($10, $12, $3, $7);              }

  | '[' expr ':' vid "<-" expr ']'                          {$$ = mk_expr_lc($2, $4, $6);                     }
  | '[' expr ':' vid ',' vid "<-" expr ']'                  {$$ = mk_expr_lc($2, $4, $6, $8);                 }
  | '[' expr ':' vid "<-" expr ',' expr ']'                 {$$ = mk_expr_flc($2, $4, $6, $8);                }
  | '[' expr ':' vid ',' vid "<-" expr ',' expr ']'         {$$ = mk_expr_flc($2, $4, $6, $8, $10);           }

  | "if" if_branches "else" expr exp_close                  {$$ = mk_expr_if($2, $4);                         }
  | "match" '(' exprs ')' match_branches exp_close          {$$ = mk_expr_match($3, $5);                        }
  | "do" stmts exp_close                                    {$$ = mk_expr_do($2);                             }

  | '{' stmts '}'                                           {$$ = mk_expr_do($2);                             }

  | "replace" pattern "in" expr "with" expr exp_close       {$$ = mk_expr_repl($2, $4, $6);                   }
  | "select" type "in" expr exp_close                       {$$ = mk_expr_sel($2, $4);                        }

  | "retrieve" expr "from" pattern "in" expr exp_close      {$$ = mk_expr_retr($2, $4, $6);                   }
  | "retrieve" expr "from" pattern "in" expr
                    "if" expr exp_close                     {$$ = mk_expr_retr($2, $4, $6, $8);               }

  | expr "is" type                                          {$$ = mk_expr_is($1, $3);                         }
  //| expr "where" where_fndefs                               {$$ = mk_expr_where($1, $3);                      }
  ;

exprs:
    expr                                                    {$$ = mk_seq($1);                                 }
  | exprs ',' expr                                          {$$ = mk_seq($1, $3);                             }
  ;


subexprs:
    subexpr                                                 {$$ = mk_seq($1);                                 }
  | subexprs ',' subexpr                                    {$$ = mk_seq($1, $3);                             }
  ;

subexpr:
    expr                                                    {$$ = mk_sexpr_expr($1);                          }
  | expr "if" expr                                          {$$ = mk_sexpr_cond($1, $3);                      }
  ;

labexpr:
    lab expr                                                {$$ = mk_sexpr_lab($1, $2);                       }
  | lab expr "if" expr                                      {$$ = mk_sexpr_lab_cond($1, $2, $4);              }
  ;

labexprs:
    labexpr                                                 {$$ = mk_seq($1);                                 }
  | labexprs ',' labexpr                                    {$$ = mk_seq($1, $3);                             }
  ;

if_branches:
    expr "then" expr                                        {$$ = mk_seq(mk_if_branch($1, $3));               }
  | if_branches ',' expr "then" expr                        {$$ = mk_seq($1, mk_if_branch($3, $5));           }
  ;

match_branch:
    patterns '=' expr                                       {$$ = mk_match_branch($1, $3);                      }
  | patterns '=' expr "let" stmts                           {$$ = mk_match_branch($1, mk_expr_let($3, $5));     }

match_branches:
    match_branch                                            {$$ = mk_seq($1);                                 }
  | match_branches ',' match_branch                         {$$ = mk_seq($1, $3);                             }
  ;

//match_branches:
//    patterns '=' expr                                       {$$ = mk_seq(mk_match_branch($1, $3));              }
//  | match_branches ',' patterns '=' expr                      {$$ = mk_seq($1, mk_match_branch($3, $5));          }
//  ;

clauses:
    clause                                                  {$$ = mk_seq($1);                                 }
  | clauses ',' clause                                      {$$ = mk_seq($1, $3);                             }
  ;

clause:
    pattern "<-" expr                                       {$$ = mk_clause_in($1, $3);                       }
  | pattern "=>" pattern "<-" expr                          {$$ = mk_clause_in_map($1, $3, $5);                       }
  | pattern "</-" expr                                      {$$ = mk_clause_not_in($1, $3);                   }
  | pattern "=>" pattern "</-" expr                         {$$ = mk_clause_not_in_map($1, $3, $5);                       }
  | vid '=' expr                                            {$$ = mk_clause_eq($1, $3);                       }
  | '(' clauses ')'                                         {$$ = mk_clause_and($2);                          }
  | clause "\\/" clause                                     {$$ = mk_clause_or($1, $3);                       }
  ;

map_entry:
    expr "=>" expr                                          {$$ = mk_map_entry($1, $3);                       }
  | expr "=>" expr "if" expr                                {$$ = mk_map_entry_cond($1, $3, $5);              }
  ;

map_entries:
    map_entry                                               {$$ = mk_seq($1);                                 }
  | map_entries ',' map_entry                               {$$ = mk_seq($1, $3);                             }
  ;

let_fndef:
    fnid '=' expr                                           {$$ = mk_fndef(null(), $1, $3);                   }
  | fnid '(' let_fnargs ')' '=' expr                        {$$ = mk_fndef(null(), $1, $3, $6);               }
  ;

let_fndefs:
    let_fndef                                               {$$ = mk_seq($1);                                 }
  | let_fndefs ',' let_fndef                                {$$ = mk_seq($1, $3);                             }
  ;

let_fnargs:
    vid                                                     {$$ = mk_seq(mk_fnarg(null(), $1));         }
  | let_fnargs ',' vid                                      {$$ = mk_seq($1, mk_fnarg(null(), $3));     }
  ;

/****************************** OTHERS ********************************/

exp_close:  ';' | "end" ;

tname:      MIXED_CASE_ID;
tvar:       UPPER_CASE_ID;

snum:       num
          | '-' num                                         {$$ = mk_snum_neg_num($2);                      }
          ;

num:        NUMBER;
str:        STRING;

//  Not sure about this one...
uqctor:     LOWER_CASE_ID
          | keyword
          | ctor
          ;

ctor:       SYMBOL
          ;

builtin:    BUILTIN;

lab:        LABEL;

fnid:       rid
          | op
          ;

op:         OP_FN_NAME;

//pid:       LOWER_CASE_ID
//         | "and"
//         | "or"
//         | "in"
//         | "type"
//         ;

rid:        vid
          | "in"
          | "and"
          | "or"
          ;

vid:        LOWER_CASE_ID
          | "type"
          | "case"
          ;

pid:        LOWER_CASE_ID
          | keyword
          ;

keyword:    "and"
          | "assert"
          | "case"
          | "def"
          | "disabled"
          | "do"
          | "else"
          | "elif"
          | "end"
          | "fail"
          | "false"
          | "for"
          | "if"
          | "in"
          | "is"
          | "itself"
          | "loop"
          | "match"
          | "nil"
          | "not"
          | "or"
          | "print"
          | "replace"
          | "return"
          | "select"
          | "subtypecheck"
          | "testcases"
          | "then"
          | "true"
//          | "try"
          | "type"
          | "using"
          | "when"
          | "where"
          | "while"
          | "with"
          ;

%%  /* Epilogue */



#include <string.h>
#include <stdio.h>

#include "common.h"
#include "lexer.h"


struct StrIntPair {
  const char *str;
  int n;
};

static StrIntPair token_to_id_map[] = {
  {"and",            KW_AND           },
  {"assert",         KW_ASSERT        },
  {"break",          KW_BREAK         },
  {"case",           KW_CASE          },
  {"def",            KW_DEF           },
  {"disabled",       KW_DISABLED      },
  {"do",             KW_DO            },
  {"else",           KW_ELSE          },
  {"elif",           KW_ELIF          },
  {"end",            KW_END           },
  {"false",          KW_FALSE         },
  {"fail",           KW_FAIL          },
  {"for",            KW_FOR           },
  {"from",           KW_FROM          },
  {"if",             KW_IF            },
  {"in",             KW_IN            },
  {"is",             KW_IS            },
  {"itself",         KW_ITSELF        },
  {"let",            KW_LET           },
  {"loop",           KW_LOOP          },
  {"match",          KW_MATCH         },
  {"nil",            KW_NIL           },
  {"not",            KW_NOT           },
  {"or",             KW_OR            },
  {"print",          KW_PRINT         },
  {"replace",        KW_REPLACE       },
  {"retrieve",       KW_RETRIEVE      },
  {"return",         KW_RETURN        },
  {"select",         KW_SELECT        },
  {"subtypecheck",   KW_SUBTYPECHECK  },
  {"then",           KW_THEN          },
  {"testcases",      KW_TESTCASES     },
  {"trace",          KW_TRACE         },
  {"true",           KW_TRUE          },
  //{"try",            KW_TRY           },
  {"type",           KW_TYPE          },
  {"using",          KW_USING         },
  {"when",           KW_WHEN          },
  {"where",          KW_WHERE         },
  {"while",          KW_WHILE         },
  {"with",           KW_WITH          }
};

string clean_str(string str)
{
  int len = str.length();

  switch (token_type(str))
  {
    case plain_identifier:
      return str;

    case op_function:
      return str;

    case symbol:
      return str.substr(1);

    case label:
      return str.substr(0, len-1);

    case number:
      return str;

    case type_id:
      for (int i=0 ; i < len ; i++)
        if (isupper(str[i]))
          str[i] = tolower(str[i]);
      return str;

    case type_var:
      for (int i=0 ; i < len ; i++)
        if (isupper(str[i]))
          str[i] = tolower(str[i]);
      return str;

    case operator_symbol:
      return str;

    case builtin:
      return str.substr(1, len-2);

    case string_lit:
      return str;

    default:
      halt;
  }
}

int yylex(void)
{
  static unsigned int pos = 0;

  int line;
  int col;

  string get_token(unsigned int idx, int &line, int &col);
  string str = get_token(pos++, line, col);

  if (str == "")
    return 0;

  yylval = str_obj(clean_str(str));

  assert(yylval.get_string() == clean_str(str));

  if (str.length() == 1 && !isalnum(str[0]))
    return str[0];

  switch (token_type(str))
  {
    case plain_identifier:  break; // LOWER_CASE_ID;
    case op_function:       return OP_FN_NAME;
    case symbol:            return SYMBOL;
    case label:             return LABEL;
    case number:            return NUMBER;
    case type_id:           return MIXED_CASE_ID;
    case type_var:          return UPPER_CASE_ID;
    case operator_symbol:   break;
    case builtin:           return BUILTIN;
    case string_lit:        return STRING;
    default:                assert(false);
  }

  if (str == "->")          return RIGHT_ARROW;
  if (str == "=>")          return DOUBLE_RIGHT_ARROW;
  if (str == "<-")          return LEFT_ARROW;
  if (str == "..")          return DOUBLE_DOT;
  if (str == "...")         return ELLIPSIS;
  if (str == ":=")          return ASSIGNMENT;
  if (str == "<=")          return LE;
  if (str == ">=")          return GE;
  if (str == "==")          return EQ;
  if (str == "/=")          return NEQ;
  if (str == "::")          return IN;
  if (str == "\\/")         return OR;
  if (str == "</-")         return CROSSED_LEFT_ARROW;

  for (int i=0 ; i < lengthof(token_to_id_map) ; i++)
    if (str == token_to_id_map[i].str)
      return token_to_id_map[i].n;

  return LOWER_CASE_ID;
}

//struct StrIntPair {
//  const char *str;
//  int n;
//};

//static StrIntPair string_to_code_map[] = {
//  {"LOWER_CASE_ID",          LOWER_CASE_ID},
//  {"SYMBOL",                 SYMBOL},
//  {"OP_FN_NAME",             OP_FN_NAME},
//  {"BUILTIN",                BUILTIN},
//  {"LABEL",                  LABEL},
//  {"NUMBER",                 NUMBER},
//  {"CHAR",                   CHAR},
//  {"STRING",                 STRING},
//  {"MIXED_CASE_ID",          MIXED_CASE_ID},
//  {"UPPER_CASE_ID",          UPPER_CASE_ID},
//  {"RIGHT_ARROW",            RIGHT_ARROW},
//  {"LEFT_ARROW",             LEFT_ARROW},
//  {"DOUBLE_RIGHT_ARROW",     DOUBLE_RIGHT_ARROW},
//  {"DOUBLE_DOT",             DOUBLE_DOT},
//  {"ELLIPSIS",               ELLIPSIS},
//  {"ASSIGNMENT",             ASSIGNMENT},
//  {"LE",                     LE},
//  {"GE",                     GE},
//  {"NEQ",                    NEQ},
//  {"IN",                     IN},
//  {"OR",                     OR},
//  {"CROSSED_LEFT_ARROW",     CROSSED_LEFT_ARROW},
//  {"KW_AND",                 KW_AND},
//  {"KW_AS",                  KW_AS},
//  {"KW_ASSERT",              KW_ASSERT},
//  {"KW_BREAK",               KW_BREAK},
//  {"KW_CASE",                KW_CASE},
//  {"KW_DEF",                 KW_DEF},
//  {"KW_DISABLED",            KW_DISABLED},
//  {"KW_DO",                  KW_DO},
//  {"KW_ELSE",                KW_ELSE},
//  {"KW_ELIF",                KW_ELIF},
//  {"KW_END",                 KW_END},
//  {"KW_FOR",                 KW_FOR},
//  {"KW_FROM",                KW_FROM},
//  {"KW_IF",                  KW_IF},
//  {"KW_IN",                  KW_IN},
//  {"KW_IS",                  KW_IS},
//  {"KW_ITSELF",              KW_ITSELF},
//  {"KW_LET",                 KW_LET},
//  {"KW_LOOP",                KW_LOOP},
//  {"KW_MATCH",               KW_MATCH},
//  {"KW_NOT",                 KW_NOT},
//  {"KW_OR",                  KW_OR},
//  {"KW_PRINT",               KW_PRINT},
//  {"KW_REPLACE",             KW_REPLACE},
//  {"KW_RETRIEVE",            KW_RETRIEVE},
//  {"KW_RETURN",              KW_RETURN},
//  {"KW_SELECT",              KW_SELECT},
//  {"KW_THEN",                KW_THEN},
//  {"KW_TESTCASES",           KW_TESTCASES},
//  {"KW_TRACE",               KW_TRACE},
//  //{"KW_TRY",                 KW_TRY},
//  {"KW_TYPE",                KW_TYPE},
//  {"KW_USING",               KW_USING},
//  {"KW_WHEN",                KW_WHEN},
//  {"KW_WHERE",               KW_WHERE},
//  {"KW_WHILE",               KW_WHILE},
//  {"KW_WITH",                KW_WITH}
//};
//
//int yylex(void)
//{
//  int res = _yylex();
//  string res_str;
//  for (int i=0 ; i < lengthof(string_to_code_map) ; i++)
//    if (string_to_code_map[i].n == res)
//    {
//      res_str = string_to_code_map[i].str;
//      break;
//    }
//  cout << res_str << " - " << yylval.get_string() << endl;
//  return res;
//}
