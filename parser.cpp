#include "common.h"
#include "term.h"
#include "parser.h"



Term null()
{
  return Term();
}

Term mk_seq_empty()
{
  return empty_seq_obj();
}

Term mk_seq(Term first_element)
{
  if (!first_element.is_null())
  {
    term_v ts = mk_v(first_element);
    return seq_obj(ts);
  }
  else
    return null();
}

Term mk_seq(Term seq, Term tail_element)
{
  if (!seq.is_null() && !tail_element.is_null())
  {
    term_v ts = mk_v(seq.items(), tail_element);
    return seq_obj(ts);
  }
  else
    return null();
}

Term append_es(Term element, Term sequence)
{
  term_v ts = mk_v(element, sequence.items());
  return seq_obj(ts);
}


string translate_op(string op)
{
  static string op_map[][2] = {
    {"+",   "plus"},
    {"-",   "minus"},
    {"*",   "star"},
    {"/",   "slash"},
    {"^",   "exp"},
    {"&",   "amp"},
    {"<",   "lower"},
    {">",   "greater"},
    {"<=",  "lower_eq"},
    {">=",  "greater_eq"},
    {"[]",  "brackets"}
  };

  for (int i=0 ; i < lengthof(op_map) ; i++)
  {
    string s = op_map[i][0];

    if (op == s)//op_map[i][0])
      return op_map[i][1];
  }

  halt;
}

Term mk_fn_name(Term name)
{
  bool is_valid_identifier(const string &s);
  bool is_valid_operator_as_function(const string &str);

  string s = name.get_string();

  if (is_valid_identifier(s))
  {
    Term t = symbol_obj(s);
    return tagged_obj("fn_symbol", t);
  }

  assert(is_valid_operator_as_function(s));

  Term t = symbol_obj(translate_op(s.substr(3)));
  return tagged_obj("op_symbol", t);
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Term all_decls;

void consume_decl(Term declaration)
{
  if (declaration.is_null())
    return;

  if (all_decls.is_null())
    all_decls = mk_seq_empty();
  all_decls = mk_seq(all_decls, declaration);

  static int counter = -1;
  counter++;

  char filename[1024];
  sprintf(filename, "decl_%02d.txt", counter);

  ofstream ofs(filename);
  declaration.print_indented(ofs);

  ofstream cofs("decl_num.txt");
  cofs << counter + 1;
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Term make_meta_obj(Term obj)    // Any -> Obj
{
  return tagged_obj("object", obj);
}

Term make_atom(Term token)      // TOKEN -> Atom
{
  return symbol_obj(token.get_string().c_str());
}

Term make_int(Term token)       // TOKEN -> Int
{
  string str = token.get_string();
  return int_obj(atoi(str.c_str()));
}

Term make_var(Term token)       // TOKEN -> Var
{
  Term t = make_atom(token);
  return tagged_obj("var", t);
}

Term make_symb_obj(Term token)  // TOKEN -> SymbObj
{
  return make_meta_obj(make_atom(token));
}

Term make_int_obj(Term token)   // TOKEN -> IntObj = <object(Int)>
{
  return make_meta_obj(make_int(token));
}

Term make_basic_type_symbol(Term token)
{
  Term t = make_atom(token);
  return tagged_obj("type_symbol", t);
}

Term make_par_type_symbol(Term token, Term types)
{
  return tagged_map("par_type_symbol",
           "symbol", make_basic_type_symbol(token),
           "params", types
         );
}

Term seq_to_set(Term seq)
{
  return set_obj(seq.items());
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Term mk_subtype_decl(Term subtype, Term supertype)
{
  return tagged_map("subtype_decl",
           "subtype",   subtype,
           "supertype", supertype
         );
}

Term mk_test_block(bool enabled, Term instructions)
{
  return null();
}


Term mk_test_instr_bool(Term expr)
{
  return null();
}

Term mk_test_instr_print(Term expr)
{
  return null();
}

Term mk_test_instr_loop(Term iterators, Term instructions)
{
  return null();
}

Term mk_test_instr_assignment(Term var, Term expr)
{
  return null();
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Term mk_using_block(Term signatures, Term fndefs)
{
  return tagged_map("using_block",
           //"type_params", mk_seq_empty(),
           "signatures",  signatures,
           "fn_defs",     fndefs
         );
}

//Term mk_using_block(Term type_params, Term signatures, Term fndefs)
//{
//  term_v tps = type_params.items();
//  for (unsigned int i=0 ; i < tps.size() ; i++)
//    tps[i] = mk_type_var(tps[i]);
//  type_params = seq_obj(tps);
//
//  return tagged_map("using_block",
//           "type_params", type_params,
//           "signatures",  signatures,
//           "fn_defs",     fndefs
//         );
//}


Term mk_signature(Term const_name, Term ret_type)
{
  return tagged_map("syn_sgn",
           "name",     mk_fn_name(const_name),
           "params",   mk_seq_empty(),
           "res_type", ret_type
         );
}

Term mk_signature(Term fn_name, Term par_types, Term ret_type)
{
  return tagged_map("syn_sgn",
           "name",     mk_fn_name(fn_name),
           "params",   par_types,
           "res_type", ret_type
         );
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Term mk_typedef(Term name, Term pretypes)
{
  return tagged_map("typedef",
           "name", make_basic_type_symbol(name),
           "type", mk_type_inline(pretypes)
         );
}

Term mk_typedef_par(Term name, Term vars, Term pretypes)
{
  term_v vs = vars.items();
  for (unsigned int i=0 ; i < vs.size() ; i++)
    vs[i] = mk_type_var(vs[i]);
  vars = seq_obj(vs);

  return tagged_map("par_typedef",
           "name",   make_basic_type_symbol(name),
           "params", vars,
           "type",   mk_type_inline(pretypes)
         );
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Term mk_gen_fndef(Term ret_type, Term fn_name, Term fn_args, Term expr, Term local_fns)
{
  if (ret_type.is_null())
    return tagged_map("syn_fn_def",
             "name",       mk_fn_name(fn_name),
             "params",     fn_args,
             "expr",       expr,
             "local_fns",  local_fns
           );
  else
    return tagged_map("syn_fn_def",
             "name",       mk_fn_name(fn_name),
             "params",     fn_args,
             "res_type",   ret_type,
             "expr",       expr,
             "local_fns",  local_fns
           );
}


Term mk_fndef(Term ret_type, Term fn_name, Term expr)
{
  return mk_gen_fndef(ret_type, fn_name, mk_seq_empty(), expr, mk_seq_empty());
}

Term mk_fndef(Term ret_type, Term fn_name, Term fn_args, Term expr)
{
  return mk_gen_fndef(ret_type, fn_name, fn_args, expr, mk_seq_empty());
}

Term mk_fndef_switch(Term ret_type, Term fn_name, Term fn_args, Term branches)
{
  return mk_fndef_switch(ret_type, fn_name, fn_args, branches, mk_seq_empty());
}

Term mk_fndef_switch(Term ret_type, Term fn_name, Term fn_args, Term branches, Term local_fns)
{
  int par_count = fn_args.size();

  term_v mes(par_count);
  for (int i=0 ; i < par_count ; i++)
  {
    Term t = int_obj(i);
    mes[i] = tagged_obj("fn_par", t);
  }

  return mk_gen_fndef(ret_type, fn_name, fn_args, mk_expr_match(seq_obj(mes), branches), local_fns);
}

Term mk_fndef_proc(Term ret_type, Term fn_name, Term fn_args, Term statements)
{
  return mk_gen_fndef(ret_type, fn_name, fn_args, mk_expr_do(statements), mk_seq_empty());
}

Term mk_fndef_proc(Term ret_type, Term fn_name, Term fn_args, Term statements, Term local_fns)
{
  return mk_gen_fndef(ret_type, fn_name, fn_args, mk_expr_do(statements), local_fns);
}

Term mk_fnarg(Term type, Term var)
{
  //[(type: SynType?, var: var(Atom)?)*]

  if (type.is_null() && var.is_null())
    return empty_map_obj();

  if (type.is_null())
    return map_obj("var", make_var(var));

  if (var.is_null())
    return map_obj("type", type);

  return map_obj("type", type, "var", make_var(var));
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Term mk_stmt_assignment(Term var, Term expr)
{
  return tagged_map("assignment_stmt",
           "var", make_var(var),
           "value", expr
         );
}

Term mk_stmt_assignment_if(Term var, Term expr, Term cond)
{
  return mk_stmt_if(
           cond,
           mk_seq(mk_stmt_assignment(var, expr)),
           mk_seq_empty()
         );
}

Term mk_stmt_return(Term expr)
{
  return tagged_obj("return_stmt", expr);
}

Term mk_stmt_return_if(Term expr, Term cond)
{
  return mk_stmt_if(
           cond,
           mk_seq(mk_stmt_return(expr)),
           mk_seq_empty()
         );
}

Term mk_stmt_if(Term cond, Term statements, Term elifs)
{
  return mk_stmt_if(cond, statements, elifs, empty_seq_obj());
}

Term mk_stmt_if(Term cond, Term statements, Term elifs, Term else_branch)
{
  return tagged_map("if_stmt",
           "branches", append_es(mk_elif(cond, statements), elifs),
           "else", else_branch
         );
}


Term mk_stmt_loop(Term statements)
{
  return tagged_obj("inf_loop_stmt", statements);
}

Term mk_stmt_loop(Term statements, Term condition)
{
  return tagged_map("loop_stmt",
           "cond", condition,
           "skip_first", symbol_obj("true"),
           "body", statements
         );
}

Term mk_stmt_while(Term cond, Term statements)
{
  return tagged_map("loop_stmt",
           "cond", cond,
           "skip_first", symbol_obj("false"),
           "body", statements
         );
}

Term mk_stmt_let(Term fndefs, Term statements)
{
  return tagged_map("let_stmt",
           "asgnms", fndefs,
           "body",   statements
         );
}

Term mk_stmt_break()
{
  return symbol_obj("break_stmt");
}

Term mk_stmt_break_if(Term cond)
{
  return mk_stmt_if(
           cond,
           mk_seq(mk_stmt_break()),
           mk_seq_empty()
         );
}

Term mk_stmt_fail()
{
  return symbol_obj("fail_stmt");
}

Term mk_stmt_fail_if(Term cond)
{
  return mk_stmt_if(
           cond,
           mk_seq(mk_stmt_fail()),
           mk_seq_empty()
         );
}

Term mk_stmt_for(Term iters, Term statements)
{
  return tagged_map("for_stmt",
           "loops", iters,
           "body", statements
         );
}


Term mk_stmt_assert(Term expr)
{
  return tagged_obj("assert_stmt", expr);
}

Term mk_stmt_print(Term expr)
{
  return tagged_obj("print_stmt", expr);
}

Term mk_stmt_print_if(Term expr, Term cond)
{
  return mk_stmt_if(
           cond,
           mk_seq(mk_stmt_print(expr)),
           mk_seq_empty()
         );
}


Term mk_elif(Term cond, Term statements)
{
  return map_obj(
           "cond", cond,
           "body", statements
         );
}


Term mk_for_iter(Term var, Term seq)
{
  return tagged_map("seq_iter",
           "var",    make_var(var),
           "values", seq
         );
}

Term mk_for_iter(Term var, Term idx_var, Term seq)
{
  return tagged_map("seq_iter",
           "var",     make_var(var),
           "idx_var", make_var(idx_var),
           "values",  seq
         );
}

Term mk_for_iter_range(Term var, Term start_val, Term end_val)
{
  return tagged_map("range_iter",
           "var",       make_var(var),
           "start_val", start_val,
           "end_val",   end_val
         );
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Term mk_ptrn_type(Term type)
{
  return tagged_obj("type_ptrn", type);
}

Term mk_ptrn_var(Term var)
{
  return tagged_map("var_ptrn", "name", make_var(var));
}

Term mk_ptrn_ptrn_var(Term pattern, Term var)
{
  return tagged_map("var_ptrn", "name", make_var(var), "ptrn", pattern);
}

Term mk_ptrn_ctor(Term symb)
{
  Term t = make_symb_obj(symb);
  return tagged_obj("obj_ptrn", t);
}

Term mk_ptrn_num(Term num)
{
  Term t = make_int_obj(num);
  return tagged_obj("obj_ptrn", t);
}

Term mk_ptrn_jolly()
{
  return symbol_obj("ptrn_any");
}

Term mk_ptrn_expr(Term var)
{
  Term t = make_var(var);
  return tagged_obj("ext_var_ptrn", t);
}

Term mk_ptrn_tuple(Term labptrns, bool open)
{
  return tagged_map("tuple_ptrn",
           "fields", seq_to_set(labptrns),
           "is_open", bool_obj(open)
         );
}

Term mk_ptrn_tag_ptrn(Term tag, Term ptrn)
{
  return tagged_map("tag_ptrn",
           "tag", mk_ptrn_ctor(tag),
           "obj", ptrn
         );
}

Term mk_ptrn_tag_obj(Term tag_var, Term obj_var)
{
  return tagged_map("tag_ptrn",
           "tag", mk_ptrn_var(tag_var),
           "obj", mk_ptrn_var(obj_var)
         );
}

Term mk_lab_ptrn(Term lab, Term ptrn)
{
  return map_obj("label", make_symb_obj(lab), "ptrn", ptrn);
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Term mk_type_ref(Term name)
{
  Term t = make_basic_type_symbol(name);
  return tagged_obj("type_ref", t);
}

Term mk_type_var(Term var_name)
{
  Term t = make_atom(var_name);
  return tagged_obj("type_var", t);
}

Term mk_type_ref(Term name, Term type_pars)
{
  Term t = make_par_type_symbol(name, type_pars);
  return tagged_obj("type_ref", t);
}

Term mk_type_inline(Term pretypes)
{
  if (pretypes.size() > 1)
  {
    Term t = seq_to_set(pretypes);
    return tagged_obj("union_type", t);
  }
  else
    return pretypes.item(0);
}

Term mk_type_symb()
{
  return symbol_obj("atom_type");
}

Term mk_type_int()
{
  return symbol_obj("integer");
}

Term mk_type_low_bounded_int(Term min)
{
  return tagged_map("high_ints", "min", make_int(min));
}

Term mk_type_up_bounded_int(Term max)
{
  return tagged_map("low_ints", "max", make_int(max));
}

Term mk_type_bounded_int(Term min_token, Term max_token)
{
  int min = make_int(min_token).get_int();
  int max = make_int(max_token).get_int();

  int size = max - min + 1;

  if (size <= 0)
  {
    void yyerror(char const *);
    yyerror("Invalid range type");
    halt;
  }

  return tagged_map("int_range", "min", int_obj(min), "size", int_obj(size));
}

Term mk_type_seq(Term type, bool nonempty)
{
  Term ne_type = tagged_map("seq_type", "elem_type", type);
  if (nonempty)
    return ne_type;

  Term e_type = mk_pretype_empty_seq();
  Term ts = mk_seq(e_type);
  ts = mk_seq(ts, ne_type);
  ts = seq_to_set(ts);
  return tagged_obj("union_type", ts);
  //   Term mk_type_inline(Term pretypes)
  // {
  //   if (pretypes.size() > 1)
  //   {
  //     Term t = seq_to_set(pretypes);
  //     return tagged_obj("union_type", t);
  //   }
  //   else
  //     return pretypes.item(0);
  // }
}

Term mk_type_fixed_seq(Term pretypes)
{
  return tagged_obj("fixed_seq_type", pretypes);
}

Term mk_type_set(Term elem_type, bool nonempty)
{
  Term ne_type = tagged_map("set_type", "elem_type", elem_type);
  if (nonempty)
    return ne_type;

  Term e_type = mk_pretype_empty_set();
  Term ts = seq_to_set(mk_seq(mk_seq(e_type), ne_type));
  return tagged_obj("union_type", ts);
}

Term mk_type_map(Term key_type, Term value_type)
{
  return tagged_map("map_type", "key_type", key_type, "value_type", value_type);
}

Term mk_type_tuple(Term lab_types)
{
  Term t = seq_to_set(lab_types);
  return tagged_obj("tuple_type", t);
}

Term mk_type_tagged_obj(Term tag_type, Term obj_type)
{
  return tagged_map("tag_type", "tag_type", tag_type, "obj_type", obj_type);
}

Term mk_pretype_type(Term type)
{
  return type;
}

Term mk_pretype_empty_set()
{
  return symbol_obj("empty_set_type");
}

Term mk_pretype_empty_seq()
{
  return symbol_obj("empty_seq_type");
}

Term mk_pretype_empty_map()
{
  return symbol_obj("empty_map_type");
}

Term mk_pretype_sing(Term symb)
{
  Term t = make_symb_obj(symb);
  return tagged_obj("symb_type", t);
}

Term mk_pretype_tagged_obj(Term tag, Term obj_type)
{
  return tagged_map("tag_type", "tag_type", mk_pretype_sing(tag), "obj_type", obj_type);
}

Term mk_pretype_tagged_tuple(Term tag, Term lab_types)
{
  return mk_pretype_tagged_obj(tag, mk_type_tuple(lab_types));
}

Term mk_labtype(Term lab, Term type, bool optional)
{
  return map_obj(
           "label",    make_symb_obj(lab),
           "type",     type,
           "optional", bool_obj(optional)
         );
}

////////////////////////////////////////////////////////////////////////////////

Term mk_expr_num(Term num)
{
  return make_int_obj(num);
}

Term mk_expr_ctor(Term symb)
{
  return make_symb_obj(symb);
}

Term mk_expr_str(Term str)
{
  string s = str.get_string();
  int len = s.length();
  assert(len >= 2 && s[0] == '"' && s[len-1] == '"');

  int_v codes;
  for (int i=0 ; i < len-2 ; i++)
  {
    char ch = s[i+1];

    if (ch != '\\')
    {
      codes.push_back(ch);
      continue;
    }

    i++;

    assert(i < len-2);

    ch = s[i+1];

    assert(ch == 'n' || ch == '"' || ch == '\\');

    switch (ch)
    {
      case 'n':
        codes.push_back('\n');
        break;

      case '"':
        codes.push_back('"');
        break;

      case '\\':
        codes.push_back('\\');
        break;

      default:
        halt;
    }
  }

  term_v chs(codes.size());
  for (unsigned int i=0 ; i < codes.size() ; i++)
    chs[i] = make_meta_obj(int_obj(codes[i]));

  // tag_obj_expr(tag: SynExpr, obj: SynExpr)

  // tag_obj_expr(
  //   tag: object(string),
  //   obj: seq_expr(head: [object(97), object(98)])
  // )

  return tagged_map("tag_obj_expr",
           "tag", make_meta_obj(symbol_obj("string")),
           "obj", tagged_map("seq_expr", "head", seq_obj(chs))
         );
}

Term mk_expr_set(Term subexprs)
{
  Term t = seq_to_set(subexprs);
  return tagged_obj("set_expr", t);
}

Term mk_expr_seq(Term subexprs)
{
  return tagged_map("seq_expr", "head", subexprs);
}

Term mk_expr_seq(Term subexprs, Term tail_expr)
{
  return tagged_map("seq_expr", "head", subexprs, "tail", tail_expr);
}

Term mk_expr_map(Term entries)
{
  Term t = seq_to_set(entries);
  return tagged_obj("map_expr", t);
}

Term mk_expr_tuple(Term labexprs)
{
  Term t = seq_to_set(labexprs);
  return tagged_obj("map_expr", t);
}

Term mk_expr_tag_obj(Term tag_expr, Term obj_expr)
{
  return tagged_map("tag_obj_expr", "tag", tag_expr, "obj", obj_expr);
}

Term mk_expr_tag_map(Term tag, Term labexprs)
{
  return mk_expr_tag_obj(make_symb_obj(tag), mk_expr_tuple(labexprs));
}

Term mk_expr_const_or_var(Term const_or_var_name)
{
  Term t = make_atom(const_or_var_name);
  return tagged_obj("const_or_var", t);
}

Term mk_expr_fn_call(Term fn_name, Term params)
{
  return tagged_map("fn_call",
           "name",          mk_fn_name(fn_name),
           "params",        params,
           "named_params",  mk_seq_empty()
         );
}

Term mk_expr_fn_call_impl(Term fn_name, Term params, Term fndefs)
{
  return tagged_map("fn_call",
           "name",          mk_fn_name(fn_name),
           "params",        params,
           "named_params",  fndefs
         );
}

Term mk_expr_builtin_call(Term builtin_name, Term params)
{
  return tagged_map("builtin_call",
           "name",   make_atom(builtin_name),
           "params", params
         );
}

Term mk_expr_op(Term op, Term val)
{
  Term t = symbol_obj(translate_op(op.get_string()));
  return tagged_map("fn_call",
           "name",         tagged_obj("op_symbol", t),
           "params",       mk_seq(val),
           "named_params", mk_seq_empty()
         );
}

Term mk_expr_op(Term op, Term val1, Term val2)
{
  Term t = symbol_obj(translate_op(op.get_string()));
  return tagged_map("fn_call",
           "name",         tagged_obj("op_symbol", t),
           "params",       mk_seq(mk_seq(val1), val2),
           "named_params", mk_seq_empty()
         );
}

Term mk_expr_and(Term expr1, Term expr2)
{
  return tagged_map("and",
           "left",  expr1,
           "right", expr2
         );
}

Term mk_expr_or(Term expr1, Term expr2)
{
  return tagged_map("or",
           "left",  expr1,
           "right", expr2
         );
}

Term mk_expr_not(Term expr)
{
  return tagged_obj("not", expr);
}


Term mk_expr_eq(Term expr1, Term expr2)
{
  return tagged_map("eq",
           "left", expr1,
           "right", expr2
         );
}

Term mk_expr_neq(Term expr1, Term expr2)
{
  return tagged_map("neq",
           "left", expr1,
           "right", expr2
         );
}

Term mk_expr_type_test(Term expr, Term type)
{
  return tagged_map("membership",
           "obj", expr,
           "type", type
         );
}

Term mk_expr_dot_acc(Term expr, Term name)
{
  return tagged_map("accessor",
           "expr", expr,
           "field", make_symb_obj(name)
         );
}

Term mk_expr_dot_acc_test(Term expr, Term name)
{
  return tagged_map("accessor_test",
           "expr", expr,
           "field", make_symb_obj(name)
         );
}

Term make_expr_idx(Term obj, Term params)
{
  int len = params.size();

  term_v ps(len+1);
  ps[0] = obj;
  for (int i=0 ; i < len ; i++)
    ps[i+1] = params.item(i);

  Term t = symbol_obj("brackets");
  return tagged_map("fn_call",
           "name",         tagged_obj("op_symbol", t),
           "params",       seq_obj(ps),
           "named_params", mk_seq_empty()
         );
}

Term mk_expr_idx(Term const_or_var, Term exprs)
{
  return make_expr_idx(mk_expr_const_or_var(const_or_var), exprs);
}

Term mk_expr_idx_member(Term expr, Term name, Term exprs)
{
  return make_expr_idx(mk_expr_dot_acc(expr, name), exprs);
}

Term mk_expr_ex_qual(Term clauses)
{
  return tagged_map("ex_qual",
           "source", clauses,
           "sel_exprs", mk_seq_empty()
         );
}

Term mk_expr_ex_qual(Term clauses, Term sel_exprs)
{
  return tagged_map("ex_qual",
           "source", clauses,
           "sel_exprs", sel_exprs
         );
}

Term mk_expr_sc(Term expr, Term clauses)
{
  return tagged_map("set_comp",
           "expr", expr,
           "source", clauses,
           "sel_exprs", mk_seq_empty()
         );
}

Term mk_expr_sc(Term expr, Term clauses, Term sel_exprs)
{
  return tagged_map("set_comp",
           "expr", expr,
           "source", clauses,
           "sel_exprs", sel_exprs
         );
}

Term mk_expr_mc(Term key_expr, Term val_expr, Term clauses)
{
  return tagged_map("map_comp",
           "key_expr",   key_expr,
           "value_expr", val_expr,
           "source",     clauses,
           "sel_exprs",  empty_seq_obj()
         );
}

Term mk_expr_mc(Term key_expr, Term val_expr, Term clauses, Term sel_exprs)
{
  return tagged_map("map_comp",
           "key_expr", key_expr,
           "value_expr", val_expr,
           "source", clauses,
           "sel_exprs", sel_exprs
         );
}

Term mk_expr_lc(Term expr, Term var_name, Term src_expr)
{
  return tagged_map("seq_comp",
           "expr", expr,
           "var", make_var(var_name),
           "src_expr", src_expr
         );
}

Term mk_expr_lc(Term expr, Term var_name, Term idx_var_name, Term src_expr)
{
  return tagged_map("seq_comp",
           "expr", expr,
           "var", make_var(var_name),
           "idx_var", make_var(idx_var_name),
           "src_expr", src_expr
         );
}

Term mk_expr_flc(Term expr, Term var_name, Term src_expr, Term sel_expr)
{
  return tagged_map("seq_comp",
           "expr", expr,
           "var", make_var(var_name),
           "src_expr", src_expr,
           "sel_expr", sel_expr
         );
}

Term mk_expr_flc(Term expr, Term var_name, Term idx_var_name, Term src_expr, Term sel_expr)
{
  return tagged_map("seq_comp",
           "expr", expr,
           "var", make_var(var_name),
           "idx_var", make_var(idx_var_name),
           "src_expr", src_expr,
           "sel_expr", sel_expr
         );
}

Term mk_expr_if(Term branches, Term else_expr)
{
  return tagged_map("if_expr",
           "branches", branches,
           "else", else_expr
         );
}

Term mk_expr_match(Term exprs, Term branches)
{
  return tagged_map("match_expr",
           "exprs", exprs,
           "cases", branches
         );
}

Term mk_expr_do(Term statements)
{
  return tagged_obj("do_expr", statements);
}

Term mk_expr_repl(Term ptrn, Term src_expr, Term rep_expr)
{
  return tagged_map("replace_expr",
           "expr",     rep_expr,
           "src_expr", src_expr,
           "ptrn",     ptrn
         );
}

Term mk_expr_sel(Term type, Term expr)
{
  return tagged_map("select_expr",
           "type",     type,
           "src_expr", expr
         );
}

Term mk_expr_retr(Term expr, Term ptrn, Term src)
{
  return tagged_map("retrieve_expr",
           "expr", expr,
           "ptrn", ptrn,
           "src_expr", src
         );
}

Term mk_expr_retr(Term expr, Term ptrn, Term src, Term cond)
{
  return tagged_map("retrieve_expr",
           "expr", expr,
           "ptrn", ptrn,
           "src_expr", src,
           "cond", cond
         );
}


Term mk_expr_is(Term expr, Term type)
{
  return tagged_map("is_expr",
           "expr", expr,
           "type", type
         );
}


Term mk_expr_where(Term expr, Term fndefs)
{
  return tagged_map("where_expr",
           "expr", expr,
           "fndefs", fndefs
         );
}

Term mk_expr_let(Term expr, Term statements)
{
  return tagged_map("let_expr",
           "expr", expr,
           "stmts", statements
         );
}


Term mk_sexpr_expr(Term expr)
{
  return expr;
}

Term mk_sexpr_cond(Term value, Term cond)
{
  return tagged_map("cond_expr",
           "expr", value,
           "cond", cond
         );
}

Term mk_sexpr_lab(Term lab, Term expr)
{
  return map_obj("key", make_symb_obj(lab), "value", expr);
}

Term mk_sexpr_lab_cond(Term lab, Term expr, Term cond)
{
  return map_obj("key", make_symb_obj(lab), "value", expr, "cond", cond);
}

Term mk_if_branch(Term cond, Term value)
{
  return map_obj("cond", cond, "expr", value);
}


Term mk_match_branch(Term ptrns, Term expr)
{
  return tagged_map("case",
           "patterns", ptrns,
           "expr", expr
         );
}


Term mk_clause_in(Term ptrn, Term src_expr)
{
  return tagged_map("in_clause",
           "ptrn", ptrn,
           "src", src_expr
         );
}

Term mk_clause_not_in(Term ptrn, Term src_expr)
{
  return tagged_map("not_in_clause",
           "ptrn", ptrn,
           "src", src_expr
         );
}

Term mk_clause_in_map(Term key_ptrn, Term value_ptrn, Term src_expr)
{
  return tagged_map("map_in_clause",
           "key_ptrn", key_ptrn,
           "value_ptrn", value_ptrn,
           "src", src_expr
         );
}

Term mk_clause_not_in_map(Term key_ptrn, Term value_ptrn, Term src_expr)
{
  return tagged_map("map_not_in_clause",
           "key_ptrn", key_ptrn,
           "value_ptrn", value_ptrn,
           "src", src_expr
         );
}

Term mk_clause_eq(Term var, Term expr)
{
  return tagged_map("eq_clause",
           "var", make_var(var),
           "expr", expr
         );
}

Term mk_clause_and(Term clauses)
{
  return tagged_obj("and_clause", clauses);
}

Term mk_clause_or(Term clause1, Term clause2)
{
  return tagged_map("or_clause",
           "left", clause1,
           "right", clause2
         );
}

Term mk_map_entry(Term expr1, Term expr2)
{
  return map_obj(
           "key", expr1,
           "value", expr2
         );
}

Term mk_map_entry_cond(Term expr1, Term expr2, Term cond)
{
  return map_obj(
           "key", expr1,
           "value", expr2,
           "cond", cond
         );
}


Term mk_snum_neg_num(Term num)
{
  string str = num.get_string();
  int n = -atoi(str.c_str());
  char buffer[64];
  sprintf(buffer, "%d", n);
  return str_obj(buffer);
}
