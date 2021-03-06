#ifndef _PARSER_ZH_
#define _PARSER_ZH_

#include "term.h"

Term null();
Term mk_seq(Term first_element);
Term mk_seq(Term seq, Term tail_element);
Term mk_seq_empty();
void consume_decl(Term declaration);
Term mk_subtype_decl(Term subtype, Term supertype);
Term mk_test_block(bool enabled, Term instructions);
Term mk_test_instr_bool(Term expr);
Term mk_test_instr_print(Term expr);
Term mk_test_instr_loop(Term iterators, Term instructions);
Term mk_test_instr_assignment(Term var, Term expr);
Term mk_using_block(Term signatures, Term fndefs);
Term mk_signature(Term const_name, Term ret_type);
Term mk_signature(Term fn_name, Term par_types, Term ret_type);
Term mk_typedef(Term name, Term pretypes);
Term mk_typedef_par(Term name, Term vars, Term pretypes);
Term mk_fndef(Term ret_type, Term fn_name, Term expr);
Term mk_fndef(Term ret_type, Term fn_name, Term fn_args, Term expr);
Term mk_fndef_switch(Term fn_name, Term fn_args, Term ret_type, Term branches);
Term mk_fndef_switch(Term fn_name, Term fn_args, Term ret_type, Term branches, Term local_fns);
Term mk_fndef_proc(Term fn_name, Term fn_args, Term ret_type, Term statements);
Term mk_fndef_proc(Term fn_name, Term fn_args, Term ret_type, Term statements, Term local_fns);
Term mk_fnarg(Term type, Term var);
Term mk_stmt_assignment(Term var, Term expr);
Term mk_stmt_assignment_if(Term var, Term expr, Term cond);
Term mk_stmt_return(Term expr);
Term mk_stmt_return_if(Term expr, Term cond);
Term mk_stmt_if(Term cond, Term statements, Term elifs);
Term mk_stmt_if(Term cond, Term statements, Term elifs, Term else_branch);
Term mk_stmt_loop(Term statements);
Term mk_stmt_loop(Term statements, Term condition);
Term mk_stmt_while(Term cond, Term statements);
Term mk_stmt_let(Term fndefs, Term statements);
Term mk_stmt_break();
Term mk_stmt_break_if(Term cond);
Term mk_stmt_for(Term iters, Term statements);
Term mk_stmt_assert(Term expr);
Term mk_stmt_print(Term expr);
Term mk_stmt_print_if(Term expr, Term cond);
Term mk_elif(Term cond, Term statements);
Term mk_for_iter(Term var, Term seq);
Term mk_for_iter(Term var, Term idx_var, Term seq);
Term mk_for_iter_range(Term var, Term start_val, Term end_val);
Term mk_ptrn_type(Term type);
Term mk_ptrn_var_len_seq(Term pattern, bool nonempty);
Term mk_ptrn_map_type(Term key_ptrn, Term value_ptrn, bool nonempty);
Term mk_ptrn_ptrn_var(Term type, Term var);
Term mk_ptrn_var(Term var);
Term mk_ptrn_bound_var(Term var);
Term mk_ptrn_jolly();
Term mk_ptrn_var_ptrn(Term lab, Term ptrn);
Term mk_ptrn_undo_click(Term top_lab_var, Term ptrn);
Term mk_ptrn_ctor(Term symb);
Term mk_ptrn_empty_seq();
Term mk_ptrn_empty_map();
Term mk_ptrn_snum(Term num);
Term mk_ptrn_term(Term top_symb, Term sub_ptrns);
Term mk_ptrn_term_dots(Term top_symb);
Term mk_ptrn_term_dots(Term top_symb, Term sub_ptrns);
Term mk_ptrn_term_split(Term top_symb, Term var1, Term var2);
Term mk_ptrn_empty_set();
Term mk_ptrn_set(Term sub_ptrns);
Term mk_ptrn_set_dots();
Term mk_ptrn_set_dots(Term sub_ptrns);
Term mk_ptrn_set_split(Term var1, Term var2);
Term mk_ptrn_tuple(Term lab_ptrns);
Term mk_ptrn_tuple_dots(Term lab_ptrns);
Term mk_ptrn_seq(Term ptrns);
Term mk_ptrn_seq(Term ptrns, Term tail_ptrn);
Term mk_ptrn_map_dots(Term entries);
Term mk_ptrn_symb();
Term mk_ptrn_int();
Term mk_ptrn_empty_seq();
Term mk_ptrn_seq();
Term mk_ptrn_empty_set();
Term mk_ptrn_set();
Term mk_ptrn_empty_map();
Term mk_ptrn_map();
Term mk_subpattern_pattern(Term ptrn);
Term mk_subpattern_lab(Term lab, Term ptrn);
Term mk_subpattern_any(Term ptrn);
Term mk_subpattern_mult(Term ptrn);
Term mk_labpattern(Term lab, Term ptrn);
Term mk_ptrn_map_entry(Term ptrn1, Term ptrn2);
Term mk_type_name(Term name);
Term mk_type_ref(Term name);
Term mk_type_var(Term var_name);
Term mk_type_ref(Term name, Term type_pars);
Term mk_type_inline(Term pretypes);
Term mk_type_symb();
Term mk_type_int();
Term mk_type_low_bounded_int(Term min);
Term mk_type_up_bounded_int(Term max);
Term mk_type_bounded_int(Term min, Term max);
Term mk_type_seq(Term type, bool nonempty);
Term mk_type_fixed_seq(Term pretypes);
Term mk_type_empty_set();
Term mk_type_set(Term subpretypes);
Term mk_type_set_from_single_type(Term type, bool nonempty);
Term mk_type_map(Term left_type, Term right_type, bool nonempty);
Term mk_type_tagged_obj(Term tag_type, Term obj_type);
Term mk_pretype_type(Term type);
Term mk_pretype_sing(Term symb);
Term mk_pretype_term(Term top_symbol, Term subpretypes);
Term mk_pretype_set(Term subpretypes);
Term mk_pretype_tuple(Term subpretypes);
Term mk_subpretype_pretype(Term pretype);
Term mk_subpretype_opt(Term pretype);
Term mk_subpretype_any(Term pretype);
Term mk_subpretype_mult(Term pretype);
Term mk_subpretype_lab(Term lab, Term pretype);
Term mk_subpretype_lab_opt(Term lab, Term pretype);
Term mk_expr_ctor(Term symb);
Term mk_expr_empty_set();
Term mk_expr_set(Term subexprs);
Term mk_expr_term(Term top_symb, Term subexprs);
Term mk_expr_rep_term(Term top_symb, Term reps, Term subexprs);
Term mk_expr_lab_tuple(Term labexprs);
Term mk_expr_lab_set(Term lab, Term set);
Term mk_expr_const_or_var(Term const_or_var_name);
Term mk_expr_fn_call(Term fn_name, Term params);
Term mk_expr_fn_call_impl(Term fn_name, Term params, Term fndefs);
Term mk_expr_builtin_call(Term builtin_name, Term params);
Term mk_expr_empty_seq();
Term mk_expr_seq(Term exprs);
Term mk_expr_seq(Term first, Term rest);
Term mk_expr_empty_map();
Term mk_expr_map(Term entries);
Term mk_expr_num(Term num);
Term mk_expr_str(Term str);
Term mk_expr_op(Term op, Term val);
Term mk_expr_op(Term op, Term val1, Term val2);
Term mk_expr_and(Term expr1, Term expr2);
Term mk_expr_or(Term expr1, Term expr2);
Term mk_expr_not(Term expr);
Term mk_expr_eq(Term expr1, Term expr2);
Term mk_expr_neq(Term expr1, Term expr2);
Term mk_expr_type_test(Term e, Term t);
Term mk_expr_type_cast(Term t, Term e);
Term mk_expr_dot_acc(Term expr, Term name);
Term mk_expr_dot_acc_test(Term expr, Term name);
Term mk_expr_dot_acc_star(Term expr);
Term mk_expr_idx(Term var_or_const, Term exprs);
Term mk_expr_idx_member(Term expr, Term name, Term exprs);
Term mk_expr_ex_qual(Term clauses);
Term mk_expr_ex_qual(Term clauses, Term sel_exprs);
Term mk_expr_sc(Term expr, Term clauses);
Term mk_expr_sc(Term expr, Term clauses, Term sel_exprs);
Term mk_expr_tc(Term top_label, Term expr, Term clauses);
Term mk_expr_tc(Term top_label, Term expr, Term clauses, Term sel_exprs);
Term mk_expr_msc(Term key_expr, Term val_expr, Term clauses);
Term mk_expr_msc(Term key_expr, Term val_expr, Term clauses, Term sel_exprs);
Term mk_expr_lc(Term expr, Term var_name, Term src_expr);
Term mk_expr_lc(Term expr, Term var_name, Term idx_var_name, Term src_expr);
Term mk_expr_flc(Term expr, Term var_name, Term src_expr, Term sel_expr);
Term mk_expr_flc(Term expr, Term var_name, Term idx_var_name, Term src_expr, Term sel_expr);
Term mk_expr_if(Term branches, Term else_expr);
Term mk_expr_match(Term exprs, Term branches);
Term mk_expr_do(Term statements);
Term mk_expr_repl(Term type, Term var, Term src_expr, Term rep_expr);
Term mk_expr_sel(Term type, Term expr);
Term mk_expr_retr(Term expr, Term ptrn, Term src);
Term mk_expr_retr(Term expr, Term ptrn, Term src, Term cond);
Term mk_expr_is(Term expr, Term type);
Term mk_expr_where(Term expr, Term fndefs);
Term mk_expr_let(Term expr, Term statements);
Term mk_sexpr_expr(Term expr);
Term mk_sexpr_lab(Term lab, Term expr);
Term mk_sexpr_cond(Term value, Term cond);
Term mk_sexpr_lab_cond(Term lab, Term expr, Term cond);
Term mk_if_branch(Term cond, Term value);
Term mk_match_branch(Term ptrns, Term expr);
Term mk_clause_in(Term ptrn, Term src_expr);
Term mk_clause_not_in(Term ptrn, Term src_expr);
Term mk_clause_eq(Term var, Term expr);
Term mk_clause_and(Term clauses);
Term mk_clause_or(Term clause1, Term clause2);
Term mk_map_entry(Term expr1, Term expr2);
Term mk_map_entry_cond(Term expr1, Term expr2, Term cond);
Term mk_snum_neg_num(Term num);

Term mk_stmt_fail();
Term mk_stmt_fail_if(Term cond);

Term mk_ptrn_type(Term type);
Term mk_ptrn_var(Term var);
Term mk_ptrn_ptrn_var(Term pattern, Term var);
Term mk_ptrn_ctor(Term symb);
Term mk_ptrn_num(Term num);
Term mk_ptrn_jolly();
Term mk_ptrn_expr(Term var);
Term mk_ptrn_tuple(Term labptrns, bool open);
Term mk_ptrn_tag_ptrn(Term tag, Term ptrn);
Term mk_ptrn_tag_obj(Term tag_var, Term obj_var);
Term mk_lab_ptrn(Term lab, Term ptrn);

Term mk_type_ref(Term name);
Term mk_type_var(Term var_name);
Term mk_type_ref(Term name, Term type_pars);
Term mk_type_inline(Term pretypes);
Term mk_type_any();
Term mk_type_symb();
Term mk_type_int();
Term mk_type_low_bounded_int(Term min);
Term mk_type_up_bounded_int(Term max);
Term mk_type_bounded_int(Term min, Term max);
Term mk_type_seq(Term type, bool nonempty);
Term mk_type_fixed_seq(Term pretypes);
Term mk_type_set(Term elem_type, bool nonempty);
Term mk_type_map(Term key_type, Term value_type);
Term mk_type_tuple(Term lab_types);
Term mk_pretype_type(Term type);
Term mk_pretype_empty_set();
Term mk_pretype_empty_seq();
Term mk_pretype_empty_map();
Term mk_pretype_sing(Term symb);
Term mk_pretype_tagged_obj(Term tag, Term obj);
Term mk_pretype_tagged_tuple(Term tag, Term lab_types);
Term mk_labtype(Term lab, Term type, bool optional);

Term mk_expr_tuple(Term labexprs);
Term mk_expr_tag_obj(Term tag_expr, Term obj_expr);
Term mk_expr_tag_map(Term tag, Term labexprs);
Term mk_expr_mc(Term key_expr, Term val_expr, Term clauses);
Term mk_expr_mc(Term key_expr, Term val_expr, Term clauses, Term sel_exprs);

Term mk_clause_in_map(Term key_ptrn, Term value_ptrn, Term src_expr);
Term mk_clause_not_in_map(Term key_ptrn, Term value_ptrn, Term src_expr);


#endif
