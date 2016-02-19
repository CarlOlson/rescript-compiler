(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



(** Analyzing utilities for [J] module *) 

(** for example, whether it has side effect or not.
 *)

val free_variables_of_statement : 
    Ident_set.t -> Ident_set.t -> J.statement -> Ident_set.t

val free_variables_of_expression : 
    Ident_set.t -> Ident_set.t -> J.finish_ident_expression -> Ident_set.t

val no_side_effect_expression : 
    J.expression -> bool
(** [no_side_effect] means this expression has no side effect, 
    but it might *depend on value store*, so you can not just move it around,

    for example,
    when you want to do a deep copy, the expression passed to you is pure
    but you still have to call the function to make a copy, 
    since it maybe changed later
 *)

val no_side_effect_statement : 
    J.statement -> bool
(** 
    here we say 
    {[ var x = no_side_effect_expression ]}
    is [no side effect], but it is actually side effect, 
    since  we are defining a variable, however, if it is not exported or used, 
    then it's fine, so we delay this check later
 *)

val eq_expression : J.expression -> J.expression -> bool

val eq_statement : J.statement -> J.statement -> bool

val rev_flatten_seq : J.expression -> J.block 

val rev_toplevel_flatten : J.block -> J.block
(** return the block in reverse order *)

val is_constant : J.expression -> bool
