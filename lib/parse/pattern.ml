(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom
open Ast
open Common
open Smisc
open Stdlib

(* ======= Single patterns parsing ======= *)

let parse_pat_any = char '_' *> return Pat_any

let parse_pat_var = parse_value_name >>| fun name -> Pat_var (Ident name)

let parse_pat_const = parse_const >>| fun const -> Pat_constant const

(** [a; b; c] *)
let parse_pat_list ppat =
  let parse_list =
    sep_by (ws *> char ';') ppat
    >>| fun list ->
    let rec helper = function
      | h :: tl ->
          Pat_construct
            (Ident "::", Some (Pat_tuple (to_list2_exn [h; helper tl])))
      | [] ->
          Pat_construct (Ident "[]", None)
    in
    helper list
  in
  char '[' *> parse_list <* ws <* char ']'

(* [Cons (hd, tl)] *)
let parse_pat_constr ppat_single =
  let* name = parse_constr_name in
  let* arg = option None (ppat_single <* ws >>| Option.some) in
  return (Pat_construct (Ident name, arg))

let parse_pat_constraint ppat =
  lift2
    (fun pat ty -> Pat_constraint (pat, ty))
    (char '(' *> ppat)
    (ws *> char ':' *> ws *> Ty.parse_ty <* ws <* char ')')

let parse_single_pat ppat =
  fix (fun ppat_single ->
      ws
      *> choice
           [ parse_pat_any
           ; parse_pat_var
           ; parse_pat_const
           ; parse_pat_list ppat
           ; parse_pat_constr ppat_single
           ; char '(' *> ppat <* ws <* char ')'
           ; parse_pat_constraint ppat ] )

(* ======= Operators parsing ======= *)

type pat_infix_op =  OpTuple | OpList

let peek_infix_op =
  let peek_1char_op =
    peek_char_fail
    >>= function
    | ',' ->
        return {op= OpTuple; op_length= 1}
    | _ ->
        fail "not a pattern infix operator"
  in
  let peek_2chars_op =
    peek_string 2
    >>= function
    | "::" ->
        return {op= OpList; op_length= 2}
    | _ ->
        fail "not a pattern infix operator"
  in
  peek_1char_op <|> peek_2chars_op

(** Set precedence and associativity for infix operators *)
let get_infix_binding_power = function
  | OpTuple ->
      (5, 4)
  | OpList ->
      (11, 10)

let parse_pattern =
  let fold_infix acc (op, rhs) =
    match op with
    | OpTuple -> (
      match rhs with
      | Pat_tuple tl ->
          Pat_tuple (to_list2_exn (acc :: from_list2 tl))
      | _ ->
          Pat_tuple (to_list2_exn [acc; rhs]) )
    | OpList ->
        Pat_construct (Ident "::", Some (Pat_tuple (to_list2_exn [acc; rhs])))
  in
  fix (fun ppat ->
      parse_operators
        ~infix:
          { peek= peek_infix_op
          ; get_binding_power= get_infix_binding_power
          ; fold= fold_infix }
        ~parse_oprnd:(parse_single_pat ppat) () )
