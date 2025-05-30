(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Smisc
open Tys
open Vars

let id name = Ident.Ident name

let var x = Ty.Ty_var (Var x)

let ( @> ) ty_arg ty_res = Ty.Ty_arr (ty_arg, ty_res)

let tuple tys = Ty.Ty_tuple (to_list2_exn tys)

let no_vars ty = Scheme.Forall (VarSet.empty, ty)

let single_var ty = Scheme.Forall (VarSet.singleton @@ Var "a", ty)
let alpha = var "a"

let arith = Ty.int @> Ty.int @> Ty.int

let alpha_option = Ty.Ty_con (id "option", [alpha])

let alpha_list = Ty.Ty_con (id "list", [alpha])

let env =
  [ (id "id", single_var (alpha @> alpha))
  ; (id "+", no_vars arith)
  ; (id "-", no_vars arith)
  ; (id "*", no_vars arith)
  ; (id "=", no_vars (Ty.int @> Ty.int @> Ty.bool))
  ; (id "<", no_vars (Ty.int @> Ty.int @> Ty.bool))
  ; (id ">", no_vars (Ty.int @> Ty.int @> Ty.bool))
  ; (id ">=", no_vars (Ty.int @> Ty.int @> Ty.bool))
  ; (id "<=", no_vars (Ty.int @> Ty.int @> Ty.bool))
  ; (id "true", no_vars Ty.bool)
  ; (id "false", no_vars Ty.bool)
  ; (id "()", no_vars Ty.unit)
  ; (id "Some", single_var (alpha @> alpha_option))
  ; (id "None", single_var alpha_option)
  ; (id "[]", single_var alpha_list)
  ; (id "::", single_var (tuple [alpha; alpha_list] @> alpha_list)) ]
  |> Env.of_alist_exn
