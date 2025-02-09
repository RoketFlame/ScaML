(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Pp

let parse_ty = Ty.parse_ty

let%expect_test "parse_var" =
  pp Ast.pp_ty parse_ty "'some_type_var" ;
  [%expect {| (Ty_var (Ident "some_type_var")) |}]

let%expect_test "parse_arrow" =
  pp Ast.pp_ty parse_ty "'a -> 'b -> 'c" ;
  [%expect
    {|
       (Ty_arr ((Ty_var (Ident "a")),
          (Ty_arr ((Ty_var (Ident "b")), (Ty_var (Ident "c")))))) |}]

let%expect_test "parse_tuple" =
  pp Ast.pp_ty parse_ty "'a * 'b * 'c" ;
  [%expect
    {|
       (Ty_tuple
          ((Ty_var (Ident "a")), (Ty_var (Ident "b")), [(Ty_var (Ident "c"))])) |}]

let%expect_test "parse_constr1" =
  pp Ast.pp_ty parse_ty "int" ;
  [%expect {| (Ty_con ((Ident "int"), [])) |}]

let%expect_test "parse_constr2" =
  pp Ast.pp_ty parse_ty "int list" ;
  [%expect {| (Ty_con ((Ident "list"), [(Ty_con ((Ident "int"), []))])) |}]

let%expect_test "parse_constr3" =
  pp Ast.pp_ty parse_ty "(int, string) map" ;
  [%expect
    {|
       (Ty_con ((Ident "map"),
          [(Ty_con ((Ident "int"), [])); (Ty_con ((Ident "string"), []))])) |}]

let%expect_test "parse_constr4" =
  pp Ast.pp_ty parse_ty "('a -> int * (string, unit, 'b -> 'c) foo bar) -> e" ;
  [%expect
    {|
       (Ty_arr (
          (Ty_arr ((Ty_var (Ident "a")),
             (Ty_tuple
                ((Ty_con ((Ident "int"), [])),
                 (Ty_con ((Ident "bar"),
                    [(Ty_con ((Ident "foo"),
                        [(Ty_con ((Ident "string"), []));
                          (Ty_con ((Ident "unit"), []));
                          (Ty_arr ((Ty_var (Ident "b")), (Ty_var (Ident "c"))))]
                        ))
                      ]
                    )),
                 []))
             )),
          (Ty_con ((Ident "e"), [])))) |}]
