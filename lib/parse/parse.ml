(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

open! Base

let parse str =
  Angstrom.parse_string ~consume:Angstrom.Consume.All Structure.parse_structure
    str
  |> Result.ok

let parse_ty_exn str =
  Angstrom.parse_string ~consume:Angstrom.Consume.All Ty.parse_ty str
  |> Result.ok_or_failwith
