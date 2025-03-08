(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

open Base

module Se_monad (StateTy : T) (ErrorTy : T) = struct
  type 'a t = StateTy.t -> StateTy.t * ('a, ErrorTy.t) Result.t

  let return x st = (st, Result.Ok x)
  let fail er st = (st, Result.Error er)

  let ( >>= ) x f st =
    let st, x = x st in
    match x with Result.Ok x -> f x st | Result.Error er -> fail er st

  let ( >>| ) x f st =
    let st, x = x st in
    match x with
    | Result.Ok x ->
        return (f x) st
    | Result.Error er ->
        fail er st

  let ( let* ) = ( >>= )
  let ( let+ ) = ( >>| )

  let read st = return st st
  let write new_st _ = (new_st, Result.Ok ())
  let run f st = f st

  let map f list =
    let open Base in
    let f acc el = acc >>= fun acc -> f el >>= fun el -> return (el :: acc) in
    List.fold_left ~f ~init:(return []) list >>| List.rev

  let fold_left f acc l =
    let f' acc a = acc >>= fun acc -> f acc a in
    List.fold_left ~f:f' ~init:acc l

  let opt_with f = function
    | Some e ->
        let+ e' = f e in
        Some e'
    | None ->
        return None
end

module Se_counter_monad (StateTy : T) (ErrorTy : T) = struct
  include
    Se_monad
      (struct
        type t = int * StateTy.t
      end)
      (ErrorTy)

  let next_id : int t =
    let* id, st = read in
    let+ () = write (id + 1, st) in
    id

  let new_name =
    let+ id = next_id in
    Int.to_string id

  let write st =
    let* id, _ = read in
    write (id, st)

  let read : StateTy.t t =
    let+ _, st = read in
    st

  let run f id st = run f (id, st)
end
