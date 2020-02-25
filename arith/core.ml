open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval t  -> true
  | _ -> false

let rec eval t = match t with
    TmIf(_,TmTrue(_),t2,t3) when (isval t2) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) when (isval t3) ->
      t3
  | TmSucc(fi, t1) ->
      TmSucc(fi, eval t1)
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_, t1) -> (
      let t' = eval t1 in
      match t' with
        TmSucc(_,nv1) when (isnumericval nv1) -> nv1
      | t -> t)
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo) 
  | TmIsZero(_, t1) -> (
      let t' = eval t1 in
      match t' with
        TmSucc(_,nv1) when (isnumericval nv1) ->
          TmFalse(dummyinfo)
      | t -> t)
  | t -> 
      t

