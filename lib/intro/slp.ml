type id = string

type binop =
  | Plus
  | Minus
  | Times
  | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

module MTableSet = struct
  include Set.Make (struct
      type t = id * int

      let compare (id1, _) (id2, _) = String.compare id1 id2
    end)

  let addM elt s =
    let s = remove elt s in
    add elt s
  ;;
end

type table = MTableSet.t

let max = Int.max

let prog =
  CompoundStm
    ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3))
    , CompoundStm
        ( AssignStm
            ( "b"
            , EseqExp
                ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ]
                , OpExp (NumExp 10, Times, IdExp "a") ) )
        , PrintStm [ IdExp "b" ] ) )
;;

let rec maxargs = function
  | AssignStm _ -> 0
  | PrintStm e ->
    List.fold_left
      (fun a esub ->
         match esub with
         | EseqExp (s, _) -> max (maxargs s) a
         | _ -> a)
      (List.length e)
      e
  | CompoundStm (s1, s2) -> max (maxargs s1) (maxargs s2)
;;

(* let rec update table id int =
   match table with
   | [] -> (id, int) :: table
   | (t_id, t_int) :: xs ->
   if t_id = id then (t_id, int) :: xs else (t_id, t_int) :: update xs id int
   ;; *)

let[@inline] update table id int = MTableSet.addM (id, int) table
let[@inline] assoc id table = MTableSet.find (id, 0) table |> snd

let rec interpStm stm table =
  match stm with
  | AssignStm (id, exp) ->
    let res, t = interpExp exp table in
    update t id res
  | PrintStm es ->
    let s, t = string_of_expr es table "" in
    let () = Printf.printf "%s" s in
    t
  | CompoundStm (s1, s2) -> interpStm s2 (interpStm s1 table)

and interpExp exp table =
  match exp with
  | IdExp id -> assoc id table, table
  | NumExp i -> i, table
  | OpExp (e1, op, e2) ->
    let i1, t = interpExp e1 table in
    let i2, t = interpExp e2 t in
    (interpOp op) i1 i2, t
  | EseqExp (stm, e) ->
    let t = interpStm stm table in
    let res, t = interpExp e t in
    res, t

and string_of_expr es t l =
  match es with
  | [] -> l ^ "\n", t
  | x :: xs ->
    let i, t = interpExp x t in
    string_of_expr xs t (l ^ string_of_int i)

and[@inline] interpOp = function
  | Plus -> ( + )
  | Minus -> ( - )
  | Times -> ( * )
  | Div -> ( / )
;;

let test () =
  interpStm prog MTableSet.empty
  |> MTableSet.to_list
  |> List.iter (fun (id, v) -> Format.printf "%s = %d; " id v)
;;
