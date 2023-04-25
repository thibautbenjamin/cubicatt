open Stdlib

type var =
  | Name of string
  | New of int

let string_of_var v =
  match v with
  | Name s -> s
  | New i -> "_" ^ string_of_int i

let make_var s = Name s

  (** A raw type. *)
  type ty =
    | Letin_ty of var * tm * ty
    | Obj
    | Path of var * ty * tm * tm
  (** A raw term. *)
   and tm =
    | Letin_tm of var * tm * tm
    | Var of var
    | Sub of tm * (tm list)

  let rec string_of_ty e =
    match e with
    | Letin_ty (v,e,ty) -> Printf.sprintf "let %s = %s in %s" (string_of_var v) (string_of_tm e) (string_of_ty ty)
    | Obj -> "*"
    | Path (i,ty,u,v) -> Printf.sprintf "Path^%s %s %s %s" (string_of_var i) (string_of_ty ty) (string_of_tm u) (string_of_tm v)
  and string_of_tm e =
    match e with
    | Letin_tm (v,e,tm) -> Printf.sprintf "let %s = %s in %s" (string_of_var v) (string_of_tm e) (string_of_tm tm)
    | Var x -> string_of_var x
    | Sub (t,s) -> Printf.sprintf "(%s %s)" (string_of_tm t) (string_of_sub s)
  and string_of_sub s =
    match s with
    | [] -> ""
    | t::s -> Printf.sprintf "%s %s" (string_of_tm t) (string_of_sub s)

  (** List the variables of an non-checked term (ie only the explicit variables)*)
  let rec list_vars e =
    match e with
    | Letin_tm _ -> assert false
    | Var v -> [v]
    | Sub (_,l) -> List.unions (List.map list_vars l)

  (** remove the let in in a term *)
  (* TODO : write this function *)
  let replace_tm _l _e =
    assert false
  and replace_ty _l _t =
    assert false


  (* let rec replace_tm l e =
   *   match e with
   *   | Var a ->
   *      begin
   *        try replace_tm l (List.assoc a l)
   *        with
   *          Not_found -> Var a
   *      end
   *   | Sub (e,s,func) -> Sub(replace_tm l e, List.map (replace_tm l) s,func)
   *   | Letin_tm (v,t,tm) -> replace_tm ((v,t)::l) tm
   * and replace_ty l t =
   *   match t with
   *   | Obj -> t
   *   | Arr(i,ty,u,v) -> Arr (replace_tm l u, replace_tm l v)
   *   | Letin_ty (v,t,ty) -> replace_ty ((v,t)::l) ty *)

  let remove_let_tm e =
    replace_tm [] e

  let remove_let_ty e =
    replace_ty [] e

  (** replace the term Var v1 by the term Var v2 in the list l *)
  let rec replace_tm_list l v1 v2 =
    match l with
    |[] -> []
    |(Var v)::l when v == v1-> (Var v2)::l
    |t::l -> t::(replace_tm_list l v1 v2)
