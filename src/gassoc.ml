open Syntax
open Common
       
module type EVar = sig
  type t

  val make : var -> t
  val new_fresh : unit -> t
  val to_var : t -> var
  val to_string : t -> string
end                         
                       
module type EVal = sig
  type t

  val mk_coh : string -> (var * ty) list -> ty -> t
  val mk_let : string -> (var * ty) list -> tm -> t * string
  val mk_let_check : string -> (var * ty) list -> tm -> ty -> t * string

  val dim : t -> int
end
                     
module GAssoc (A : EVar) (B : EVal) = struct

  type a = A.t
  type t = B.t
             
  let env = ref ([] : (a * t) list)

  let init () = env := []
                
  exception Found of t

  (** Add a variable together with the corresponding coherence*)
  let add_coh x ps t =
    let t = B.mk_coh (string_of_var x) ps t in
    let x = A.make x in
    env := (x,t)::!env

  (** Add a variable together with the corresponding let term*)
  let add_let x c u =
    (* debug "adding %s" (Var.to_string x); *)
    let u,msg = B.mk_let (string_of_var x) c u in
    let x = A.make x in
    env := (x,u)::!env;
    msg

  (** Add a variable together with the corresponding let term whose type is checked by the user*)
  let add_let_check x c u t =
    (* debug "adding %s" (Var.to_string x); *)
    let u,msg = B.mk_let_check (string_of_var x) c u t in
    let x = A.make x in
    env := (x,u)::!env;
    msg

      
  let val_var x =
    List.assoc x !env
end
