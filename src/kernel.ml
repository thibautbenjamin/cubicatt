open Stdlib
open Settings
open Common
open Syntax
open Gassoc

(** Environment variables (i.e. defined coherences or let definitions). *)
module EVar
: sig
  type t
  val to_string : t -> string
  val make : var -> t
  val to_var : t -> var
  val new_fresh : unit -> t
end
=
struct
  type t = var

  let next_fresh = ref (New 0)
             
  let to_string v =
    string_of_var v

  let make v = v

  let to_var v = v

  let new_fresh () =
    let res = !next_fresh in
    let nxt = match res with
           |New k -> New (k+1)
           |_ -> assert (false)
    in next_fresh := nxt; res                      
end

(** Context variables (i.e. "arguments of functions"). *)
module CVar
: sig 
    type t
    val to_string : t -> string
    val make : var -> t
    val to_var : t -> var
    val compute_vars : (t*'a) list -> t list
end
=
struct
  type t = var

  let to_string v =
    string_of_var v
	       
  let make v = v

  let to_var v = v

  let compute_vars l = List.map fst l
end

module DVar
: sig
  type t = private
         |Var of var
         |Z
         |O
         |New of int

  val to_string : t -> string
  val make : var -> t
  val print_list : t list -> string
  val zero : t
  val one : t
  val new_var : unit -> t
end
=
struct
  type t =
    |Var of var
    |Z (* constant zero *)
    |O (* constant one *)
    |New of int

  let count = ref 0
              
  let to_string v =
    match v with
    |Var v ->  string_of_var v
    |_ -> assert false
	       
  let make v = Var v

  let rec print_list l =
    match l with
    |[] -> ""
    |(h::t) -> Printf.sprintf "%s %s" (to_string h) (print_list t)

  let zero = Z
  let one = O

  let new_var ()  = incr(count); New !count
end

  
type evar = EVar.t
type cvar = CVar.t

let var_of_cvar = CVar.to_var

(** Dimension variables *)

    

(** Operations on substitutions. *)
module rec Sub
 :
sig
  type t

  (* Structural functions *)
  val mk : Tm.t list -> Ctx.t -> Ctx.t  -> t
	   
  (* Syntactic properties *)		    
  val free_vars : t -> cvar list
  val apply_Ty : t -> Ty.t -> Ty.t
  val apply_Tm : t -> Tm.t -> Tm.t
           
  (* Equality procedures *)
  val check_equal : t -> t -> unit

  (* Printing *)	
  val to_string : t ->  string
end
  =
struct
  (** A substitution. *)
  type t = {list : Tm.t list; src : Ctx.t; tar : Ctx.t}
	     
  (** Free context variables. *)
  let free_vars s =
    List.concat (List.map (Tm.free_vars s.src) s.list)


  let rec apply_list_var l tar x = 
      match l,tar with
      |_,_ when Ctx.is_empty tar ->
        assert false
      |t::l, _ ->
        let open Tm in
        let ((y,_),tar) = (Ctx.head tar, Ctx.tail tar) in
        if y = x
        then t
        else apply_list_var l tar x
      |[], _ -> assert false
  and compose_lists src tar s s' =
    List.rev (List.map (fun t -> apply_list_Tm s tar src t) s')
  (** Apply a substitution to a term. *)
  and apply_list_Tm s tar src tm =
    let open Tm in
    let e =
      match tm with
      |CVar x -> apply_list_var s tar x
      |Sub (x,v,s') ->
        let newtar = EnvVal.ctx v in 
        Sub (x,v, Sub.mk (compose_lists src tar s s'.list) src newtar)
      |App (t,i) -> App(apply_list_Tm s tar src t, i)
    in e
  (** Apply a substitution to a type. *)
  and apply_list_Ty s tar src ty =
    let open Ty in
    let e = 
      match ty with
      | Obj -> Obj
      | Path (i,a,u,v) ->
         let a = apply_list_Ty s tar src a in
         let u = apply_list_Tm s tar src u in
         let v = apply_list_Tm s tar src v in Path(i,a,u,v)
    in e

  let rec print l =
    match l with
    |t::l -> Printf.sprintf "%s %s" (Tm.to_string t) (print l)
    |[] -> ""

  let mk l src tgt =
    (* debug "making substitution %s \n with target %s" *)
          (* (print l) (Ctx.to_string tgt); *)
    let rec aux l (tgt : Ctx.t) =
      match l,Ctx.value tgt with
      |[],[] -> []
      |(_::_,[] |[],_::_) -> raise NotValid
      |t::s,_ -> 
	let (x,u),tar = (Ctx.head tgt,Ctx.tail tgt) in
	let s = aux s tar in 
	let ty = Tm.infer [] src t in
	let () = Ty.check_equal [] src ty (apply_list_Ty s tar src u)
	in t::s
    in {list = aux (List.rev l) tgt; src = src; tar = tgt}


  let apply_Tm s tm =
     apply_list_Tm s.list s.tar s.src tm

  let apply_Ty s ty =
    apply_list_Ty s.list s.tar s.src ty

  (** Check equality of substitutions. *)
  (* TODO : Check the sources too*)
  let check_equal (s1:t) (s2:t) =
    Ctx.check_equal s1.tar s2.tar;
    let ctx = s1.tar in
    let rec check_list s1 s2 = 
      match s1,s2 with
      | [],[] -> ()
      | t1::s1,t2::s2 ->
         Tm.check_equal ctx t1 t2;
         check_list s1 s2
      | _,_ -> raise NotValid
    in check_list s1.list s2.list 

  (** String representation of a substitution. We print only maximal elements *)
  let to_string (s:t) =
    let rec print_list s =
      match s with
      | [] -> ""
      | (u::s) -> Printf.sprintf "%s %s" (print_list s) (Tm.to_string u) 
    in print_list s.list



  (* TODO : Implement elaboration of substitution 
     - write an mk_elaborated
     - write list_expl_vars
     - write explicit  
     - change mk to mk_elaborated in apply_Tm and apply_Ty
   *)

  (* TODO : Implement suspension and functorialisation 
     - write to_string_func to print with functorialised arguments   
   *)
end

(** A context, associating a type to each context variable. *)
and Ctx
    :
sig
  type t = private (cvar * Ty.t) list
                     
  (* Makers *)
  val empty : unit -> t
  val add : t -> var -> ty -> t
  val add_safe : t -> CVar.t -> Ty.t -> t
  val make : (var * ty) list -> t
  val of_ps : PS.t -> t
       
  (* Structural operations *)
  val head : t -> cvar * Ty.t
  val tail : t -> t
                              
  (* Syntactic properties *)
  val ty_var : t -> cvar -> Ty.t
  val domain : t -> cvar list
  val value : t -> (cvar * Ty.t) list
  val mem : t -> cvar -> bool
  val dim : t -> int
  val vars : t -> CVar.t list
                   
  (* Equality procedure *)
  val is_empty : t -> bool
  val check_equal : t -> t -> unit

  val make_safe : (CVar.t * Ty.t) list -> t
                                
  (* Printing *)
  val to_string : t -> string
end
  =
struct
  (** A context. Variables together with a type a a boolean indicating if the variable is explicit or implicit*)
  
  type t = (cvar * Ty.t) list


  (** type of a variable in a context. *)
  let ty_var (ctx:Ctx.t) x =
    (* debug "looking up var %s in context %s" (CVar.to_string x) (Ctx.to_string ctx); *)
    try
     List.assoc x (Ctx.value ctx)
    with
    | Not_found -> raise (UnknownId (CVar.to_string x))

  (* ------ Makers ------ *)
  (** Empty context. *)
  let empty () = []

  let add (c:Ctx.t) x u =
    let x = CVar.make x in
    try
      ignore (List.assoc x (c :> t));
      raise (DoubleDef (CVar.to_string x))
    with Not_found ->
          let u = Ty.make [] c u in
          (x,u)::(c :> t)

  let add_safe (c:Ctx.t) x u =
    try
      ignore (List.assoc x (c :> t));
      raise (DoubleDef (CVar.to_string x))
    with Not_found ->
          Ty.check [] c u;
          (x,u)::(c :> t)
    
                         
  (** Create a context from a list of terms. *)
  let rec make l =
    let rec aux l ctx =
      match l with
      | [] -> ctx
      | (x,t)::l ->
	 let ctx = Ctx.add ctx x t in
	 aux l ctx
    in aux l (Ctx.empty ())

  let make_safe l =
    let rec aux l ctx =
      match l with
      |[] -> ctx
      |(x,t)::l ->
        let ctx = Ctx.add_safe ctx x t in
        aux l ctx
    in aux l (Ctx.empty ())


  (** Create a context from a pasting scheme. *)
  let of_ps ps =
    PS.ctx ps
           
  (** First element of a context. *)
  let rec head ctx =
    match ctx with
    |[] -> assert false
    |a::_ -> a
           
  (** Tail of a context. *)
  let rec tail ctx =
    match ctx with 
    |[] -> assert false
    |_::l -> l
                   
       
  (** Domain of definition of a context. *)
  let domain ctx = List.map fst ctx

  let value (ctx : t) = ctx
    
  (** Check whether a variable belongs to a context. *)
  let mem (c:t) v =
    let rec aux c =  
      match c with
      | [] -> false
      | (x,_)::c when x = v -> true
      | _::c -> aux c
    in aux c
	      
  (** Is a context empty? *)
  let is_empty (c:t) =
    c = []

  let max_used_var ctx =
    let rec aux n l =
      match l with
      |[] -> n
      |v::l ->
        match CVar.to_var v with
        |Name _ -> aux n l 
        |New k -> aux (max k n) l
    in aux 0 (domain ctx)
                  
           
  (** Equality of contexts. *)
  let check_equal ctx1 ctx2 =
    let rec equal c (ctx1 : Ctx.t) (ctx2 : Ctx.t) =
      match (ctx1 :> t), (ctx2 :> t) with
      | [],[] -> ()
      | (v1,x1)::_, (v2,x2)::_ ->
         let t1 = Ctx.tail ctx1 and t2 = Ctx.tail ctx2 in
         if not (v1 = v2) then raise NotValid;
         Ty.check_equal [] c x1 x2;
         equal ctx1 t1 t2
      | _,_ -> raise NotValid
    in equal (Ctx.empty ()) ctx1 ctx2
	
  (** String representation of a context. *)
  let rec to_string ctx =
    match ctx with
    | [] -> ""
    | (x,t)::c ->
       Printf.sprintf "%s (%s,%s)"
         (to_string c)
	 (CVar.to_string x)
         (Ty.to_string t)
                      
  (** dimension of a context is the maximal dimension of its variables *)
  let dim ctx =
    let rec aux c i = match c with
      |[] -> i
      |(_,ty)::c when Ty.dim ty>i -> aux c (Ty.dim ty)
      |_::c -> aux c i
    in aux ctx 0

  let vars ctx =
    List.map fst ctx

             
  (* TODO : suspension and functorialisation
     - write suspend
     - write functorialize 
   *)
end


(** Operations on pasting schemes. *)
and PS
: sig
  type t
         
  (* Maker *)
  val mk : Ctx.t -> t 

  (* Syntactic properties *)
  val domain : t -> cvar list

  (* Structural operations *)
  val dim : t -> int
  val source : int -> t -> t
  val target : int -> t -> t

  val vars : t -> CVar.t list

  val ctx : t -> Ctx.t
  (* Printing *)
  val to_string : t -> string
end
  =
struct
  exception Invalid
  (** A pasting scheme. *)
  type shape =  
    |PStart 
    |PExt of shape * int
  type t = shape * Ctx.t
                   
  (** Domain of definition. *)
  let domain ps = Ctx.domain (Ctx.of_ps ps)

  let ctx = snd
                             
  let rec check_equal_shape sh1 sh2 =
    match sh1,sh2 with
    |PStart, PStart -> ()
    |PExt(sh1,n1), PExt(sh2,n2) when n1 = n2 -> check_equal_shape sh1 sh2
    |_ -> assert false

  let rec check_src_vars l_vars ctx dim =
    match l_vars,ctx with
    |[], [] -> ()
    |v::l_vars, (v',ty)::ctx ->
      Ty.check_src_var_in_dim dim v ty;
      check_src_vars l_vars ctx dim
    |_,_ -> raise Invalid

  let rec check_tgt_vars l_vars ctx dim =
    match l_vars,ctx with
    |[], [] -> ()
    |v::l_vars, (v',ty)::ctx ->
      Ty.check_tgt_var_in_dim dim v ty;
      check_tgt_vars l_vars ctx dim
    |_,_ -> raise Invalid


  (* TODO : move chop *)
  let rec chop l k =
      assert (k >= 0);
      if k = 0 then [],l
      else
        match l with
        |[] -> assert false
        |a::l -> let res = chop l (k-1) in
                 a::(fst res), snd res

  let rec slices_of_length len l =
    let l,rest = chop l len in
    match rest with
    |[] -> [l]
    |_ -> l::(slices_of_length len rest)
 

    
  (** Create a pasting scheme from a context. *)
  let mk (l : Ctx.t) : t =
    (* TODO : Clean this algorithm *)
    let rec count l dim_max dim_min res =
      match l with
      |[] -> res
      |n::k::l when n = dim_min && k = dim_max ->
        let res = 
          match res with
          |a::res -> 1::(a+1::res)
          |[] -> assert false
        in count l dim_max dim_min res 
      |k::l ->
        let res = 
          match res with
          |a::res -> a+1::res
          |[] -> 1::[]
        in
        count l dim_max dim_min res
    in
    let  common l =
      let rec check l a =
        match l with
        |[] -> a
        |h::l -> check_equal_shape h a; check l a
      in
      match l with
      |[] -> assert false
      |h::l -> check l h
    in
    let rec chop_all vars0 dim_glue l l_dim n_list =
      match n_list with
      |[] -> begin
          match l with
          |[] -> []
          |_ -> assert false
        end
      |n::n_list -> let chopped1, rest = chop l n in
                    let chopped1_dim, rest_dim = chop l_dim n in
                    let chopped2, rest = chop rest n in
                    let chopped2_dim, rest_dim = chop rest_dim n in
                    let vars1 = CVar.compute_vars chopped1 in
                    check_src_vars vars0 chopped2 dim_glue;
                    check_tgt_vars vars1 chopped2 dim_glue;
                    (compute_shape chopped1 chopped1_dim) ::
                      (compute_shape chopped2 chopped2_dim) ::
                        (chop_all vars1 dim_glue rest rest_dim n_list)
    and compute_shape l l_dim =
      match l with
      |[] -> raise Invalid
      |(x,ty)::[] -> PStart
      |_ ->
        let min = List.min l_dim and max = List.max l_dim in
        let cuts = List.rev (count l_dim min max []) in
        let n = List.length cuts in
        let cuts =
          match cuts with
          |[] -> assert false
          |n::cuts ->
            assert (n mod 3 = 0);
            n/3::(List.map (fun x -> assert (x mod 2 = 0); x/2) cuts)
        in
        let shape_list =
          match cuts with
          |[] -> assert false
          |k::_ -> let chopped,l = chop l k in
                   let chopped_dim,l_dim  = chop l_dim k in
                   let vars = CVar.compute_vars chopped in
                   (compute_shape chopped chopped_dim) :: (chop_all vars min l l_dim cuts)
        in  let sh = common shape_list in PExt(sh,n)
    in
    let init l =
      let l = List.rev l in
      match l with
      |[] -> raise Invalid
      |(x,ty)::[] when Ty.dim ty = 0 -> PStart (* Only type of dimension 0 is Obj *)
      |_::[] -> raise Invalid
      |_ -> let list_dim  = (List.map (fun x -> Ty.dim (snd x))) l in
            compute_shape l list_dim 
    in init (Ctx.value l), l
           

  let dim ps =
    let rec dim_shape sh =
      match sh with
      |PStart -> 0
      |PExt (sh,_) -> 1+(dim_shape sh)
    in dim_shape (fst ps)
                   
  let to_string ps = Ctx.to_string (snd ps)

  let rec size_shape sh =
    match sh with
    |PStart -> 1
    |PExt (sh,n) -> (3 + 2 * (n - 1)) * size_shape sh

  let rec border_shape i sh =
    match sh with
    |PStart -> assert false
    |PExt (sh,n) ->
      if i = 0 then sh
      else PExt (border_shape (i-1) sh,n)
                                                   
  let rec print_ctx_list l =
    match l with
    |(v,ty)::l ->
      Printf.sprintf "(%s:%s) %s" (CVar.to_string v) (Ty.to_string ty) (print_ctx_list l)
    |[] -> ""

  let rec print l = 
    match l with
    |l::rest ->
      Printf.sprintf "%s \n %s" (print_ctx_list l) (print rest)
    |[] -> ""

             
  let source i (sh,ctx) =
    (* debug "calculating source of context %s" (Ctx.to_string ctx); *)
    let rec src i sh ctx = 
      match sh with
      |PStart -> assert false
      |PExt (sh,n) ->
        let len = size_shape sh in
        if i = 0 then
          begin
          (* debug "list of slices is \n %s" (print (slices_of_length len ctx)); *)
          List.rev(List.hd (List.rev (slices_of_length len ctx)))
                  (* fst (chop ctx len) *)
          end
        else
          List.unions (List.map (src (i-1) sh) (slices_of_length len ctx))
    in
    let res = src i sh (Ctx.value ctx) in
    (* debug "res is %s" (print_ctx_list res); *)
    PS.mk (Ctx.make_safe res)
    (* PS.mk (Ctx.make_safe (src i sh (Ctx.value ctx))) *)
                  
  let target i (sh,ctx) = 
    let rec tgt i sh ctx = 
      match sh with
      |PStart -> assert false
      |PExt (sh,n) ->
        let len = size_shape sh in
        if i = 0 then
          begin
          (* debug "list of slices is \n %s" (print (slices_of_length len ctx)); *)
          List.rev(List.hd (List.tl (slices_of_length len ctx)))
                  (* fst (chop ctx len) *)
          end
        else
          List.unions (List.map (tgt (i-1) sh) (slices_of_length len ctx))
    in
    let res = tgt i sh (Ctx.value ctx) in
    (* debug "res is %s" (print_ctx_list res); *)
    PS.mk (Ctx.make_safe res)
    (* PS.mk (Ctx.make_safe (src i sh (Ctx.value ctx))) *)

                               
  let vars (_,c) = Ctx.vars c
end

and EnvVal
: sig
  type v =
    |Coh of Coh.t
    |Let of Tm.t
  type t = {print : string * int list; value : v}      

  val mk_coh : string -> (var * ty) list -> ty-> t
  val mk_let : string -> (var * ty) list -> tm -> t * string
  val mk_let_check : string -> (var * ty) list -> tm -> ty -> t * string

  val dim : t -> int
                         
  val ty :  t -> Ty.t
  val ctx : t -> Ctx.t
  val check_equal : t -> Tm.t -> Sub.t -> t -> Tm.t -> Sub.t -> Ctx.t -> unit
end
= 
struct
  type v =
    |Coh of Coh.t
    |Let of Tm.t

  type t = {print : string * int list; value : v}

  let mk_coh nm ps t =
  let ps = Ctx.make ps in
  let c = Coh.mk ps t in
  {print = (nm,[]); value = Coh c}

  let mk_let nm c u =
  let c = Ctx.make c in
  let u,ty = Tm.make [] c u in
  (* let u = Tm.mark_ctx u in *)
  {print = (nm,[]); value = Let u}, Ty.to_string ty

  let mk_let_check nm c u t =
    assert false
  (* TODO : think about what list of DVar to give *)
  (* let c = Ctx.make c in
   * let u,ty = Tm.make c u in
   * let t = Ty.make c t in
   * Ty.check_equal c t ty;
   * (\* let u = Tm.mark_ctx u in *\)
   * {print = (nm,[]); value = Let u}, Ty.to_string t *)

  (* TODO in dim ty and ctx : allow cut to perform on Let*)
  let dim v =
    match v.value with
    |Coh c -> Coh.dim c
    |Let t -> assert false


  let ty v =
    match v.value with
    |Coh coh -> Coh.target coh
    |Let t -> assert false

  let ctx v =
    match v.value with
    |Coh c -> (Ctx.of_ps (Coh.ps c))
    |Let t -> assert false

  let print_list l = 
    let rec aux l =
      match l with
      |[] -> ""
      |i::l -> Printf.sprintf "%d %s" i (aux l)
    in Printf.sprintf "[%s]" (aux l)

  let check_equal v1 tm1 s1 v2 tm2 s2 src =
    match (v1.value, v2.value) with
    |Coh c1, Coh c2 -> Sub.check_equal s1 s2
    |Let t1, Let t2 -> Tm.check_equal src (Sub.apply_Tm s1 t1) (Sub.apply_Tm s2 t2)
    |Let t, Coh c -> Tm.check_equal src (Sub.apply_Tm s1 t) tm2
    |Coh c, Let t -> Tm.check_equal src tm1 (Sub.apply_Tm s2 t)

end
      
and Ty
: sig
  type t = 
    | Obj
    | Path of DVar.t * t * Tm.t * Tm.t

           
  val free_vars : Ctx.t -> t -> cvar list
  val to_string : t -> string

  val check : DVar.t list -> Ctx.t -> t -> unit
  val check_equal : DVar.t list -> Ctx.t -> t -> t -> unit
  val make : DVar.t list -> Ctx.t -> ty -> t

  val check_src_var_in_dim : int -> CVar.t -> t -> unit
  val check_tgt_var_in_dim : int -> CVar.t -> t -> unit
                                                     
  val dim : t -> int
  val source : int -> t -> DVar.t list * Tm.t 
  val target : int -> t -> DVar.t list * Tm.t
  val sub_DVar : DVar.t list -> Ctx. t -> t -> DVar.t -> DVar.t -> t
end
  =
struct
  (** A type exepression. *)
  type t =
    | Obj
    | Path of DVar.t * t * Tm.t * Tm.t
                                    
  exception Unknown

  (** Free variables of a type. *)
  let rec free_vars ctx ty =
    match ty with
    | Obj -> []
    | Path (_,t,u,v) ->  List.unions [free_vars ctx t; Tm.free_vars ctx u; Tm.free_vars ctx v]
  (* The dimension variables are not counted *)

  let rec to_string ty =
    match ty with
    | Obj -> "*"
    | Path (i,t,u,v) ->
       Printf.sprintf "Path^%s (%s) (%s) (%s)" (DVar.to_string i) (to_string t) (Tm.to_string u) (Tm.to_string v)
           
  (** Test for equality. *)
  let rec check_equal l ctx ty1 ty2 =
    let equal = check_equal l ctx in
    match ty1, ty2 with
    | Obj,Obj -> ()
    | Path(i1,t1,u1,v1),Path(i2,t2,u2,v2) when i1 = i2 ->
      equal t1 t2; Tm.check_equal ctx u1 u2; Tm.check_equal ctx v1 v2
    | Path(i1,t1,u1,v1),Path(i2,t2,u2,v2) ->
      equal t1 (Ty.sub_DVar l ctx t2 i2 i1); Tm.check_equal ctx u1 u2; Tm.check_equal ctx v1 v2
    | (Obj |Path _),_ ->
      raise (NotEqual (to_string ty1, to_string ty2))

  let rec sub_DVar l ctx ty i r =
    match ty with
    | Obj -> Obj
    | Path(j,a,t,u) when r = j ->
                         let newdim = DVar.new_var () in
                         let a = sub_DVar (newdim::l) ctx a i newdim in
                         let a = sub_DVar (j::l) ctx a i r in
                         let t = Tm.sub_DVar l ctx t i r in
                         let u = Tm.sub_DVar l ctx u i r in
                         Path(j,a,t,u)
    | Path (j,a,t,u) ->
                         let a = sub_DVar (j::l) ctx a i r in
                         let t = Tm.sub_DVar l ctx t i r in
                         let u = Tm.sub_DVar l ctx u i r in
                         Path(j,a,t,u)

  (** Ensure that a type is well-formed in given context. *)
  let rec check l ctx ty =
    match ty with
    |Obj -> ()
    |Path (i,ty,u,v) ->
      let tu = Tm.infer l ctx u in
      let tv = Tm.infer l ctx v in
      Ty.check (i::l) ctx ty;
      check_equal l ctx (sub_DVar l ctx ty i DVar.zero) tu; 
      check_equal l ctx (sub_DVar l ctx ty i DVar.one) tv
                             
  (** Construct a type. *)
  let rec make l c (e : Syntax.ty) =
    match e with
    |Obj -> Obj
    |Path (i,ty,u,v) ->
      let u,tu = Tm.make l c u in
      let v, tv = Tm. make l c v in
      let i = DVar.make i in 
      let ty = Ty.make (i::l) c ty in
      let _ = check_equal l c (sub_DVar l c ty i DVar.zero) tu in 
      let _ = check_equal l c (sub_DVar l c ty i DVar.one) tv in 
      Path (i,ty,u,v)
    |letin_ty -> assert false

  (** Dimension of a type. *)
  let rec dim ty =
    match ty with
    | Obj -> 0
    | Path(_,a,_,_) -> 1 + dim a

  let rec check_src_var_in_dim i v ty =
    match ty with
    | Obj -> assert false 
    | Path(_,a,u,_) when dim a = i -> Tm.check_var v u;
    | Path (_,a,_,_) -> check_src_var_in_dim i v a

  let rec check_tgt_var_in_dim i v ty =
    match ty with
    | Obj -> assert false 
    | Path(_,a,_,u) when dim a = i -> Tm.check_var v u;
    | Path (_,a,_,_) -> check_tgt_var_in_dim i v a 

  let source i ty =
    let rec src i l ty =
      (* debug "computing source of %s in dim %d" (to_string ty) i; *) 
      match ty with
      | Obj -> assert false
      | Path (j,a,u,_) when dim a = i ->
         l,u
      | Path(j,a,_,_) ->
         src i (j::l) a
    in src i [] ty
         
  let target i ty =
    let rec tgt i l ty = 
      match ty with
      | Obj -> assert false
      | Path (j,a,_,u) when i = dim a ->
         l,u
      | Path(j,a,_,_) ->
         tgt i (j::l) a
    in tgt i [] ty       
end

(** Operations on terms. *)
and Tm
    :
sig
  type t = 
    | CVar of cvar
    | Sub of evar * EnvVal.t * Sub.t
    | App of t * DVar.t
           
  val free_vars : Ctx.t -> t -> cvar list
  val to_string : t -> string

  val infer : DVar.t list -> Ctx.t -> t -> Ty.t
  val check_equal : Ctx.t -> t -> t -> unit
  val check_type : DVar.t list -> Ctx.t -> t -> Ty.t -> unit
  val make : DVar.t list -> Ctx.t -> tm -> t * Ty.t

  val sub_DVar : DVar.t list -> Ctx.t -> t -> DVar.t -> DVar.t -> t
  val check_var : CVar.t -> t -> unit
end
  =
struct
  (** An expression. *)
  type t =
    | CVar of cvar (** a context variable *)
    | Sub of evar * EnvVal.t * Sub.t (** a substituted environment variable *)
    | App of t * DVar.t
                                 
  exception Unknown
             
  let rec free_vars ctx tm =
    match tm with
    | CVar x -> x :: (Ty.free_vars ctx (Ctx.ty_var ctx x))
    | Sub (_,_,sub) -> Sub.free_vars sub
    | App(t,_) -> free_vars ctx t
                     
  let rec to_string tm =
    match tm with
    | CVar x -> CVar.to_string x
    | Sub (x,v,s) ->
       let open EnvVal in
       Printf.sprintf "(%s %s)" (fst(v.print)) (Sub.to_string s)
    | App (t,i) -> Printf.sprintf "(%s %s)" (to_string t) (DVar.to_string i)

  let rec check_equal ctx tm1 tm2 =
    match tm1, tm2 with
    | CVar x,CVar y ->
      if not (x = y)
      then
	raise (NotEqual (to_string tm1, to_string tm2))
      else ()
    | Sub(x,v1,s1),Sub(y,v2,s2) ->
       EnvVal.check_equal v1 tm1 s1 v2 tm2 s2 ctx
    | App(t,r), App(t',r') when r = r' ->
       check_equal ctx t t'
    | (CVar _|Sub _|App _),_ ->
       raise (NotEqual (to_string tm1, to_string tm2))

  (** Infer the type of an expression. *)
  let rec infer l ctx tm =
    match tm,l with
    | CVar x,[] -> Ctx.ty_var ctx x
    | CVar x,_ -> failwith "the cubes must be strict"
    | Sub (_,v,s),_ ->
       let ty = EnvVal.ty v in
       Sub.apply_Ty s ty
    | App (t,r),l ->
       let l = match l with
       |i::l when i = r -> l
       |_ -> assert false
       in
       let ty = infer l ctx t in
       match ty with
       |Ty.Path(i,a,u,v) -> Ty.sub_DVar l ctx a i r
       |_ -> assert false


  (** Check that term has given type. *)
  let check_type l ctx e t  =
    assert false
  (* TODO : think which dim var list to give *)
    (* Ty.check_equal l ctx (infer l ctx e) t *)

  (** Create a term from an expression. *)
  let rec make l c e =
    (* debug "making term %s in context %s and dim vars %s" (string_of_tm e) (Ctx.to_string c) (DVar.print_list l); *) 
    match e,l with
    | Var v,_ ->
       let e = CVar (CVar.make v) in let ty = infer l c e in  e,ty
    | Sub (tm,(Var r)::[]),r'::l when (DVar.make r) = r' ->
       let t,_ = make l c tm in
       let e = App (t,DVar.make r) in
       let ty = infer (r'::l) c e in
       e,ty
    | Sub (t,s),_ ->
      let s = List.map (Tm.make l c) s in
      let v,t = match t with
        |Var v -> let v = EVar.make v in v,Env.val_var v 
        |_ -> assert false
      in let ty = EnvVal.ty t in
         let tar = EnvVal.ctx t in
         let s = Sub.mk (List.map fst s) c tar in
         let ty = Sub.apply_Ty s ty in
         Sub(v,t,s), ty
    | _ -> assert false
    
  let rec sub_DVar l ctx tm i r =
    match tm with
    | CVar v -> CVar v
    | App (t,j) when i = j ->
       let src,tgt = 
         match infer l ctx t with
         | Ty.Path(_,_,u,v) -> u,v
         | _ -> assert false
       in
       begin
         match r with
         |DVar.Z -> src
         |DVar.O -> tgt
         |_ -> App (t,r)
       end
    | App (t,j) -> let l = match l with
                     | k :: l when k = j -> l
                     | _ -> assert false
                   in App (sub_DVar l ctx t i r, j)
    | Sub _ -> assert false

  let rec check_var v t =
    match t with
    | CVar v' when v = v' -> () 
    | CVar v' -> assert false
    | App (t,_) -> check_var v t
    | Sub _ -> assert false
end
  
(** A coherence. *)
and Coh    
    : sig
  type t = private (PS.t * Ty.t)   

  val mk : Ctx.t -> ty -> t
  val to_string : t -> string
  val check_equal : t -> t -> Ctx.t
  val ps : t -> PS.t
  val dim : t -> int
  val target : t -> Ty.t
end
=
struct
  type t = PS.t * Ty.t
	        
  let mk ps t =
    (* TODO : write a real typing rule *)
    let ty = Ty.make [] ps t in
    let ps = PS.mk ps in
    if List.included (PS.vars ps) (Ty.free_vars (Ctx.of_ps ps) ty)
    then ps,ty
    else
      let n = PS.dim ps in
      let rec check i =
        (* debug "checking algebraicity in dim %d" i; *)
        if (i < 0) then ()
        else
          let pss = PS.source i ps in
          let l,tms = Ty.source i ty in
          let _ = Tm.infer l (Ctx.of_ps pss) tms in
          let pst = PS.target i ps in
          let l,tmt = Ty.target i ty in
          let _ = Tm.infer l (Ctx.of_ps pst) tmt in
          (* debug "in dim %d : \n the source ps is %s \n the source (term,ty) is (%s, %s)\n the target ps is %s \n the target (term,ty) is (%s, %s)"
           * i (PS.to_string pss) (Tm.to_string tms) (Ty.to_string tys) 
           *      (PS.to_string pst)(Tm.to_string tmt) (Ty.to_string tyt); *)
          if (List.included (PS.vars pss) (Tm.free_vars (Ctx.of_ps pss) tms)
              && List.included (PS.vars pst) (Tm.free_vars (Ctx.of_ps pst) tmt))
          then check (i - 1)
          else raise NotAlgebraic
      in check (n - 1); ps,ty

  let to_string (ps,t) =
    Printf.sprintf "Coh {%s |- %s}"
      (PS.to_string ps)
      (Ty.to_string t)

  let check_equal (ps1,t1) (ps2,t2) =
    assert false

  let ps (ps,t) = ps

  let target (ps,t) = t

  let dim (ps,t) = PS.dim ps
end

(** Operations on environments. *)
and Env : sig 

  val add_let : var -> (var * ty) list -> tm -> string
  val add_let_check : var -> (var * ty) list -> tm -> ty -> string
  val add_coh : var -> (var * ty) list -> ty -> unit
  val init : unit -> unit
  val val_var : EVar.t -> EnvVal.t
end
  = GAssoc(EVar)(EnvVal) 

  
type kTm = Tm.t
type kTy = Ty.t

type ctx = Ctx.t

let init_env = Env.init
                 
let add_coh_env = Env.add_coh 

let add_let_env v c u =
  Env.add_let v c u

let add_let_env_of_ty v c u t =
  Env.add_let_check v c u t
               
let mk_tm c e =
  let c = Ctx.make c in
  let e,t = Tm.make [] c e in
  (Tm.to_string e, Ty.to_string t)

let mk_ctx c =
  let c = Ctx.make c in Ctx.to_string c
    
let mk_tm_of_ty c e t = assert false
  (* let c = Ctx.make c in
   * let e,t' = Tm.make c e in
   * let t = Ty.make c t in
   * Ty.check_equal c t' t *)              

