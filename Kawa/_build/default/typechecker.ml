open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

(* On se fait un mini-environnement de classes, sans héritage. *)
let class_env = ref Env.empty

(* Pour chaque classe : on enregistre la liste de ses attributs *)
let rec get_class_attributes cname =
  match Env.find_opt cname !class_env with
  | Some cdef -> 
    let parent_attrs =
      match cdef.parent with
      | Some parent_name -> get_class_attributes parent_name  (* On remonte vers la classe parente *)
      | None -> []
    in
    cdef.attributes @ parent_attrs  (* On fusionne les attributs de la classe et ceux de la classe parente *)
  | None -> error ("Unknown class: " ^ cname)

let get_class_def cname =
  match Env.find_opt cname !class_env with
  | Some cdef -> cdef
  | None -> error ("Unknown class: " ^ cname)
let rec get_class_method cname mname =
  match Env.find_opt cname !class_env with
  | None ->
      error ("Unknown class: "^ cname)
  | Some cdef ->
      (* On tente d’abord de trouver la méthode dans la classe courante *)
      match List.find_opt (fun md -> md.method_name = mname) cdef.methods with
      | Some md -> md
      | None ->
        (* Si on ne la trouve pas, on remonte vers la super-classe si elle existe *)
        match cdef.parent with
        | Some parent_name -> get_class_method parent_name mname
        | None ->
          error (Printf.sprintf "Method '%s' not found in class '%s'" mname cname)

let rec is_subtype t1 t2 =
  (* Cas trivial : mêmes types *)
  if t1 = t2 then true
  else
    match t1, t2 with
    | TClass c1, TClass c2 ->
       (* On remonte la chaîne de parents de c1 pour voir s’il hérite de c2 *)
       let rec has_ancestor c =
         if c = c2 then true
         else
           match (Env.find c !class_env).parent with
           | Some parent_name -> has_ancestor parent_name
           | None -> false
       in
       has_ancestor c1
    | _, _ ->
       false

(* Vérification qu’une classe existe (sans héritage) *)
let check_class_exists cname =
  match Env.find_opt cname !class_env with
  | None -> error ("No such class: "^ cname)
  | Some _ -> ()

let get_class_method cname mname =
  match Env.find_opt cname !class_env with
  | None -> error ("Unknown class: "^ cname)
  | Some cdef ->
      try
        List.find (fun md -> md.method_name = mname) cdef.methods
      with Not_found ->
        error (Printf.sprintf "Method '%s' not found in class '%s'" mname cname)
let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let env_cl =
    List.fold_left
      (fun acc cdef -> Env.add cdef.class_name cdef acc)
      Env.empty
      p.classes
  in
  class_env := env_cl;


  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if not (is_subtype typ_e typ) then
      type_error typ_e typ

  and type_expr e tenv = match e with
    | Int  _  -> TInt
    | Bool  _  -> TBool
    | Binop (op, e1, e2) ->
      let t1 = type_expr e1 tenv in
      let t2 = type_expr e2 tenv in
      begin match op, t1, t2 with
      | (Add | Sub | Mul | Div | Rem), TInt, TInt -> TInt
      | (And | Or), TBool, TBool -> TBool
      | (Gt | Lt | Ge | Le ) , TInt, TInt -> TBool
      | Neq, t1, t2 -> if t1 = t2 then TBool else error "invalid operands for neq operator"
      | Eq, t1, t2 -> if t1 = t2 then TBool else error "invalid operands for eq operator" 
      | Neqs, t1, t2 -> if t1 = t2 then TBool else error "invalid operands for neqs operator"
      | Eqs, t1, t2 -> if t1 = t2 then TBool else error "invalid operands for eqs operator" 
      | _ -> error "invalid operands for binary operator"
      end
    | Unop (op, e) -> 
      let t = type_expr e tenv in
      begin match op, t with
      | Opp, TInt -> TInt 
      | Not, TBool -> TBool 
      | _ -> error "invalid operands for unary operator"
      end
    | Get mem -> type_mem_access mem tenv

    | New cname ->
      (* Vérifier que la classe existe *)
      check_class_exists cname;
      TClass cname
    
    | NewCstr (cname, args) ->
      (* Vérifie que la classe existe. *)
      check_class_exists cname;
      let cdef = get_class_def cname in
      (* On recherche la méthode 'constructor' dans la classe *)
      begin
        try
          let constructor_def =
            List.find (fun m -> m.method_name = "constructor") cdef.methods
          in
          (* Vérifie que le nombre d'arguments correspond au nombre de paramètres *)
          let n_params = List.length constructor_def.params in
          let n_args = List.length args in
          if n_params <> n_args then
            error (Printf.sprintf
                     "Wrong number of arguments in constructor call for class '%s'"
                     cname);
          (* On compare types attendus et types passés *)
          List.iter2 (fun (_, ptyp) arg_expr ->
              let targ = type_expr arg_expr tenv in
              if not (is_subtype targ ptyp) then
                type_error targ ptyp
            ) constructor_def.params args;
        with Not_found ->
          error (Printf.sprintf "No constructor found in class '%s'" cname)
      end;
      TClass cname
    
    | MethCall (obj_expr, mname, args) ->
      let tobj = type_expr obj_expr tenv in
      begin match tobj with
      | TClass cname ->
        (* On récupère la définition de la méthode dans la classe. *)
        let mdef = get_class_method cname mname in
        (* Vérifie le nombre d'arguments *)
        let n_params = List.length mdef.params in
        let n_args   = List.length args in
        if n_params <> n_args then
          error (Printf.sprintf
                   "Wrong number of arguments in method call '%s' of class '%s'"
                   mname cname);
        (* Vérifie la cohérence de type de chaque argument. *)
        List.iter2 (fun (_, ptyp) arg_expr ->
            let targ = type_expr arg_expr tenv in
            if targ <> ptyp then type_error targ ptyp
          ) mdef.params args;
        (* Le type de l'expression est le type de retour de la méthode. *)
        mdef.return
      | _ -> error "method call on non-class type"
      end
    

    | _ -> failwith "case not implemented in type_expr"

  and type_mem_access m tenv = 
    match m with
    | Var ident ->begin
      if not (Env.mem ident tenv) then 
        raise(error("Variable "^ident^" undeclared"))
      else Env.find ident tenv
      end
    | Field (obj_expr, fieldname) ->
      (* On type l’expression 'obj_expr' qui doit être de la forme TClass c. *)
      let tobj = type_expr obj_expr tenv in
      match tobj with
      | TClass cname ->
        (* Récupère la liste d’attributs de la classe cname *)
        let attrs = get_class_attributes cname in
        begin
          try
            let (_, attr_type) =
              List.find (fun (aname, _) -> aname = fieldname) attrs
            in
            attr_type
          with Not_found ->
            error (Printf.sprintf "Unknown field '%s' in class '%s'"
                    fieldname cname)
        end
    | _ ->
      error "field access on a non-object type"

  and check_instr i ret tenv = 
    match i with
    | Print e -> 
      let t =  type_expr e tenv in
      check e t tenv
    | Set (Var id, e) -> 
      let t_id = Env.find id tenv in
      check e t_id tenv
    | Set (Field (obj_expr, fieldname), e) ->
      let tobj = type_expr obj_expr tenv in
      (match tobj with
        | TClass cname ->
          (* Récupère le type de l’attribut 'fieldname' *)
          let attrs = get_class_attributes cname in
          let attr_ty =
            try snd (List.find (fun (a,_) -> a = fieldname) attrs)
            with Not_found ->
              error (Printf.sprintf "Unknown field '%s' in class '%s'"
                      fieldname cname)
          in
          let te = type_expr e tenv in
          if te <> attr_ty then type_error te attr_ty
        | _ -> error "set field on non-class type")
    | If (e, s1, s2) ->
      check e TBool tenv;
      check_seq s1 ret tenv;
      check_seq s2 ret tenv;
    | While (e, s1) -> 
      check e TBool tenv;
      check_seq s1 ret tenv;
    | Return e -> 
      check e ret tenv;
    | Expr e -> ignore (type_expr e tenv) 
    
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in

  check_seq p.main TVoid tenv
