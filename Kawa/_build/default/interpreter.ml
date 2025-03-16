open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | Null
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;

  let class_env = Hashtbl.create 16 in
  List.iter (fun cls -> Hashtbl.add class_env cls.class_name cls) p.classes;
  
(*------------------------------------------------------------------*)
  (* Récupère la liste de *tous* les attributs (classe + parents).    *)
  (* Si vous voulez gérer l’héritage, on va “remonter” la chaîne      *)
  (* des parents. Sinon on peut se contenter de la classe courante.   *)
  (*------------------------------------------------------------------*)
  let rec get_attributes cls_name =
    match Hashtbl.find_opt class_env cls_name with
    | Some cdef ->
      let parent_attrs =
        match cdef.parent with
        | Some parent_name -> get_attributes parent_name  (* Appel récursif vers la super-classe *)
        | None -> []
      in
      cdef.attributes @ parent_attrs  (* On concatène les attributs de la classe et de la super-classe *)
    | None -> raise (Error ("Unknown class: " ^ cls_name))
  

 
  and exec_seq s lenv =
    let rec evalb e = match eval e with
      | VBool b -> b
      | _ -> assert false
    
    and eval (e: expr): value = match e with
      | Int n  -> VInt n
      | Bool b  -> VBool b
      | Get (Var id) ->
        if Hashtbl.mem lenv id then
          Hashtbl.find lenv id
        else if Hashtbl.mem env id then
          Hashtbl.find env id
        else failwith ("Variable undefined : " ^ id)

      | Get (Field (o, fieldname)) ->
        let vo = eval o  in
        begin match vo with
          | VObj obj ->
            if Hashtbl.mem obj.fields fieldname then
              Hashtbl.find obj.fields fieldname
            else
              raise (Error (Printf.sprintf "Unknown field '%s' in class '%s'"
                              fieldname obj.cls))
          | _ -> raise (Error "Cannot access field on non-object")
        end 
      
      
      | Binop (op, e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        begin match op, v1, v2 with
        | Add, VInt n1, VInt n2 -> VInt (n1 + n2)
        | Sub, VInt n1, VInt n2 -> VInt (n1 - n2)
        | Mul, VInt n1, VInt n2 -> VInt (n1 * n2)
        | Div, VInt n1, VInt n2 -> VInt (n1 / n2)
        | Rem , VInt n1, VInt n2 -> VInt (n1 mod n2)
        | And, VBool b1, VBool b2 -> VBool (b1 && b2)
        | Or,  VBool b1, VBool b2 -> VBool (b1 || b2)

        | Eq, VInt t1, VInt t2 -> VBool (t1 == t2)
        | Eq, VBool t1, VBool t2 -> VBool (t1 == t2)
        | Eq, VObj t1, VObj t2 -> VBool (t1 == t2)
        | Eq, Null, Null -> VBool (true)
        
        | Neq, VInt t1, VInt t2 -> VBool (t1 != t2)
        | Neq, VBool t1, VBool t2 -> VBool (t1 != t2)
        | Neq, VObj t1, VObj t2 -> VBool (t1 != t2)
        | Neq, Null, Null -> VBool (false)

        | Eqs, VInt t1, VInt t2 -> VBool (t1 = t2)
        | Eqs, VBool t1, VBool t2 -> VBool (t1 = t2)
        | Eqs, VObj t1, VObj t2 -> VBool (t1 = t2)
        | Eqs, Null, Null -> VBool (true)
        
        | Neqs, VInt t1, VInt t2 -> VBool (t1 <> t2)
        | Neqs, VBool t1, VBool t2 -> VBool (t1 <> t2)
        | Neqs, VObj t1, VObj t2 -> VBool (t1 <> t2)
        | Neqs, Null, Null -> VBool (false)

        | _ -> raise (Error "invalid operands for binary operator1")
        end
        
      | Unop (op, e) -> 
        let v = eval e in
        begin match op, v with
        | Opp, VInt n -> VInt (0 - n)
        | Not, VBool b -> VBool (not b)
        | _ -> raise (Error "invalid operands for unary operator")
        end
      | New class_name ->
        (* Création d’un objet de classe cls_name, attributs = Null *)
        let attrs = get_attributes class_name in
        let table = Hashtbl.create 16 in
        List.iter (fun (attr_name, _ty) ->
          Hashtbl.add table attr_name Null
        ) attrs;
        VObj { cls = class_name; fields = table }

      | NewCstr (class_name, args) ->
        (* On recupere les attributs *)
        let all_attrs = get_attributes class_name in 

        (* On crée l'objet *)
        let cdef = 
          match List.find_opt (fun c -> c.class_name = class_name) p.classes with 
          | None -> raise (Error ("Class not found : " ^ class_name)) (* cas devrait être impossible *)
          | Some cd -> cd 
        in 
        let fields = Hashtbl.create 16 in 
        List.iter (fun (attr_name, _) -> Hashtbl.add fields attr_name Null) all_attrs;
        let instance = { cls = class_name; fields } in
        let vobj = VObj instance in 

        (* On appele la méthode constructeur sur cet objet crée *)
        begin
          match List.find_opt (fun m -> m.method_name = "constructor") cdef.methods with
          | Some constructor_def ->
              let arg_values = List.map eval args in
              let local_env = Hashtbl.create 16 in
              List.iter2 (fun (pname, _) arg_val ->
                Hashtbl.add local_env pname arg_val
              ) constructor_def.params arg_values;
              Hashtbl.add local_env "this" vobj;
    
              (try
                  exec_seq constructor_def.code local_env
                with Return _ ->
                  (* Si quelqu'un fait un 'return' depuis un constructor, on l'ignore ou on rend la valeur,
                    mais en principe c'est un 'void' => on ignore. *)
                  ());
              vobj
          | None ->
              (* Devrait ne jamais arriver si le typechecker l'a autorisé. *)
              vobj
        end

      | MethCall (obj, id, args) -> 
        let vobj = eval obj in
        begin match vobj with
        | VInt a -> raise (Error "method call on Int")
        | VBool a -> raise (Error "method call on Bool")
        | Null-> raise (Error "method call on Null")
        | VObj instance ->
          let find_class class_name =
            match List.find_opt (fun c -> c.class_name = class_name) p.classes with
            | Some class_def -> class_def
            | None -> raise (Error ("Class not found : " ^ class_name))
          in
          (* On recupére la définition de la méthode *)
          let method_def =
            List.find (fun m -> m.method_name = id) (find_class instance.cls).methods
          in
          (* On évalue les arguments *)
          let arg_values = List.map eval args in
          (* On créer un environnement local pour executer la méthode *)
          let local_env = Hashtbl.create 16 in
            List.iter2 (fun (param_name, _) arg_value ->
              Hashtbl.add local_env param_name arg_value
            ) method_def.params arg_values;
            Hashtbl.add local_env "this" vobj;
          (* Ajouter l'objet courant "this" *)
          Hashtbl.add local_env "this" (VObj instance);
          (* On éxecute le corps de la méthode *)
          try
            exec_seq method_def.code local_env;
            Null (* Si c'est un void *)
          with Return v -> v 
        | _ -> raise (Error "method call on non-object")
        end


      | This -> begin 
        try Hashtbl.find lenv "this"
        with
        |Not_found -> raise(Error ("'this' undefined"))
        end
    in
  

    
    let rec exec (i: instr) : unit = 
      match i with
      | Print e -> 
        let v = eval e in
        begin match v with
        | VInt n -> Printf.printf "%d\n" n
        | VBool b -> Printf.printf "%s\n" (if b then "true" else "false")
        | _ -> raise (Error "cannot print non-integer or non-boolean value")
        end
      | Set (Var id, e) ->
        let v = eval e in
        if Hashtbl.mem lenv id then
          Hashtbl.replace lenv id v
        else if Hashtbl.mem env id then
          Hashtbl.replace env id v
        else
          raise (Error (Printf.sprintf "Variable '%s' not found" id))
      | Set (Field (o, fieldname), e) ->
        (* Évaluer l’objet o *)
        let vo = eval o in
        let vnew = eval e in
        begin match vo with
        | VObj obj ->
          if Hashtbl.mem obj.fields fieldname then
            Hashtbl.replace obj.fields fieldname vnew
          else
            raise (Error (Printf.sprintf "Unknown field '%s' in class '%s'"
                            fieldname obj.cls))
        | _ -> raise (Error "Cannot set field on non-object")
        end
      | If (e, s1, s2) ->
        let v = evalb e in
        if v then exec_seq s1 else exec_seq s2
      | While (e, s1) ->  
        while evalb e do exec_seq s1 done
      | Return e -> 
        let v = eval e in
        raise (Return v)
      | Expr e -> ignore (eval e)
    and exec_seq s = 
      List.iter exec s
    in

    exec_seq s
  in
  
  exec_seq p.main (Hashtbl.create 1)
