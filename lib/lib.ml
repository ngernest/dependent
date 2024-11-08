open Printf

(** Expressions written in HOAS *)
type expr = 
  | Axiom of string * expr 
  | Lam of expr * (expr -> expr)
  | App of expr * expr 
  | Pi of expr * (expr -> expr)
  | U 

(** Counter for variables *)  
let var_counter : int ref = ref 0 

(** Produces a fresh variable *)
let gensym () : string = 
  let varname = sprintf "x%d" !var_counter in 
  var_counter := !var_counter + 1;
  varname

(** Pretty-prints an [expr] *)
let rec to_string (e : expr) : string = 
  match e with 
  | Axiom (s, e') -> sprintf "(%s : %s)\n" s (to_string e')
  | Lam (t, body) -> 
    (* We generate a fresh var [x] , then apply [body] (an [expr -> expr] function) 
      to an [Axiom] parameterized by [x] so that we can actually print it *)
    let x : string = gensym () in 
    let actual_body = body (Axiom (x, t)) in 
    sprintf "(fun %s : %s => %s)\n" x (to_string t) (to_string actual_body)
  | App (e1, e2) -> 
    sprintf "%s (%s)\n" (to_string e1) (to_string e2)
  | Pi (t, body) -> 
    (* We generate a fresh var [x] , then apply [body] 
      (an [expr -> expr] function) to an [Axiom] parameterized by [x] 
      so that we can actually print it *)
    let x : string = gensym () in 
    let actual_body = body (Axiom (x, t)) in 
    sprintf "(forall %s : %s, %s)\n" x (to_string t) (to_string actual_body)
  | U -> "U"

(** Computes the beta-normal form of an [expr] *)  
let rec nf (e : expr) : expr = 
  begin match e with 
  | U -> U 
  | Axiom (s, body) -> Axiom (s, nf body)
  | Lam (t, body) -> Lam (nf t, body)
  | Pi (t, body) -> Pi (nf t, body)
  | App (e1, e2) -> 
    begin match nf e1 with 
    | Lam (_, f) -> nf (f e2)
    | e1' -> App (e1', nf e2)
    end
  end
    
let rec typeof (e : expr) : expr = 
  match e with 
  | U | Axiom _ -> U 
  (* [t1 -> t2] is the same as [forall x:t1 -> t2[x |-> t1]] *)
  | Lam (t, body) -> 
    let x = gensym () in 
    Pi (t, fun t' -> typeof @@ body (Axiom (x, t')))
  | Pi (t, _body) -> 
    let t' = typeof t in 
    begin match t' with 
    | U -> failwith "TODO: continue!"
    | _ -> failwith @@ 
      sprintf "expected %s to have type U but got %s instead\n" 
      (to_string t) (to_string t')
    end
  | _ -> failwith "TODO"