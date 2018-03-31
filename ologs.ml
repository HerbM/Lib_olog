

(* Base definitions for ologs *)
type aspect =
  | Is
  | Symbolic of string

type application_type =
  | Ordinary
  | Injective
  | Surjective

type object_role =
  | Empty
  | Singleton
  | Product
  | Coproducts
(* | Pullbacks | Pushouts  To be completed *)

type object_ = {
  name : string;
  mutable aspects : (application_type * aspect * object_) list;
  role : object_role option;
}

type fact = (int list * int list)

type olog = {
  label : string;
  mutable objects : object_ list;
  facts : fact list;
}

type  'a table_obj = {
  obj : object_;
  mutable instances : 'a list;

}

let default_obj = {name = ""; aspects = []; role = None}
let default_olog = {label = ""; objects = []; facts = []}

let make_olog objects facts name = 
  {label = name; objects = objects; facts = facts}

let make_default_table () = {obj = default_obj; instances = []}

let make_obj_table ?(instances = []) obj = 
  {obj = obj; instances = instances}

let make_aspect ?(app_type = Ordinary) ?(aspect_type = Is) target =
  (app_type, aspect_type, target)

let (=>) name obj = {default_obj with name = name; aspects = [make_aspect obj]}

let is_ x = {default_obj with name = ""; aspects = [make_aspect x]}
let rel s target = {default_obj with name = ""; aspects = [make_aspect target ~aspect_type:(Symbolic s)]}

let (<$>) name object_ = {default_obj with name = name; aspects = object_.aspects}
let (<*>) obj1 obj2 = {default_obj with name = obj1.name; aspects = obj1.aspects @ obj2.aspects}

let add_target obj type_asp aspect target = obj.aspects <- obj.aspects@[type_asp,aspect,target];;

let add_instance_obj table value = table.instances <- List.append table.instances [value];;

(*let add_instance_aspects = ;;
let add_instances_of_facts = ;;*)

let a_person = {default_obj with name = "a person"; aspects = []};;
let table_a_person = make_obj_table a_person;;
add_instance_obj table_a_person "hakim";;
List.iter print_endline table_a_person.instances;;



exception Invalid_path

(* Checks that a path exists in a given object *)
let check_path obj path =
  try
    ignore (List.fold_left
      (fun aspects n ->
        match List.nth aspects n with
          | (_, _, obj) -> obj.aspects)
      obj.aspects
      path)
  with Failure _ -> raise Invalid_path

let make_commute obj path1 path2 =
  check_path obj path1;
  check_path obj path2;
  (path1, path2)

