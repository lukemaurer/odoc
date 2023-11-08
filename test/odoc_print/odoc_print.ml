(** Print .odocl files. *)

open Odoc_odoc
open Odoc_odoc.Or_error
open Odoc_model_desc

let print_json_desc desc x =
  let yojson = Type_desc_to_yojson.to_yojson desc x in
  Yojson.Basic.pretty_print Format.std_formatter yojson

module Element = struct
  open Odoc_model.Lang

  type t =
    | Module of Module.t
    | ModuleType of ModuleType.t
    | Type of TypeDecl.t
    | ClassType of ClassType.t
    | Class of Class.t
    | Value of Value.t
end

let rec signature_of_simple_expansion :
    Odoc_model.Lang.ModuleType.simple_expansion -> Odoc_model.Lang.Signature.t =
  function
  | Signature sg -> sg
  | Functor (_, e) -> signature_of_simple_expansion e

and signature_of_module_type_expr = function
  | Odoc_model.Lang.ModuleType.Signature sg -> Some sg
  | Path { p_expansion = Some exp; _ } ->
      Some (signature_of_simple_expansion exp)
  | Path { p_expansion = None; _ } -> None
  | Functor (_, e) -> signature_of_module_type_expr e
  | TypeOf { t_expansion = Some e; _ } -> Some (signature_of_simple_expansion e)
  | TypeOf _ -> None
  | With { w_expansion = Some e; _ } -> Some (signature_of_simple_expansion e)
  | With _ -> None

and signature_of_module :
    Odoc_model.Lang.Module.t -> Odoc_model.Lang.Signature.t option =
 fun m ->
  match m.type_ with
  | Alias (_, Some e) -> Some (signature_of_simple_expansion e)
  | Alias (_, None) -> None
  | ModuleType m -> signature_of_module_type_expr m

and signature_of_module_type :
    Odoc_model.Lang.ModuleType.t -> Odoc_model.Lang.Signature.t option =
 fun m ->
  match m.expr with Some e -> signature_of_module_type_expr e | None -> None

let find_map fn list =
  let rec inner = function
    | x :: xs -> ( match fn x with Some y -> Some y | None -> inner xs)
    | [] -> None
  in
  inner list

let find_module name sg =
  let open Odoc_model.Lang.Signature in
  find_map
    (function
      | Module (_, ({ id; _ } as m))
        when Odoc_model.Paths.Identifier.name id = name ->
          Some (Element.Module m)
      | _ -> None)
    sg.items

let find_module_type name sg =
  let open Odoc_model.Lang.Signature in
  find_map
    (function
      | ModuleType ({ id; _ } as m)
        when Odoc_model.Paths.Identifier.name id = name ->
          Some (Element.ModuleType m)
      | _ -> None)
    sg.items

let find_type name sg =
  let open Odoc_model.Lang.Signature in
  find_map
    (function
      | Type (_, ({ id; _ } as m))
        when Odoc_model.Paths.Identifier.name id = name ->
          Some (Element.Type m)
      | ClassType (_, ({ id; _ } as m))
        when Odoc_model.Paths.Identifier.name id = name ->
          Some (Element.ClassType m)
      | Class (_, ({ id; _ } as m))
        when Odoc_model.Paths.Identifier.name id = name ->
          Some (Element.Class m)
      | _ -> None)
    sg.items

let find_value name sg =
  let open Odoc_model.Lang.Signature in
  find_map
    (function
      | Value ({ id; _ } as m) when Odoc_model.Paths.Identifier.name id = name
        ->
          Some (Element.Value m)
      | _ -> None)
    sg.items

(* Really cut-down reference lookup! *)
let rec handle_ref :
    Odoc_model.Lang.Signature.t ->
    Odoc_model.Paths.Reference.t ->
    Element.t option =
 fun sg r ->
  let ( >>= ) m f = match m with Some x -> f x | None -> None in
  let ( ||> ) f1 f2 x = match f1 x with Some x -> Some x | None -> f2 x in
  let signature_of_element : Element.t -> Odoc_model.Lang.Signature.t option =
   fun e ->
    match e with
    | Element.Module m -> signature_of_module m
    | Element.ModuleType mt -> signature_of_module_type mt
    | _ -> None
  in
  match r with
  | `Root (n, `TUnknown) ->
      let find =
        find_module n ||> find_module_type n ||> find_type n ||> find_value n
      in
      find sg
      (* Assume this is a module *)
  | `Root (name, `TModule) -> find_module name sg
  | `Root (name, `TModuleType) -> find_module_type name sg
  | `Root (name, `TType) -> find_type name sg
  | `Dot (parent, n) ->
      let find =
        find_module n ||> find_module_type n ||> find_type n ||> find_value n
      in
      handle_ref sg (parent :> Odoc_model.Paths.Reference.t)
      >>= signature_of_element >>= find
  | `Module (parent, m) ->
      handle_ref sg (parent :> Odoc_model.Paths.Reference.t)
      >>= signature_of_element
      >>= find_module (Odoc_model.Names.ModuleName.to_string m)
  | `ModuleType (parent, m) ->
      handle_ref sg (parent :> Odoc_model.Paths.Reference.t)
      >>= signature_of_element
      >>= find_module (Odoc_model.Names.ModuleTypeName.to_string m)
  | `Type (parent, m) ->
      handle_ref sg (parent :> Odoc_model.Paths.Reference.t)
      >>= signature_of_element
      >>= find_module (Odoc_model.Names.TypeName.to_string m)
  | `Value (parent, m) ->
      handle_ref sg (parent :> Odoc_model.Paths.Reference.t)
      >>= signature_of_element
      >>= find_module (Odoc_model.Names.ValueName.to_string m)
  | _ ->
      Format.eprintf "Reference unhandled\n%!";
      None

let print_element elt =
  match elt with
  | Element.Module m -> print_json_desc Lang_desc.module_t m
  | Element.ModuleType m -> print_json_desc Lang_desc.moduletype_t m
  | Element.Type t -> print_json_desc Lang_desc.typedecl_t t
  | Element.Value v -> print_json_desc Lang_desc.value_t v
  | Element.ClassType v -> print_json_desc Lang_desc.classtype_t v
  | Element.Class v -> print_json_desc Lang_desc.class_t v

module Print_short = struct
  open Odoc_model.Lang

  type id = Odoc_model.Paths.Identifier.t
  type path = Odoc_model.Paths.Path.t
  type rpath = Odoc_model.Paths.Path.Resolved.t
  type frag = Odoc_model.Paths.Fragment.t
  type rfrag = Odoc_model.Paths.Fragment.Resolved.t

  let fpf = Format.fprintf

  let fpp_opt fmt pp_a ppf = function
    | Some t -> fpf ppf fmt pp_a t
    | None -> ()

  let fpp_list fmt_sep fmt_outer pp_a ppf t =
    let pp_sep ppf () = fpf ppf fmt_sep in
    match t with
    | [] -> ()
    | t -> fpf ppf fmt_outer (Format.pp_print_list ~pp_sep pp_a) t

  let rec identifier ppf (p : id) =
    Format.fprintf ppf "%s" (Odoc_model.Paths.Identifier.name p)

  and path : Format.formatter -> path -> unit =
   fun ppf (p : path) ->
    match p with
    | `Resolved rp -> Format.fprintf ppf "%a" resolved_path rp
    | `Identifier (id, _) -> Format.fprintf ppf "%a" identifier (id :> id)
    | `Root s -> Format.fprintf ppf "%s" s
    | `Forward s -> Format.fprintf ppf "forward(%s)" s
    | `Dot (parent, s) -> Format.fprintf ppf "%a.%s" path (parent :> path) s
    | `Apply (func, arg) ->
        Format.fprintf ppf "%a(%a)" path (func :> path) path (arg :> path)

  and resolved_path ppf (p : rpath) =
    match p with
    | `Identifier id -> identifier ppf (id :> id)
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s" resolved_path
          (parent :> rpath)
          (Odoc_model.Names.ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s" resolved_path
          (parent :> rpath)
          (Odoc_model.Names.ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s" resolved_path
          (parent :> rpath)
          (Odoc_model.Names.TypeName.to_string name)
    | `Alias (_dest, src) -> path ppf (src :> path)
    | `AliasModuleType (_path, realpath) ->
        resolved_path ppf (realpath :> rpath)
    | `Subst (_modty, m) -> resolved_path ppf (m :> rpath)
    | `SubstT (_t1, t2) -> resolved_path ppf (t2 :> rpath)
    | `CanonicalModuleType (_t1, t2) -> path ppf (t2 :> path)
    | `CanonicalType (_t1, t2) -> path ppf (t2 :> path)
    | `Apply (funct, arg) ->
        Format.fprintf ppf "%a(%a)" resolved_path
          (funct :> rpath)
          resolved_path
          (arg :> rpath)
    | `Canonical (_p1, p2) -> path ppf (p2 :> path)
    | `Hidden p -> resolved_path ppf (p :> rpath)
    | `Class (parent, name) ->
        Format.fprintf ppf "%a.%s" resolved_path
          (parent :> rpath)
          (Odoc_model.Names.ClassName.to_string name)
    | `ClassType (parent, name) ->
        Format.fprintf ppf "%a.%s" resolved_path
          (parent :> rpath)
          (Odoc_model.Names.ClassTypeName.to_string name)
    | `OpaqueModule m -> resolved_path ppf (m :> rpath)
    | `OpaqueModuleType m -> resolved_path ppf (m :> rpath)
    | `CanonicalDataType (_t1, t2) -> path ppf (t2 :> path)
    | `Constructor (_parent, name) ->
        Format.fprintf ppf "%s"
          (Odoc_model.Names.ConstructorName.to_string name)
    | `Value (_parent, name) ->
        Format.fprintf ppf "%s" (Odoc_model.Names.ValueName.to_string name)

  and fragment ppf (f : frag) =
    match f with
    | `Resolved r -> Format.fprintf ppf "%a" resolved_fragment r
    | `Dot (`Root, n) -> Format.fprintf ppf "%s" n
    | `Dot (s, n) -> Format.fprintf ppf "%a.%s" fragment (s :> frag) n
    | `Root -> Format.fprintf ppf "rooooot"

  and resolved_fragment ppf (f : rfrag) =
    match f with
    | `Subst (s, f) ->
        Format.fprintf ppf "subst(%a,%a)" resolved_path
          (s :> rpath)
          resolved_fragment
          (f :> rfrag)
    | `Alias (m, f) ->
        Format.fprintf ppf "substalias(%a,%a)" resolved_path
          (m :> rpath)
          resolved_fragment
          (f :> rfrag)
    | `Module (`Root _, n) ->
        Format.fprintf ppf "%s" (Odoc_model.Names.ModuleName.to_string n)
    | `Module (p, n) ->
        Format.fprintf ppf "%a.%s" resolved_fragment
          (p :> rfrag)
          (Odoc_model.Names.ModuleName.to_string n)
    | `OpaqueModule m ->
        Format.fprintf ppf "opaquemodule(%a)" resolved_fragment (m :> rfrag)
    | `Module_type (p, n) ->
        Format.fprintf ppf "%a.%s" resolved_fragment
          (p :> rfrag)
          (Odoc_model.Names.ModuleTypeName.to_string n)
    | `Type (p, n) ->
        Format.fprintf ppf "%a.%s" resolved_fragment
          (p :> rfrag)
          (Odoc_model.Names.TypeName.to_string n)
    | `Class (p, n) ->
        Format.fprintf ppf "%a.%s" resolved_fragment
          (p :> rfrag)
          (Odoc_model.Names.ClassName.to_string n)
    | `ClassType (p, n) ->
        Format.fprintf ppf "%a.%s" resolved_fragment
          (p :> rfrag)
          (Odoc_model.Names.ClassTypeName.to_string n)
    | `Root _ -> Format.fprintf ppf "rot"

  let rec module_decl ppf d =
    let open Module in
    match d with
    | Alias (p, _) -> Format.fprintf ppf "= %a" path (p :> path)
    | ModuleType mt -> Format.fprintf ppf ": %a" module_type_expr mt

  and module_ ppf m =
    let open Module in
    module_decl ppf m.type_

  and simple_expansion ppf (m : ModuleType.simple_expansion) =
    match m with
    | ModuleType.Signature sg ->
        Format.fprintf ppf ": sig@,@[<v 2>%a@]@,end" signature sg
    | Functor (arg, sg) ->
        Format.fprintf ppf "functor: (%a) -> %a" functor_parameter arg
          simple_expansion sg

  and module_type_expr ppf mt =
    let open ModuleType in
    match mt with
    | Path { p_path; p_expansion = Some sg; _ } ->
        Format.fprintf ppf "%a %a" path (p_path :> path) simple_expansion sg
    | Path { p_path; p_expansion = None; _ } -> path ppf (p_path :> path)
    | Signature sg -> Format.fprintf ppf "sig@,@[<v 2>%a@]@,end" signature sg
    | With { w_substitutions = subs; w_expr; w_expansion = Some sg } ->
        Format.fprintf ppf "%a with %a %a" u_module_type_expr w_expr
          substitution_list subs simple_expansion sg
    | With { w_substitutions = subs; w_expr; _ } ->
        Format.fprintf ppf "%a with %a" u_module_type_expr w_expr
          substitution_list subs
    | Functor (arg, res) ->
        Format.fprintf ppf "(%a) -> %a" functor_parameter arg module_type_expr
          res
    | TypeOf { t_desc = ModPath p; t_expansion = Some sg; _ } ->
        Format.fprintf ppf "module type of %a %a" path
          (p :> path)
          simple_expansion sg
    | TypeOf { t_desc = StructInclude p; t_expansion = Some sg; _ } ->
        Format.fprintf ppf "module type of struct include %a end %a" path
          (p :> path)
          simple_expansion sg
    | TypeOf { t_desc = ModPath p; _ } ->
        Format.fprintf ppf "module type of %a" path (p :> path)
    | TypeOf { t_desc = StructInclude p; _ } ->
        Format.fprintf ppf "module type of struct include %a end" path
          (p :> path)

  and u_module_type_expr ppf mt =
    let open ModuleType.U in
    match mt with
    | Path p -> path ppf (p :> path)
    | Signature sg -> Format.fprintf ppf "sig@,@[<v 2>%a@]end" signature sg
    | With (subs, e) ->
        Format.fprintf ppf "%a with [%a]" u_module_type_expr e substitution_list
          subs
    | TypeOf (t_desc, _t_original_path) -> module_type_type_of_desc ppf t_desc

  and module_type_type_of_desc ppf t =
    match t with
    | ModuleType.ModPath p ->
        Format.fprintf ppf "module type of %a" path (p :> path)
    | StructInclude p ->
        Format.fprintf ppf "module type of struct include %a end" path
          (p :> path)

  and exception_ _ppf _e = ()

  and extension ppf e =
    Format.fprintf ppf "%a" path (e.Extension.type_path :> path)

  and substitution ppf t =
    let open ModuleType in
    match t with
    | ModuleEq (frag, decl) ->
        Format.fprintf ppf "module %a = %a" fragment
          (frag :> frag)
          module_decl decl
    | ModuleSubst (frag, mpath) ->
        Format.fprintf ppf "module %a := %a" fragment
          (frag :> frag)
          path
          (mpath :> path)
    | ModuleTypeEq (frag, mty) ->
        Format.fprintf ppf "module type %a = %a" fragment
          (frag :> frag)
          module_type_expr mty
    | ModuleTypeSubst (frag, mty) ->
        Format.fprintf ppf "module type %a := %a" fragment
          (frag :> frag)
          module_type_expr mty
    | TypeEq (frag, decl) ->
        Format.fprintf ppf "type %a%a" fragment
          (frag :> frag)
          type_equation decl
    | TypeSubst (frag, decl) ->
        Format.fprintf ppf "type %a%a" fragment
          (frag :> frag)
          type_equation decl

  and substitution_list ppf l =
    match l with
    | [ sub ] -> Format.fprintf ppf "%a" substitution sub
    | sub :: subs ->
        Format.fprintf ppf "%a %a" substitution sub substitution_list subs
    | [] -> ()

  and type_equation ppf t =
    match t.TypeDecl.Equation.manifest with
    | None -> ()
    | Some m -> Format.fprintf ppf " = %a" type_expr m

  and functor_parameter ppf x =
    let open FunctorParameter in
    match x with
    | Unit -> ()
    | Named x -> Format.fprintf ppf "%a" functor_parameter_parameter x

  and type_decl ppf t =
    let open TypeDecl in
    match t.representation with
    | Some repr ->
        Format.fprintf ppf "%a = %a"
          (fpp_opt " : %a" type_expr)
          t.equation.Equation.manifest type_decl_repr repr
    | None -> (fpp_opt " = %a" type_expr) ppf t.equation.Equation.manifest

  and type_decl_repr ppf =
    let open TypeDecl.Representation in
    function
    | Variant cs -> fpp_list " | " "%a" type_decl_constructor ppf cs
    | Record fs -> type_decl_fields ppf fs
    | Extensible -> Format.fprintf ppf ".."

  and type_decl_constructor ppf t =
    let open TypeDecl.Constructor in
    match t.res with
    | Some res ->
        fpf ppf "%a : %a -> %a" identifier
          (t.id :> id)
          type_decl_constructor_arg t.args type_expr res
    | None ->
        fpf ppf "%a of %a" identifier
          (t.id :> id)
          type_decl_constructor_arg t.args

  and type_decl_constructor_arg ppf =
    let open TypeDecl.Constructor in
    function
    | Tuple ts -> type_tuple ppf ts | Record fs -> type_decl_fields ppf fs

  and type_decl_field ppf t =
    let open TypeDecl.Field in
    let mutable_ = if t.mutable_ then "mutable " else "" in
    fpf ppf "%s%a : %a" mutable_ identifier (t.id :> id) type_expr t.type_

  and type_decl_fields ppf fs = fpp_list "; " "{ %a }" type_decl_field ppf fs

  and type_tuple ppf ts = fpp_list " * " "%a" type_expr ppf ts

  (* and type_param ppf t =
       let desc =
         match t.Odoc_model.Lang.TypeDecl.desc with Any -> "_" | Var n -> n
       and variance =
         match t.variance with Some Pos -> "+" | Some Neg -> "-" | None -> ""
       and injectivity = if t.injectivity then "!" else "" in
       Format.fprintf ppf "%s%s%s" variance injectivity desc

     and type_params ppf ts =
       let pp_sep ppf () = Format.fprintf ppf ", " in
       Format.fprintf ppf "(%a)" (Format.pp_print_list ~pp_sep type_param) ts
  *)
  and functor_parameter_parameter ppf x =
    Format.fprintf ppf "%a : %a" identifier
      (x.FunctorParameter.id :> id)
      module_type_expr x.FunctorParameter.expr

  and type_expr_label ppf l =
    match l with
    | Some (Odoc_model.Lang.TypeExpr.Label l) -> Format.fprintf ppf "%s:" l
    | Some (Optional o) -> Format.fprintf ppf "?%s:" o
    | None -> ()

  and type_expr_list ppf l =
    match l with
    | [ t ] -> Format.fprintf ppf "%a" type_expr t
    | t :: ts -> Format.fprintf ppf "%a * %a" type_expr t type_expr_list ts
    | [] -> ()

  and type_object ppf _o = Format.fprintf ppf "(object)"

  and type_class ppf (x, ys) =
    Format.fprintf ppf "(class %a %a)" path x type_expr_list ys

  and type_package ppf _p = Format.fprintf ppf "(package)"

  and type_expr_polymorphic_variant ppf p =
    let open TypeExpr.Polymorphic_variant in
    let pp_element ppf = function
      | Type t -> type_expr ppf t
      | Constructor c ->
          Format.fprintf ppf "`%s%a" c.Constructor.name
            (fpp_list " * " " of %a" type_expr)
            c.arguments
    in
    let pp_elements = fpp_list " | " "%a" pp_element in
    match p.kind with
    | Fixed -> fpf ppf "[ %a ]" pp_elements p.elements
    | Closed xs ->
        fpf ppf "[ %a > %a ]" pp_elements p.elements
          (fpp_list " " "%a" Format.pp_print_string)
          xs
    | Open -> fpf ppf "[> %a ]" pp_elements p.elements

  and type_expr ppf e =
    let open TypeExpr in
    match e with
    | Var x -> Format.fprintf ppf "%s" x
    | Any -> Format.fprintf ppf "_"
    | Alias (x, y) -> Format.fprintf ppf "(alias %a %s)" type_expr x y
    | Arrow (l, t1, t2) ->
        Format.fprintf ppf "%a(%a) -> %a" type_expr_label l type_expr t1
          type_expr t2
    | Tuple ts -> Format.fprintf ppf "(%a)" type_expr_list ts
    | Constr (p, args) -> (
        match args with
        | [] -> Format.fprintf ppf "%a" path (p :> path)
        | _ -> Format.fprintf ppf "[%a] %a" type_expr_list args path (p :> path)
        )
    | Polymorphic_variant poly ->
        Format.fprintf ppf "(poly_var %a)" type_expr_polymorphic_variant poly
    | Object x -> type_object ppf x
    | Class (x, y) -> type_class ppf ((x :> path), y)
    | Poly (_ss, _t) -> Format.fprintf ppf "(poly)"
    | Package x -> type_package ppf x

  and module_type ppf mt =
    let open ModuleType in
    match mt.expr with
    | Some x -> Format.fprintf ppf "= %a" module_type_expr x
    | None -> ()

  and value ppf v =
    let open Value in
    Format.fprintf ppf ": %a" type_expr v.type_

  and class_decl ppf _c = fpf ppf "todo"

  and class_ ppf c =
    let open Class in
    Format.fprintf ppf "%a" class_decl c.type_

  and class_type ppf _c = Format.fprintf ppf "<todo>"

  and include_decl ppf =
    let open Include in
    function
    | Alias p -> Format.fprintf ppf "= %a" path (p :> path)
    | ModuleType mt -> Format.fprintf ppf ": %a" u_module_type_expr mt

  and _content_option ppf = function
    | None -> ()
    | Some content -> Format.fprintf ppf " (sig=%a)" signature content

  and include_ ppf i =
    let open Include in
    Format.fprintf ppf "%a (sig = %a)" include_decl i.decl signature
      i.expansion.content

  and signature ppf sg =
    let open Signature in
    Format.fprintf ppf "@[<v>";
    List.iter
      (function
        | Module (_, m) ->
            Format.fprintf ppf "@[<v 2>module %a %a@]@," identifier
              (m.id :> id)
              module_ m
        | ModuleSubstitution m ->
            Format.fprintf ppf "@[<v 2>module %a := %a@]@," identifier
              (m.id :> id)
              path
              (m.manifest :> path)
        | ModuleType mt ->
            Format.fprintf ppf "@[<v 2>module type %a %a@]@," identifier
              (mt.id :> id)
              module_type mt
        | ModuleTypeSubstitution mts ->
            Format.fprintf ppf "@[<v 2>module type %a := %a@]@," identifier
              (mts.id :> id)
              module_type_expr mts.manifest
        | Type (_, t) ->
            Format.fprintf ppf "@[<v 2>type %a%a@]@," identifier
              (t.id :> id)
              type_decl t
        | TypeSubstitution t ->
            Format.fprintf ppf "@[<v 2>type %a :=%a@]@," identifier
              (t.id :> id)
              type_decl t
        | Exception e ->
            Format.fprintf ppf "@[<v 2>exception %a %a@]@," identifier
              (e.id :> id)
              exception_ e
        | TypExt e ->
            Format.fprintf ppf "@[<v 2>type_extension %a@]@," extension e
        | Value v ->
            Format.fprintf ppf "@[<v 2>val %a %a@]@," identifier
              (v.id :> id)
              value v
        | Class (_, c) ->
            Format.fprintf ppf "@[<v 2>class %a %a@]@," identifier
              (c.id :> id)
              class_ c
        | ClassType (_, c) ->
            Format.fprintf ppf "@[<v 2>class type %a %a@]@," identifier
              (c.id :> id)
              class_type c
        | Include i -> Format.fprintf ppf "@[<v 2>include %a@]@," include_ i
        | Open o -> Format.fprintf ppf "open [ %a ]" signature o.expansion
        | Comment _c -> ())
      sg.items

  and compilation_unit ppf u =
    match u.Compilation_unit.content with
    | Module m -> signature ppf m
    | Pack _ -> ()
end

let print_short elt =
  let open Print_short in
  match elt with
  | Element.Module m ->
      Format.fprintf Format.std_formatter "@[<v 2>module %a %a@]" identifier
        (m.id :> id)
        module_ m
  | Element.ModuleType m ->
      Format.fprintf Format.std_formatter "@[<v 2>module type %a %a@]"
        identifier
        (m.id :> id)
        module_type m
  | Element.Type t ->
      Format.fprintf Format.std_formatter "@[<v 2>type %a %a@]" identifier
        (t.id :> id)
        type_decl t
  | Element.Value v ->
      Format.fprintf Format.std_formatter "@[<v 2>val %a %a@]" identifier
        (v.id :> id)
        value v
  | Element.ClassType v -> Print_short.class_type Format.std_formatter v
  | Element.Class v -> Print_short.class_ Format.std_formatter v

let run inp short ref =
  let inp = Fpath.v inp in
  Odoc_file.load inp >>= fun unit ->
  match unit.content with
  | Odoc_file.Source_tree_content tree ->
      print_json_desc Lang_desc.source_tree_page_t tree;
      Ok ()
  | Odoc_file.Page_content page ->
      print_json_desc Lang_desc.page_t page;
      Ok ()
  | Unit_content u -> (
      match ref with
      | None ->
          if short then Format.printf "%a\n%!" Print_short.compilation_unit u
          else print_json_desc Lang_desc.compilation_unit_t u;
          Ok ()
      | Some r -> (
          let r = Odoc_model.Semantics.parse_reference r in
          let sg =
            match u.content with
            | Module m -> m
            | Pack _ -> failwith "Can't look up in packed modules"
          in
          match Odoc_model.Error.raise_warnings r with
          | Ok r -> (
              match handle_ref sg r with
              | Some elt ->
                  if short then print_short elt else print_element elt;
                  Ok ()
              | None -> Ok ())
          | _ -> Ok ()))

open Compatcmdliner

let reference =
  let doc = "reference to print" in
  Arg.(value & opt (some string) None & info ~doc [ "r" ])

let a_inp =
  let doc = "Input file." in
  Arg.(required & pos 0 (some file) None & info ~doc ~docv:"PATH" [])

let a_short =
  let doc = "Short output." in
  Arg.(value & flag & info ~doc [ "short" ])

let term =
  let doc = "Print the content of .odoc files into a text format. For tests" in
  Term.(const run $ a_inp $ a_short $ reference, info "odoc_print" ~doc)

let () =
  match Term.eval term with
  | `Ok (Ok ()) -> ()
  | `Ok (Error (`Msg msg)) ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | (`Version | `Help | `Error _) as x -> Term.exit x
