(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 *               2011 Mauricio Fernandez
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

let rnd = Random.State.make [|0x513511d4|]
let random_var () =
  Format.sprintf "a%08Lx" (Random.State.int64 rnd 0x100000000L)

open Camlp4

module Id : Sig.Id = struct
  let name = "Javascript"
  let version = "1.1"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  let rec filter stream =
    match stream with parser
      [< '(KEYWORD "#", loc); rest >] ->
        begin match rest with parser
          [< '(KEYWORD "#", loc') >] ->
             [< '(KEYWORD "##", Loc.merge loc loc'); filter rest >]
        | [< >] ->
             [< '(KEYWORD "#", loc); filter rest >]
        end
    | [< 'other; rest >] -> [< 'other; filter rest >]

  let _ =
    Token.Filter.define_filter (Gram.get_filter ())
      (fun old_filter stream -> old_filter (filter stream))

  let rec parse_comma_list e =
    match e with
      <:expr< $e1$, $e2$ >> -> e1 :: parse_comma_list e2
    | _                    -> [e]

  let rec to_sem_expr _loc l =
    match l with
      []        -> assert false
    | [e]       -> e
    | e1 :: rem -> <:expr< $e1$; $to_sem_expr _loc rem$ >>

  let make_array _loc l =
    match l with
      [] -> <:expr< [| |] >>
    | _  -> <:expr< [| $to_sem_expr _loc l$ |] >>

  let with_type e t =
    let _loc = Ast.loc_of_expr e in <:expr< ($e$ : $t$) >>

  let unescape lab =
    assert (lab <> "");
    let lab =
      if lab.[0] = '_' then String.sub lab 1 (String.length lab - 1) else lab
    in
    try
      let i = String.rindex lab '_' in
      if i = 0 then raise Not_found;
      String.sub lab 0 i
    with Not_found ->
      lab

  let fresh_type _loc = <:ctyp< '$random_var ()$ >>

  let access_object e m m_loc m_typ f =
    let _loc = Ast.loc_of_expr e in
    let x = random_var () in
    let obj_type = fresh_type _loc in
    let obj = <:expr< ($e$ : Js.t (< .. > as $obj_type$)) >> in
    let constr =
      let y = random_var () in
      let body =
        let o = <:expr< $lid:y$ >> in
        let _loc = m_loc in <:expr< ($o$#$m$ : $m_typ$) >>
      in
      <:expr< fun ($lid:y$ : $obj_type$) -> $body$ >>
    in
    <:expr< let $lid:x$ = $obj$ in
            let _ = $constr$ in
            $f x$ >>

  let method_call _loc obj lab lab_loc args =
    let args = List.map (fun e -> (e, fresh_type _loc)) args in
    let ret_type = fresh_type _loc in
    let method_type =
      List.fold_right
        (fun (_, arg_ty) rem_ty -> <:ctyp< $arg_ty$ -> $rem_ty$ >>)
        args <:ctyp< Js.meth $ret_type$ >>
    in
    access_object obj lab lab_loc method_type (fun x ->
    let args =
      List.map (fun (e, t) -> <:expr< Js.Unsafe.inject $with_type e t$ >>) args
    in
    let args = make_array _loc args in
    <:expr<
       (Js.Unsafe.meth_call $lid:x$ $str:unescape lab$ $args$ : $ret_type$)
    >>)

  let new_object _loc constructor args =
    let args = List.map (fun e -> (e, fresh_type _loc)) args in
    let obj_type = <:ctyp< Js.t $fresh_type _loc$ >> in
    let constr_fun_type =
      List.fold_right
        (fun (_, arg_ty) rem_ty -> <:ctyp< $arg_ty$ -> $rem_ty$ >>)
        args obj_type
    in
    let args =
      List.map (fun (e, t) -> <:expr< Js.Unsafe.inject $with_type e t$ >>) args
    in
    let args = make_array _loc args in
    let x = random_var () in
    let constr =
      with_type constructor <:ctyp< Js.constr $constr_fun_type$ >> in
    with_type
      <:expr< let $lid:x$ = $constr$ in
              Js.Unsafe.new_obj $lid:x$ $args$ >>
      <:ctyp< $obj_type$ >>

  let jsobject _loc vars_and_methods =
    let random_ty () = <:ctyp< ' $lid:random_var ()$ >> in
    let obj_lid = random_var () in
    let constr_lid = random_var () in
    let self_ty = random_ty () in
    let module S = Set.Make(String) in

    (* signal error if there are duplicated vars/methods *)
    let _ =
      List.fold_left
        (fun (vars, methods) -> function
             `Var (n, e, _) ->
               if S.mem n vars then
                 Loc.raise (Ast.loc_of_expr e)
                   (Failure (Printf.sprintf "jsobject var %s is repeated" n));
               (S.add n vars, methods)
           | `Meth (n, _, e, _) ->
               if S.mem n methods then
                 Loc.raise (Ast.loc_of_expr e)
                   (Failure (Printf.sprintf "jsobject method %s is repeated" n));
               (vars, S.add n methods))
        (S.empty, S.empty)
        vars_and_methods in

    let vars_and_methods =
      List.map
        (function
             `Var (n, e, is_opt) -> `Var (n, e, random_ty (), is_opt)
           | `Meth (n, params, e, is_opt) ->
               (* we assign a random var name to () params *)
               let params =
                 List.map
                   (function
                        `Id x -> (x, random_ty ())
                      | `Unit -> (random_var (), <:ctyp< unit >>))
                   params
               in `Meth (n, params, e, random_ty (), is_opt))
        vars_and_methods in

    let meth_ty params ty =
      List.fold_right
        (fun x ty -> <:ctyp< $x$ -> $ty$ >>)
        (List.map snd params)
        ty in

    let set_field = function
        `Var (n, e, ty, _) ->
          let _loc = Ast.loc_of_expr e in
            <:expr< Js.Unsafe.set $lid:obj_lid$ (Js.string $str:n$) ($e$ : $ty$) >>
      | `Meth (n, params, e, ty, _) ->
          (* the fst param is self, which we don't want to appear in the type *)
          let params = match params with _ :: tl | tl -> tl in
            <:expr<
              Js.Unsafe.set $lid:obj_lid$ (Js.string $str:n$)
                (Js.wrap_meth_callback $lid:n$ :
                   Js.meth_callback
                     $self_ty$
                     $meth_ty params ty$) >> in

    let assignments =
      List.fold_right
        (fun x e -> <:expr< do { $set_field x$; $e$; } >>)
        vars_and_methods <:expr< () >> in

    let meth_def n params body e =
      let rec meth_fun is_self = function
          [] -> body
        | (param, ty) :: tl ->
            let ty = if is_self then self_ty else ty in
              <:expr< fun ($lid:param$ : $ty$) -> $meth_fun false tl$ >>
      in
        let _loc = Ast.loc_of_expr body in
          <:expr< let $lid:n$ = $meth_fun true params$ in $e$ >> in

    let meth_defs_and_field_assignments =
      List.fold_right
        (fun field e -> match field with
             `Var _ -> e
           | `Meth (n, params, body, _, _) -> meth_def n params body e)
        vars_and_methods
        assignments in

    let obj_field_meth_ctyp = function
        `Var (n, _, vty, _) -> <:ctyp< $lid:n$ : Js.prop $vty$ >>
     | `Meth (n, params, _, mty, _) ->
         (* the fst param is self *)
         let params = match params with _ :: tl | tl -> tl in
         let wrapped_mty = <:ctyp< Js.meth $mty$ >> in
           <:ctyp< $lid:n$ : $meth_ty params wrapped_mty$ >> in

    let make_obj_ty vars_and_methods row_var_flag ty =
      match vars_and_methods with
          [] -> <:ctyp< < $ty$ $..:row_var_flag$ > >>
        | l ->
            let tyl =
              List.fold_right
                (fun x ty ->
                   let t = obj_field_meth_ctyp x in
                     <:ctyp< $t$; $ty$ >>)
                (List.rev l)
                ty
            in <:ctyp< < $tyl$ $..:row_var_flag$ > >> in

    let optional_part_ty =
      make_obj_ty vars_and_methods <:row_var_flag<..>> <:ctyp< >> in
    let obj_ty =
      make_obj_ty
        (* we deliberately leave optional properties/methods out
         * so that we don't need to cast the object manually *)
        (List.filter
           (function `Var (_, _, _, is_opt) | `Meth (_, _, _, _, is_opt) -> not is_opt)
           vars_and_methods)
        <:row_var_flag<>>
        (<:ctyp< __optional__ : $optional_part_ty$ >>)
    in
      <:expr<
        let ($lid:obj_lid$ : Js.t $obj_ty$ as $self_ty$) =
          let $lid:constr_lid$ : Js.constr $self_ty$ = Js.Unsafe.variable "Object" in
          let $lid:obj_lid$ = Js.Unsafe.new_obj $lid:constr_lid$ [||] in
            do {
              $meth_defs_and_field_assignments$;
              $lid:obj_lid$
            }
        in $lid:obj_lid$
       >>

  let jsmeth = Gram.Entry.mk "jsmeth"
  let jsobj_var_and_methods = Gram.Entry.mk "jsobj_var_and_methods"
  let jsobj_var = Gram.Entry.mk "jsobj_var"
  let jsobj_meth_param = Gram.Entry.mk "jsobj_meth_param"

  let method_declaration _loc name params e ~is_opt =
    (* we expand   method foo = x   to  method foo rand_var_for_self () x
     * and   method foo self = x    to  method foo self () = x
     *)
    let params = match params with
        [] -> [`Id (random_var ()); `Unit]
      | [ `Id x ] -> [`Id x; `Unit]
      | `Unit :: _ ->
          Loc.raise _loc
            (Failure
               ("First param of method must be a self variable, \
                 it cannot be unit"))
      | l -> l
    in `Meth (name, params, e, is_opt)

  let set_optional_flag =
    List.map
      (function
           `Var (i, e, _) -> `Var (i, e, true)
         | `Meth (n, p, e, _) -> `Meth (n, p, e, true))

  EXTEND Gram
    jsmeth: [["##"; lab = label -> (_loc, lab) ]];
    expr: BEFORE "."
    ["##" RIGHTA
     [ e = SELF; (lab_loc, lab) = jsmeth ->
         let prop_type = fresh_type _loc in
         let meth_type = <:ctyp< Js.gen_prop <get:$prop_type$; ..> >> in
         access_object e lab lab_loc meth_type (fun x ->
         <:expr< (Js.Unsafe.get $lid:x$ $str:unescape lab$ : $prop_type$) >>)
     | e1 = SELF; (lab_loc, lab) = jsmeth; "<-"; e2 = expr LEVEL "top" ->
         let prop_type = fresh_type _loc in
         let meth_type = <:ctyp< Js.gen_prop <set:$prop_type$->unit; ..> >> in
         access_object e1 lab lab_loc meth_type (fun x ->
         <:expr<
           Js.Unsafe.set $lid:x$ $str:unescape lab$ ($e2$ : $prop_type$)
         >>)
     | e = SELF; (lab_loc, lab) = jsmeth; "("; ")" ->
         method_call _loc e lab lab_loc []
     | e = SELF; (lab_loc, lab) = jsmeth; "("; l = comma_expr; ")" ->
         method_call _loc e lab lab_loc (parse_comma_list l)
     ]];
    expr: LEVEL "simple"
    [[ "jsnew"; e = expr LEVEL "label"; "("; ")" ->
         new_object _loc e []
     | "jsnew"; e = expr LEVEL "label"; "("; l = comma_expr; ")" ->
         new_object _loc e (parse_comma_list l)
     | "jsnew"; "object"; l = LIST0 jsobj_var_and_methods; "end" -> jsobject _loc l
     | "jsnew"; "virtual"; "object"; l = LIST0 jsobj_var_and_methods; "end" ->
         (* a "virtual" object is one where all the methods and vars are
          * optional *)
         jsobject _loc (set_optional_flag l)
     | "{:"; vars = LIST0 jsobj_var SEP ";"; ":}" -> jsobject _loc vars
     | "virtual"; "{:"; vars = LIST0 jsobj_var SEP ";"; ":}" ->
         jsobject _loc (set_optional_flag vars)
    ]];
    jsobj_var_and_methods:
    [[
      "var"; x = jsobj_var -> x
     | "method"; i = a_LIDENT; params = LIST0 jsobj_meth_param; "="; e = expr ->
         method_declaration _loc i params e ~is_opt:false
     | "method"; "virtual"; i = a_LIDENT; params = LIST0 jsobj_meth_param; "="; e = expr ->
         method_declaration _loc i params e ~is_opt:true

    ]];
    jsobj_var:
    [[ i = a_LIDENT; "="; e = expr LEVEL "top" -> `Var (i, e, false)
     | "virtual"; i = a_LIDENT; "="; e = expr LEVEL "top" -> `Var (i, e, true)
     | i = a_LIDENT -> `Var (i, <:expr< $lid:i$ >>, false)
     | "virtual"; i = a_LIDENT -> `Var (i, <:expr< $lid:i$ >>, true)
    ]];
    jsobj_meth_param:
    [[
       i = a_LIDENT -> `Id i
     | "("; ")" -> `Unit
    ]];
    END

(*XXX n-ary methods

how to express optional fields?  if they are there, they must have
some type, but  they do not have to be there

use variant types instead of object types?
   in a negative position...  (but then we have to negate again...)

    { foo: "bar", baz : 7 } : [`foo of string field | `baz of int field] obj

    let f (x : t) = (x : [< `foo of string field | `baz of int field| `x of string field] obj)


XXXX
module WEIRDMODULENAME = struct type 'a o = 'a Js.t val unsafe_get = Js.Unsafe.get ... end
(let module M = WEIRDMODULENAME in (M.unsafe_get : <x : 'a M.meth> -> 'a))

XXXX be more careful with error messages:
  put coercions against arguments or whole expression
*)

end

module M = Register.OCamlSyntaxExtension(Id)(Make)
