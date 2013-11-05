
(* OASIS_START *)
(* DO NOT EDIT (digest: d41d8cd98f00b204e9800998ecf8427e) *)
(* OASIS_STOP *)

open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

(* hack to prevent discovering the wrong ocamlbuild binary *)
(* This should be fix with the latest version of ocaml (4.01) *)
let _ = Pack.Ocamlbuild_where.bindir := "/"

(* Utils *)

let read_lines ?(empty=false) file =
  let ic = open_in file in
  let rec aux l =
    try
      let s=input_line ic in
      if not empty && s="" then aux l
      else aux (s::l)
    with End_of_file -> List.rev l in
  aux []

let read_mllist file =
  let b = Filename.dirname file in
  let l = read_lines file in
  List.map (fun f -> Filename.concat b (String.uncapitalize f)) l

let read_filelist file =
  let b = Filename.dirname file in
  let l = read_lines file in
  List.map (fun f -> Filename.concat b f) l

(* remove duplicated in sorted list *)
let rec uniq_from_sorted l acc = match l,acc with
  | [],[] -> []
  | [],acc -> acc
  | x::xs , a::l when x = a -> uniq_from_sorted xs acc
  | x::xs , acc -> uniq_from_sorted xs (x::acc)

let find_first_good_or_fail build l =
  let rec find = function
    | [] -> raise Not_found
    | Pack.My_std.Outcome.Good name::_ -> name
    | _ :: xs -> find xs in
  let res = build (List.map (fun x -> [x]) l) in
  find res

(* add syntax extension *)
let add_syntax name path =
  (* hack : not dep when "compile" to avoid the extension syntax to be link with binaries *)
  (* the dep with ocamldep make sure the extension syntax is compiled before *)
  flag ["ocaml";"compile";"syntax_"^name]  (S [A "-ppopt" ;P (path ^ name -.- "cma") ]);
  flag_and_dep ["ocaml";"ocamldep";"syntax_"^name] (S [A "-ppopt" ;P (path ^ name -.- "cma") ])


(* Concat js files into a single js file *)
let init_jslib () =
  let dep = "%.jslib"
  and prod = "%.js" in
  rule "concat"
    ~dep
    ~prod
    (fun env build ->
       let files = read_filelist (env dep) in
       let content = List.map (fun f ->
           let fname = (f ^ ".js") in
           ignore(build [[fname]]);
           read_lines ~empty:true fname) files in
       let content = List.flatten content in
       let content = List.map (fun s -> [s;"\n"]) content in
       let content = List.flatten content in
       Echo (content, env prod)
    )

(* List all external primitives used by a lib *)
let init_joo_external () =
  let dep = "%.mllib"
  and prod = "%.external" in
  rule "joo_external"
    ~dep
    ~prod
    (fun env build ->
       let modules = read_mllist (env dep) in
       let files = List.map (fun m ->
           try P (find_first_good_or_fail build [m -.- "ml"])
           with Not_found -> N
         ) modules in
       Cmd (
         S [
           A "sed"; A "-n" ; A "-e";
           Sh "'s/.*external.*\"\\([^\"%]*\\)\".*/\\1/p'";
           S files;
           Sh ">";
           A (env prod)
         ]
       )
    )

(* Create a fake stubs from external file *)
let init_joo_stubs () =
  let dep = "%.external"
  and prod = "%_stubs.c" in
  rule "joo_stubs"
    ~dep
    ~prod
    (fun env build ->
       let header = [
         "#include <stdlib.h>\n";
	       "#include <stdio.h>\n";
         "#define D(f) void f () { fprintf(stderr, \"Unimplemented Javascript primitive %s!\\\\n\", #f); exit(1); }\n";
       ] in
       let prims = read_lines (env dep) in
       let prims = uniq_from_sorted (List.fast_sort String.compare prims) [] in
       let prims_str = List.map (fun prim -> Printf.sprintf "D(%s)\n" prim) prims in
       Echo (header @ prims_str , env prod)
    )

(* js_of_ocaml rule *)
let init_js_of_ocaml () =
  let dep = "%.byte" in
  let prod = "%.js" in
  let f env build =
    let dep = env dep in
    let prod = env prod in
    let link_opts = [](* link_opts prod *) in
    let tags = tags_of_pathname dep ++ "js_of_ocaml" in
    let compiler = find_first_good_or_fail build ["compiler/compile.native";"compiler/compile.byte"] in
    Cmd (S [A compiler; A "-noruntime"; T tags; S link_opts; P dep; A "-o"; Px prod])
  in
  rule "js_of_ocaml: .byte -> .js" ~dep ~prod f;
  flag ["js_of_ocaml"; "debug"] (S [A "-pretty"; A "-debuginfo"; A "-noinline"]);
  pflag ["js_of_ocaml"] "opt" (fun n -> S [A "-opt"; A n]);
  pflag ["js_of_ocaml"] "jsopt" (fun opt -> S[Sh opt]);
  pflag_and_dep ["js_of_ocaml"] "with_js" (fun f -> P f)

(* phantomjs rule *)
let init_phantom_js () =
  let dep = "%.js"
  and prod = "%.jslog" in
  rule "phantomjs"
    ~dep
    ~prod
    (fun env build ->
       Cmd (
         S [
           Sh "phantomjs";
           P (env dep);
           Sh ">";
           P (env prod);
         ]))

(* Expunge a list of Module bytecode *)
let init_expunge () =
  let dep_desc = "%.expunge"
  and dep_byte = "%.byte"
  and prod = "%_expunge.byte" in
  rule "expunge"
    ~deps:[dep_desc;dep_byte]
    ~prod
    (fun env build ->
       let units = read_lines (env dep_desc) in
       Cmd (
         S [
           V"OCAMLFIND";
           A"stdlib/expunge";
           P (env dep_byte);
           P (env prod);
           S (List.map (fun u -> A u) units)
         ]
       )
    )

let () = dispatch (fun hook ->
  dispatch_default hook;
  match hook with
    | Before_options ->
      Options.make_links:=false;
      Options.use_ocamlfind := true
    | After_rules ->
    add_syntax "pa_js" "lib/syntax/";
    add_syntax "pa_deriving_Json" "lib/syntax/";

    if Sys.ocaml_version.[0] = '4'
    then begin
      ocaml_lib ~extern:true ~dir:"+compiler-libs" ~tag_name:"use_toplevellib" "ocamlcommon";
      ocaml_lib ~extern:true ~dir:"+compiler-libs" ~tag_name:"use_toplevellib" "ocamlbytecomp";
      ocaml_lib ~extern:true ~dir:"+compiler-libs" ~tag_name:"use_toplevellib" "ocamltoplevel";
    end else begin
      ocaml_lib ~extern:true ~dir:"+compiler-libs" ~tag_name:"use_toplevellib" "toplevellib";
    end;

    pflag ["ocaml";"parser";"menhir"] "menhir_external_token" (fun m -> S [A "--external-tokens"; A m]);

    init_js_of_ocaml ();
    init_joo_stubs ();
    init_joo_external ();
    init_jslib ();
    init_phantom_js ();
    init_expunge ()
| _ ->
             ())
