(* What the toolkit's part tools (Widen, Simplify) share: the [val] lines of
   the definition at a position, and per line the parts of its type that a
   tool has options for. A part is located by the parser; an option is
   written in its place, parentheses included, in parentheses when the
   printer puts it in them there. *)

open Mlsem_app
open Mlsem_types
open Yojson.Safe.Util

let error msg = `Assoc [("ok", `Bool false); ("error", `String msg)]

(* Build a type expression of the [val] type [line]: under the line's
   [where] definitions, so that a part may use their names ([Node(x1)] in
   [... where x1 = ...]). *)
let build envs line =
  let defs =
    match Type_refactor.strip line with
    | TyExpr.TWhere (_, defs) -> defs
    | _ -> []
  in
  fun t ->
    let t = if defs = [] then t else TyExpr.TWhere (t, defs) in
    Typings.build_signature envs t |> fst |> Signature.to_gty |> GTy.ub

(* The declared type [name] with [k] parameters, applied to the tightest
   arguments that make it contain [part_ty]: [part_ty ≤ name('_1, …, '_k)]
   tallied, the part's own variables fixed, and each argument its lower
   bound (the variables tally leaves in it [empty]). The arguments are
   printed and parsed back, to be written. [None] when no arguments fit. *)
let applied envs build part_ty name k =
  let open TyExpr in
  let names = List.init k (fun i -> "_arg" ^ string_of_int i) in
  match build (TApp (name, List.map (fun n -> TVar (KInfer, n)) names)) with
  | exception _ -> None
  | c_ty -> (
      let args =
        TVOp.vars c_ty |> MVarSet.proj1 |> TVarSet.elements |> List.filter (TVar.has_kind KInfer)
      in
      let arg n = List.find_opt (fun v -> TVar.name v = n) args in
      let mono = MVarSet.union (TVOp.all_vars KNoInfer) (TVOp.vars part_ty) in
      match TVOp.tally mono [(part_ty, c_ty)] with
      | [] -> None
      | s :: _ -> (
          let lower t =
            let t = Subst.apply s t in
            let left =
              TVOp.vars t |> MVarSet.proj1 |> TVarSet.elements
              |> List.filter (fun v -> TVar.has_kind KInfer v && not (MVarSet.mem1 v mono))
            in
            Subst.apply (Subst.of_list1 (List.map (fun v -> (v, Ty.empty)) left)) t
          in
          let print t = Main.display_gty envs (GTy.mk t) in
          match
            List.map
              (fun n ->
                 match arg n with
                 | Some v -> IO.parse_type_string (print (lower (TVar.typ v)))
                 | None -> TBase TAny )
              names
          with
          (* An [empty] argument: the part has none of that parameter's
             values ([tree(empty)] for [Leaf]), not worth the name. *)
          | exprs ->
              let exprs = List.map Type_refactor.strip exprs in
              if List.mem (TBase TEmpty) exprs then None else Some (TApp (name, exprs))
          | exception _ -> None ) )

(* The declared types that may contain a part of type [part_ty]: those
   without parameters by name, those with parameters applied to the
   tightest arguments that fit ([applied]). [build] builds an expression
   where the part is. *)
let declared envs build part_ty =
  Main.user_type_names envs
  |> List.concat_map (fun n ->
    match Main.user_alias_arity envs n with
    | Some k when k > 0 -> Option.to_list (applied envs build part_ty n k)
    | _ -> [TyExpr.TCustom n] )

(* [c] as written in place of the part [p]. *)
let written (p : Type_refactor.part) c =
  match p.parent with
  | Some (parent, i) -> Type_refactor.print_as_child parent i c
  | None -> Type_refactor.print c

(* The type written [text], [where] definitions included, and its parts as
   the parser locates them ([Type_refactor.located_parts]), the type itself
   first; a part's span is in [text]. Raises when [text] is not a type. *)
let locate text =
  let ty = IO.parse_type_string text in
  (ty, Type_refactor.located_parts ty)

(* The part [p] of [text], for the client: its [start, end), [shift]ed, its
   text, and [fields]. *)
let part_json text ?(shift = 0) (p : Type_refactor.part) fields : Yojson.Safe.t =
  let s, e = p.span in
  `Assoc
    ( [
        ("start", `Int (s + shift)); ("end", `Int (e + shift));
        ("text", `String (String.sub text s (e - s)));
      ]
    @ fields )

(* The [val] lines of the definition at [position]: per line, its number and
   text, and per part (on that line) its columns, its text and its options,
   each as written and with a label if any. [options envs line part] gives
   them, [None] for none: [line] is the line's whole type. *)
let lines params options : Yojson.Safe.t =
  Signature_check.with_target params (fun text offsets (t : Signature_check.target) ->
    let uri =
      (params |> member "textDocument" |> Lsp.Types.TextDocumentIdentifier.t_of_yojson).uri
    in
    (* Only the types matter: the declarations' environment, without
       typechecking the definitions. *)
    match (Store.declarations uri text, Main.parse (`String text)) with
    | None, _
    | _, PFailure _ ->
        error "The file does not parse."
    | Some (envs, _), PSuccess program ->
        let in_vals s = List.exists (fun (vs, ve) -> vs <= s && s < ve) t.vals in
        let lines =
          List.filter_map
            (fun (_, elem) ->
               match elem with
               | PAst.SigDef (_, _, Mlsem_types.TyExpr.TLoc ((s, e), _))
                 when in_vals s.Lexing.pos_cnum ->
                   let s = s.pos_cnum and e = e.Lexing.pos_cnum in
                   let line = (Doc.position_of_offset offsets s).line in
                   let line_start = offsets.(line) in
                   let line_end =
                     match String.index_from_opt text line_start '\n' with
                     | Some i -> i
                     | None -> String.length text
                   in
                   let option (text, label) =
                     `Assoc
                       ( ("text", `String text)
                       :: Option.fold ~none:[] ~some:(fun l -> [("label", `String l)]) label )
                   in
                   let ty_text = String.sub text s (e - s) in
                   let ty, located = locate ty_text in
                   let parts =
                     located
                     |> List.filter_map (fun (p : Type_refactor.part) ->
                       (* A part on this line only. *)
                       if s + snd p.span > line_end then
                         None
                       else
                         match
                           options envs ty p
                         with
                         | None
                         | Some [] ->
                             None
                         | Some os ->
                             Some
                               (part_json ty_text ~shift:(s - line_start) p
                                  [("options", `List (List.map option os))] ) )
                   in
                   Some
                     (`Assoc
                         [
                           ("line", `Int line);
                           ("text", `String (String.sub text line_start (line_end - line_start)));
                           ("parts", `List parts);
                         ] )
               | _ -> None )
            program
        in
        `Assoc [("ok", `Bool true); ("lines", `List lines)] )
