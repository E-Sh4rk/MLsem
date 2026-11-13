(* Rewriting the type of a [val] line: naming its repeated parts with
   [where], and the inverse, inlining the [where] definitions.

   Both work on the parsed type and print the result back in MLsem syntax.
   The printer does not keep the user's formatting, so a result is only
   offered if parsing it gives back the same type. *)

open Mlsem_types
open TyExpr

(* ===== Structure ===== *)

(* Holes are fresh variables named [_holeN] by the parser: all are the same
   here, so that a type and its reparse compare equal. *)
let rec strip (t : t) : t =
  match t with
  | TLoc (_, t) -> strip t
  | TVar (KInfer, n) when String.starts_with ~prefix:"_hole" n -> TVar (KInfer, "_hole")
  | t -> map_children strip t

(* [f] is applied to the children from left to right, as written. *)
and map_children f (t : t) : t =
  let rec rx = function
    | Epsilon -> Epsilon
    | Symbol t -> Symbol (f t)
    | Union rs -> Union (List.map rx rs)
    | Concat rs -> Concat (List.map rx rs)
    | Star r -> Star (rx r)
    | Plus r -> Plus (rx r)
    | Option r -> Option (rx r)
  in
  let two k a b =
    let a = f a in
    k a (f b)
  in
  match t with
  | TDyn
  | TVar _
  | TRowVar _
  | TBase _
  | TEnum _
  | TCustom _ ->
      t
  | TApp (s, ts) -> TApp (s, List.map f ts)
  | TTag (s, t) -> TTag (s, f t)
  | TTuple ts -> TTuple (List.map f ts)
  | TRecord (fs, tail) ->
      let fs = List.map (fun (l, t) -> (l, f t)) fs in
      TRecord (fs, f tail)
  | TSList r -> TSList (rx r)
  | TCons (a, b) -> two (fun a b -> TCons (a, b)) a b
  | TArrow (a, b) -> two (fun a b -> TArrow (a, b)) a b
  | TOption t -> TOption (f t)
  | TCup (a, b) -> two (fun a b -> TCup (a, b)) a b
  | TCap (a, b) -> two (fun a b -> TCap (a, b)) a b
  | TDiff (a, b) -> two (fun a b -> TDiff (a, b)) a b
  | TNeg t -> TNeg (f t)
  | TWhere (t, defs) ->
      let t = f t in
      TWhere (t, List.map (fun (n, ps, t) -> (n, ps, f t)) defs)
  | TRecUpd (t, fs) ->
      let t = f t in
      TRecUpd (t, List.map (fun (l, t) -> (l, f t)) fs)
  | TRecProj (t, l) -> TRecProj (f t, l)
  | TTagProj (t, l) -> TTagProj (f t, l)
  | TLoc (loc, t) -> TLoc (loc, f t)

(* The direct subterms of [t]. *)
let children (t : t) =
  let acc = ref [] in
  ignore
    (map_children
       (fun c ->
          acc := c :: !acc ;
          c )
       t ) ;
  List.rev !acc

let rec size t = 1 + List.fold_left (fun n c -> n + size c) 0 (children t)

(* The type names [t] uses, to pick fresh ones. *)
let rec names t =
  let own =
    match t with
    | TCustom s
    | TApp (s, _) ->
        [s]
    | TWhere (_, defs) -> List.map (fun (n, _, _) -> n) defs
    | _ -> []
  in
  own @ List.concat_map names (children t)

(* ===== Printing ===== *)

(* Precedence levels, from the parser: [where] 0, tuple 1, [->] 2, [|] 3,
   [&] 4, [\] 5, [::] 6, [~] 7, atomic 8. A subterm is parenthesized when
   its level is below the one its position requires. *)
let rec level = function
  | TLoc (_, t) -> level t
  | TWhere _ -> 0
  | TTuple _ -> 1
  | TArrow _ -> 2
  | TCup _ -> 3
  | TCap _ -> 4
  | TDiff _ -> 5
  | TCons _ -> 6
  | TNeg _ -> 7
  | _ -> 8

let base = function
  | TInt (Some i, Some j) when Z.equal i j -> Z.to_string i
  | TInt (None, None) -> "int"
  | TInt (i, j) ->
      let b = Option.fold ~none:"" ~some:Z.to_string in
      "(" ^ b i ^ ".." ^ b j ^ ")"
  | TCharInt (c, d) when c = d -> Printf.sprintf "%C" c
  | TCharInt (c, d) -> Printf.sprintf "(%C..%C)" c d
  | TSString s -> Printf.sprintf "%S" s
  | TBool -> "bool"
  | TTrue -> "true"
  | TFalse -> "false"
  | TUnit -> "()"
  | TChar -> "char"
  | TAny -> "any"
  | TEmpty -> "empty"
  | TNil -> "[]"
  | TString -> "string"
  | TList -> "list"
  | TFloat -> "float"
  | TArrowAny -> "arrow"
  | TTupleAny -> "tuple"
  | TTupleN n -> "tuple" ^ string_of_int n
  | TEnumAny -> "enum"
  | TTagAny -> "tag"
  | TRecordAny -> "record"

let rec pr lvl t =
  let s = pr_node t in
  if level t < lvl then "(" ^ s ^ ")" else s

and pr_node (t : t) =
  let fields fs = List.map (fun (l, t) -> l ^ " : " ^ pr 2 t) fs |> String.concat "; " in
  match t with
  | TLoc (_, t) -> pr_node t
  | TDyn -> "dyn"
  | TVar (KInfer, n) when String.starts_with ~prefix:"_hole" n -> "_"
  | TVar (_, n) -> "'" ^ n
  | TRowVar (_, n) -> "`" ^ n
  | TBase b -> base b
  | TCustom s
  | TEnum s ->
      s
  | TApp (s, ts) -> s ^ "(" ^ String.concat ", " (List.map (pr 2) ts) ^ ")"
  | TTag (s, TBase TUnit) -> s ^ "()"
  | TTag (s, t) -> s ^ "(" ^ pr 0 t ^ ")"
  | TTuple ts -> String.concat ", " (List.map (pr 2) ts)
  | TRecord (fs, tail) ->
      let tail =
        match tail with
        | TOption (TBase TAny) -> " .."
        | TOption (TBase TEmpty) -> ""
        | t -> " ;; " ^ pr 0 t
      in
      "{ " ^ fields fs ^ tail ^ " }"
  | TSList Epsilon -> "[]"
  | TSList (Symbol t) -> "[ " ^ pr 0 t ^ " ]"
  | TSList r -> "[ " ^ rx 0 r ^ " ]"
  | TCons (a, b) -> pr 7 a ^ " :: " ^ pr 6 b
  | TArrow (a, b) -> pr 3 a ^ " -> " ^ pr 2 b
  | TOption t -> pr 8 t ^ "?"
  | TCup (a, b) -> pr 3 a ^ " | " ^ pr 4 b
  | TCap (a, b) -> pr 4 a ^ " & " ^ pr 5 b
  | TDiff (a, b) -> pr 5 a ^ " \\ " ^ pr 6 b
  | TNeg t -> "~" ^ pr 7 t
  | TWhere (t, defs) ->
      let def (n, ps, t) =
        let n =
          if ps = [] then n else n ^ "(" ^ String.concat ", " (List.map (( ^ ) "'") ps) ^ ")"
        in
        n ^ " = " ^ pr 1 t
      in
      pr 1 t ^ " where " ^ String.concat " and " (List.map def defs)
  | TRecUpd (t, fs) -> "{ " ^ pr 1 t ^ " with " ^ fields fs ^ " }"
  | TRecProj (t, l)
  | TTagProj (t, l) ->
      pr 8 t ^ "." ^ l

(* Regular expressions: union 0, concatenation 1, postfix 2; a symbol in
   them is atomic. *)
and rx lvl r =
  let paren l s = if l < lvl then "(" ^ s ^ ")" else s in
  match r with
  | Epsilon -> "()"
  | Symbol t -> pr 8 t
  | Union rs -> paren 0 (String.concat " | " (List.map (rx 1) rs))
  | Concat rs -> paren 1 (String.concat " " (List.map (rx 2) rs))
  | Star r -> rx 2 r ^ "*"
  | Plus r -> rx 2 r ^ "+"
  | Option r -> rx 2 r ^ "?"

let print t = pr 0 t

(* [t] printed, if the text parses back to it. *)
let checked_print t =
  let s = print t in
  match strip (Mlsem_app.IO.parse_type_string s) = strip t with
  | true -> Some s
  | false
  | (exception _) ->
      None

(* ===== Extracting ===== *)

let rec subterms t = t :: List.concat_map subterms (children t)

let is_hole = function
  | TVar (KInfer, "_hole") -> true
  | _ -> false

(* Parts not worth a name, or that cannot have one: [t?] only means
   something as a record field (an optional one; a closed record's tail is
   [empty?]), a row variable only in a record's tail, [dyn] is rejected in
   a [where] definition, and naming a part with a [_] would make its copies
   one hole instead of several. *)
let unnamable t =
  match t with
  | TDyn
  | TVar _
  | TRowVar _
  | TBase _
  | TCustom _
  | TEnum _
  | TTag (_, TBase TUnit) ->
      true
  | TApp (_, []) -> true
  | _ ->
      List.exists
        (function
          | TOption _
          | TRowVar _
          | TDyn ->
              true
          | t -> is_hole t )
        (subterms t)

let rec replace ~what ~by t = if t = what then by else map_children (replace ~what ~by) t

(* Name the repeated parts of [t], largest first, until none is left: each
   becomes a [where] definition. Repeats are counted in the type and in the
   definitions made so far, so a part shared by two definitions gets a name
   too. With [holes], the definitions are holes instead, and only those
   still used are kept. *)
let extract ~holes t =
  let t = strip t in
  let body, old_defs =
    match t with
    | TWhere (b, d) -> (b, d)
    | b -> (b, [])
  in
  let used = ref (names t) in
  let fresh () =
    let rec go i =
      let n = if i < 26 then String.make 1 (Char.chr (97 + i)) else "t" ^ string_of_int (i - 25) in
      if List.mem n !used then go (i + 1) else n
    in
    let n = go 0 in
    used := n :: !used ;
    n
  in
  let rec loop body defs =
    let all = List.concat_map subterms (body :: List.map (fun (_, _, t) -> t) defs) in
    let counts = Hashtbl.create 16 in
    List.iter
      (fun s ->
         if not (unnamable s) then
           Hashtbl.replace counts s (1 + Option.value ~default:0 (Hashtbl.find_opt counts s)) )
      all ;
    let best =
      Hashtbl.fold
        (fun s n best ->
           if n < 2 then
             best
           else
             match
               best
             with
             | Some b when size b >= size s -> best
             | _ -> Some s )
        counts None
    in
    match best with
    | None -> (body, defs)
    | Some s ->
        let n = fresh () in
        let by = TCustom n in
        let defs = List.map (fun (m, ps, t) -> (m, ps, replace ~what:s ~by t)) defs in
        loop (replace ~what:s ~by body) (defs @ [(n, [], s)])
  in
  let body, defs = loop body [] in
  let defs =
    if not holes then
      defs
    else
      (* Only the definitions the type itself uses: those used only inside
         another definition disappear with it. *)
      let refs t = names t in
      List.filter (fun (n, _, _) -> List.mem n (refs body)) defs
      |> List.map (fun (n, ps, _) -> (n, ps, TVar (KInfer, "_hole")))
  in
  if defs = [] then None else checked_print (TWhere (body, old_defs @ defs))

(* ===== Inlining ===== *)

(* Replace the name [n] by [by] in [t], except under a [where] that defines
   it again. *)
let rec subst n by t =
  match t with
  | TCustom m when m = n -> by
  | TWhere (_, defs) when List.exists (fun (m, _, _) -> m = n) defs -> t
  | t -> map_children (subst n by) t

(* Inline the parameterless definitions of a top-level [where] that do not
   refer to themselves, in order; the others stay. *)
let rec count_name n t =
  match t with
  | TCustom m when m = n -> 1
  | TWhere (_, defs) when List.exists (fun (m, _, _) -> m = n) defs -> 0
  | t -> List.fold_left (fun k c -> k + count_name n c) 0 (children t)

let inline t =
  match strip t with
  | TWhere (body, defs) as t ->
      (* A [_] in a definition used more than once is one hole: inlined, it
         becomes a named one (['_a]) so that its copies stay one. *)
      let used =
        ref
          (List.filter_map
             (function
               | TVar (_, n) -> Some n
               | _ -> None )
             (subterms t) )
      in
      let fresh () =
        let rec go i =
          let n =
            "_"
            ^ String.make 1 (Char.chr (97 + (i mod 26)))
            ^ if i < 26 then "" else string_of_int (i / 26)
          in
          if List.mem n !used then go (i + 1) else n
        in
        let n = go 0 in
        used := n :: !used ;
        n
      in
      let rec name_holes t =
        if is_hole t then TVar (KInfer, fresh ()) else map_children name_holes t
      in
      (* An inlined definition is replaced in the type and in every other
         definition, before and after it. *)
      let rec go body kept = function
        | [] -> (body, kept)
        | (n, ps, d) :: rest when ps <> [] || List.mem n (names d) ->
            go body (kept @ [(n, ps, d)]) rest
        | (n, _, d) :: rest ->
            let uses =
              List.fold_left
                (fun k (_, _, t) -> k + count_name n t)
                (count_name n body) (kept @ rest)
            in
            let d = if uses > 1 then name_holes d else d in
            let sub = subst n d in
            let sub_def (m, ps, t) = (m, ps, sub t) in
            go (sub body) (List.map sub_def kept) (List.map sub_def rest)
      in
      let body, kept = go body [] defs in
      if List.length kept = List.length defs then
        None
      else
        checked_print (if kept = [] then body else TWhere (body, kept))
  | _ -> None
