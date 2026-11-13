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

(* Printed text, with the span of each part printed in it: the part (as
   given to the printer) and its [start, end) in the text, without the
   parentheses around it. *)
type piece = {
  s: string;
  spans: (t * int * int) list;
  wrapped: t list;  (** the parts printed in parentheses *)
}

let lit s = {s; spans = []; wrapped = []}

let cat ps =
  let _, s, spans =
    List.fold_left
      (fun (off, s, spans) p ->
         ( off + String.length p.s,
           s ^ p.s,
           List.rev_append (List.map (fun (t, a, b) -> (t, a + off, b + off)) p.spans) spans ) )
      (0, "", []) ps
  in
  {s; spans; wrapped = List.concat_map (fun p -> p.wrapped) ps}

let join sep ps =
  cat
    (List.concat
       (List.mapi
          (fun i p ->
             if i = 0 then
               [p]
             else
               [lit sep; p] )
          ps ) )

let rec pr lvl t =
  let p = pr_node t in
  let p = {p with spans = (t, 0, String.length p.s) :: p.spans} in
  if level t < lvl then
    let p = cat [lit "("; p; lit ")"] in
    {p with wrapped = t :: p.wrapped}
  else
    p

and pr_node (t : t) =
  (* [?;] would lex as one operator: an optional field, but the last, is
     followed by a space. *)
  let fields fs =
    let n = List.length fs in
    join "; "
      (List.mapi
         (fun i (l, t) ->
            let gap =
              match t with
              | TOption _ when i < n - 1 -> " "
              | _ -> ""
            in
            cat [lit (l ^ " : "); pr 2 t; lit gap] )
         fs )
  in
  match t with
  | TLoc (_, t) -> pr_node t
  | TDyn -> lit "dyn"
  | TVar (KInfer, n) when String.starts_with ~prefix:"_hole" n -> lit "_"
  | TVar (_, n) -> lit ("'" ^ n)
  | TRowVar (_, n) -> lit ("`" ^ n)
  | TBase b -> lit (base b)
  | TCustom s
  | TEnum s ->
      lit s
  | TApp (s, ts) -> cat [lit (s ^ "("); join ", " (List.map (pr 2) ts); lit ")"]
  | TTag (s, TBase TUnit) -> lit (s ^ "()")
  | TTag (s, t) -> cat [lit (s ^ "("); pr 0 t; lit ")"]
  | TTuple ts -> join ", " (List.map (pr 2) ts)
  | TRecord (fs, tail) ->
      let tail =
        match tail with
        | TOption (TBase TAny) -> lit " .."
        | TOption (TBase TEmpty) -> lit ""
        | t -> cat [lit " ;; "; pr 0 t]
      in
      cat [lit "{ "; fields fs; tail; lit " }"]
  | TSList Epsilon -> lit "[]"
  | TSList (Symbol t) -> cat [lit "[ "; pr 0 t; lit " ]"]
  | TSList r -> cat [lit "[ "; rx 0 r; lit " ]"]
  | TCons (a, b) -> cat [pr 7 a; lit " :: "; pr 6 b]
  | TArrow (a, b) -> cat [pr 3 a; lit " -> "; pr 2 b]
  | TOption t -> cat [pr 8 t; lit "?"]
  | TCup (a, b) -> cat [pr 3 a; lit " | "; pr 4 b]
  | TCap (a, b) -> cat [pr 4 a; lit " & "; pr 5 b]
  | TDiff (a, b) -> cat [pr 5 a; lit " \\ "; pr 6 b]
  | TNeg t -> cat [lit "~"; pr 7 t]
  | TWhere (t, defs) ->
      let def (n, ps, t) =
        let n =
          if ps = [] then n else n ^ "(" ^ String.concat ", " (List.map (( ^ ) "'") ps) ^ ")"
        in
        cat [lit (n ^ " = "); pr 1 t]
      in
      cat [pr 1 t; lit " where "; join " and " (List.map def defs)]
  | TRecUpd (t, fs) -> cat [lit "{ "; pr 1 t; lit " with "; fields fs; lit " }"]
  | TRecProj (t, l)
  | TTagProj (t, l) ->
      cat [pr 8 t; lit ("." ^ l)]

(* Regular expressions: union 0, concatenation 1, postfix 2; a symbol in
   them is atomic. *)
and rx lvl r =
  (* Parenthesized regular expressions are not parts: [wrapped] is for
     types. *)
  let paren l p = if l < lvl then cat [lit "("; p; lit ")"] else p in
  match r with
  | Epsilon -> lit "()"
  | Symbol t -> pr 8 t
  | Union rs -> paren 0 (join " | " (List.map (rx 1) rs))
  | Concat rs -> paren 1 (join " " (List.map (rx 2) rs))
  | Star r -> cat [rx 2 r; lit "*"]
  | Plus r -> cat [rx 2 r; lit "+"]
  | Option r -> cat [rx 2 r; lit "?"]

let print t = (pr 0 t).s

(* [t] printed, with the span of each of its parts. *)
let layout t = pr 0 t

(* [t] with its [i]th child (in the order of [children]) replaced by [c]. *)
let with_child t i c =
  let j = ref (-1) in
  map_children
    (fun x ->
       incr j ;
       if !j = i then c else x )
    t

(* [c] printed as the [i]th child of [parent]: in parentheses when the
   printer puts it in them there. *)
let print_as_child parent i c =
  let wrapped = (layout (with_child (strip parent) i c)).wrapped in
  if List.memq c wrapped then "(" ^ print c ^ ")" else print c

(* A part of a type, found by its location. *)
type part = {
  span: int * int;  (** where it is written, with the parentheses around it *)
  node: t;
  parent: (t * int) option;  (** the part it is in, and its index there *)
}

(* The located parts of [t], [t] itself included, outermost first. A part's
   location is the outermost [TLoc] around it. *)
let located_parts (t : t) : part list =
  let rec peel span = function
    | TLoc ((s, e), u) ->
        peel (if Option.is_none span then Some (s.Lexing.pos_cnum, e.Lexing.pos_cnum) else span) u
    | u -> (span, u)
  in
  let rec go parent t =
    let span, u = peel None t in
    let here =
      match span with
      | Some span -> [{span; node = u; parent}]
      | None -> []
    in
    here @ List.concat (List.mapi (fun i k -> go (Some (u, i)) k) (children u))
  in
  go None t

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
      (* [t?] and row variables are fine where they belong: as a record's
         field or tail. *)
      let rec bad t =
        match t with
        | TOption _
        | TRowVar _
        | TDyn ->
            true
        | TRecord (fs, tail) -> (
            List.exists (fun (_, f) -> bad (unoption f)) fs
            ||
            match tail with
            | TOption f -> bad f
            | TRowVar _ -> false
            | t -> bad t )
        | t -> is_hole t || List.exists bad (children t)
      and unoption = function
        | TOption f -> f
        | f -> f
      in
      bad t

(* ===== Unions and intersections as sets =====

   [1 | true | 'a] and [1 | 'a | true] are the same union: a chain of [|]
   (or [&]) is compared as the set of its members, and a part can be some
   of the members of a union. *)

type ac =
  | Cup
  | Cap

let ac_kind = function
  | TCup _ -> Some Cup
  | TCap _ -> Some Cap
  | _ -> None

(* The members of a chain of [k], through any nesting. *)
let rec ops k t =
  match (k, t) with
  | Cup, TCup (a, b)
  | Cap, TCap (a, b) ->
      ops k a @ ops k b
  | _ -> [t]

let rebuild k = function
  | [] -> invalid_arg "Type_refactor.rebuild"
  | x :: xs ->
      List.fold_left
        (fun acc y ->
           match k with
           | Cup -> TCup (acc, y)
           | Cap -> TCap (acc, y) )
        x xs

(* A canonical form: members of chains sorted, and with [~dedup] without
   duplicates. *)
let rec norm ?(dedup = true) t =
  let t = map_children (norm ~dedup) t in
  match ac_kind t with
  | Some k -> rebuild k ((if dedup then List.sort_uniq else List.sort) compare (ops k t))
  | None -> t

(* The same up to the order of the members of unions and intersections and
   to their duplicates. *)
let same a b = norm a = norm b

(* The same up to the order of the members of unions and intersections
   only: [int & int] is not [int]. *)
let same_but_order a b = norm ~dedup:false a = norm ~dedup:false b

(* The parts of [t]: its subterms, a chain counting as one part made of its
   members (not of its sub-chains). *)
let rec parts t =
  t
  :: List.concat_map parts
       ( match ac_kind t with
       | Some k -> ops k t
       | None -> children t )

(* When [g] is a chain of fewer members than [p], of the same kind, all
   among those of [p]: the positions in [p] of the members of [g]. *)
let sub_members g p =
  match (ac_kind g, ac_kind p) with
  | Some k, Some k' when k = k' ->
      let gs = List.sort_uniq compare (List.map norm (ops k g)) in
      let ps = List.map norm (ops k p) in
      if List.length gs >= List.length (List.sort_uniq compare ps) then
        None
      else
        let rec go taken = function
          | [] -> Some (List.rev taken)
          | x :: xs -> (
              match
                List.find_index
                  (fun i -> i)
                  (List.mapi (fun i y -> y = x && not (List.mem i taken)) ps)
              with
              | Some i -> go (i :: taken) xs
              | None -> None )
        in
        go [] gs
  | _ -> None

(* Replace each occurrence of [what] in [t] by [by], including as some of
   the members of a chain: [1 | 'a | true] with [what = true | 1] becomes
   [by | 'a]. *)
let rec replace ~what ~by t =
  if same t what then
    by
  else
    match
      ac_kind t
    with
    | Some k ->
        let os = ops k t in
        let os =
          match sub_members what t with
          | Some (_ :: _ as idx) ->
              let first = List.fold_left min max_int idx in
              List.concat
                (List.mapi
                   (fun i o ->
                      if i = first then
                        [by]
                      else if List.mem i idx then
                        []
                      else
                        [o] )
                   os )
          | _ -> os
        in
        rebuild k (List.map (replace ~what ~by) os)
    | None -> map_children (replace ~what ~by) t

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
    let all = List.concat_map parts (body :: List.map (fun (_, _, t) -> t) defs) in
    (* By canonical form: the first part written, and how many. *)
    let counts = Hashtbl.create 16 in
    List.iter
      (fun s ->
         if not (unnamable s) then
           let key = norm s in
           let first, n = Option.value ~default:(s, 0) (Hashtbl.find_opt counts key) in
           Hashtbl.replace counts key (first, n + 1) )
      all ;
    let best =
      Hashtbl.fold
        (fun _ (s, n) best ->
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

(* ===== Records sharing some fields =====

   Records can share some of their fields: the shared fields become a
   top-level type [r], and each record [{ r with ... }], the fields it adds.
   Not a [where] definition: MLsem builds those as recursive equations, so
   [with] cannot see that the name is a record. *)

(* A record's fields and tail, for a closed or open record. *)
let record_view = function
  | TRecord (fs, (TOption (TBase (TEmpty | TAny)) as tail)) -> Some (fs, tail)
  | _ -> None

(* When the fields of record [g] are all fields of record [p], and both have
   the same tail: the other fields of [p], in order. *)
let record_rest g p =
  match (record_view g, record_view p) with
  | Some (gs, gt), Some (ps, pt) when gt = pt ->
      let has (l, f) = List.exists (fun (l', f') -> l = l' && same f f') ps in
      if List.for_all has gs then
        Some (List.filter (fun (l, _) -> not (List.mem_assoc l gs)) ps)
      else
        None
  | _ -> None

(* The rigid type variables of [t], in order: the parameters of a top-level
   type made of it. *)
let tvars_of t =
  List.fold_left
    (fun acc s ->
       match s with
       | TVar (KNoInfer, n) when not (List.mem n acc) -> acc @ [n]
       | _ -> acc )
    [] (subterms t)

let has_weak t =
  List.exists
    (function
      | TVar (KInfer, _) -> true
      | _ -> false )
    (subterms t)

(* Whether [g] is extracted as a top-level type: a record that is only part
   of some record in [t]. *)
let global g t =
  record_view g <> None
  && List.exists
       (fun p ->
          match record_rest g p with
          | Some (_ :: _) -> true
          | _ -> false )
       (parts t)

(* Records of [t] sharing at least two fields, with the same tail: the
   shared fields, as a record. *)
let shared_records t =
  let records = List.filter (fun p -> record_view p <> None) (parts t) in
  List.concat
    (List.mapi
       (fun i a ->
          List.concat
            (List.mapi
               (fun j b ->
                  match (record_view a, record_view b) with
                  | Some (fa, ta), Some (fb, tb) when j > i && ta = tb ->
                      let common =
                        List.filter
                          (fun (l, f) -> List.exists (fun (l', f') -> l = l' && same f f') fb)
                          fa
                      in
                      let g = TRecord (common, ta) in
                      if List.length common >= 2 && not (has_weak g) then [g] else []
                  | _ -> [] )
               records ) )
       records )

(* ===== Extracting chosen parts, for the toolkit ===== *)

let holes_in t = List.length (List.filter is_hole (subterms t))

(* The parts of [t] that repeat, largest first. Unlike [extract], parts with
   holes are included: all [_] compare equal, so these group by shape, and
   the toolkit extracts them linking the holes or keeping them apart. *)
let groups t =
  let t = strip t in
  let ps = parts t in
  (* Candidates: every part, and every set of at least two members that two
     chains share (all such sets, up to a common set of 5 members). *)
  let rec subsets = function
    | [] -> [[]]
    | x :: xs ->
        let rest = subsets xs in
        List.map (fun s -> x :: s) rest @ rest
  in
  let chains = List.filter (fun p -> ac_kind p <> None) ps in
  let shared =
    List.concat
      (List.mapi
         (fun i a ->
            List.concat
              (List.filteri
                 (fun j _ -> j > i)
                 (List.map
                    (fun b ->
                       match (ac_kind a, ac_kind b) with
                       | Some k, Some k' when k = k' ->
                           let bs = List.map norm (ops k b) in
                           let common = List.filter (fun m -> List.mem (norm m) bs) (ops k a) in
                           if List.length common > 5 then
                             [rebuild k common]
                           else
                             subsets common
                             |> List.filter (fun s -> List.length s >= 2)
                             |> List.map (rebuild k)
                       | _ -> [] )
                    chains ) ) )
         chains )
  in
  let count g =
    List.length
      (List.filter (fun p -> same p g || sub_members g p <> None || record_rest g p <> None) ps)
  in
  (* [unnamable] without its rule on holes: a hole inside is fine here. *)
  let extractable s =
    let rec no_holes s = if is_hole s then TBase TAny else map_children no_holes s in
    not (unnamable (no_holes s))
  in
  List.fold_left
    (fun acc g ->
       if List.exists (same g) acc || (not (extractable g)) || count g < 2 then acc else acc @ [g] )
    []
    (ps @ shared @ shared_records t)
  |> List.sort (fun a b -> compare (size b, print a) (size a, print b))

(* Where the occurrences of [g] are in [l], the layout of [t]: one span per
   occurrence, or, for a [g] made of some members of a chain, one span per
   run of consecutive members. *)
let occurrences (l : piece) t g =
  let span node =
    List.find_map (fun (s, a, b) -> if s == node then Some (a, b) else None) l.spans
  in
  List.concat_map
    (fun p ->
       if same p g || record_rest g p <> None then
         Option.to_list (span p)
       else
         match
           (sub_members g p, ac_kind p)
         with
         | Some idx, Some k ->
             let os = Array.of_list (ops k p) in
             let runs =
               List.fold_left
                 (fun runs i ->
                    match runs with
                    | (a, b) :: rest when b + 1 = i -> (a, i) :: rest
                    | _ -> (i, i) :: runs )
                 [] (List.sort compare idx)
             in
             List.filter_map
               (fun (i, j) ->
                  match (span os.(i), span os.(j)) with
                  | Some (a, _), Some (_, b) -> Some (a, b)
                  | _ -> None )
               (List.rev runs)
         | _ -> [] )
    (parts t)

(* A default name for a part, from its shape, not in [used]. *)
let default_name used g =
  let base =
    match g with
    | TArrow _ -> "fn"
    | TTuple _ -> "tup"
    | TRecord _ -> "r"
    | TSList _ -> "lst"
    | TCup _ -> "u"
    | TTag (s, _) -> String.lowercase_ascii s
    | _ -> "t"
  in
  let rec go i =
    let n = if i = 1 then base else base ^ string_of_int i in
    if List.mem n used then go (i + 1) else n
  in
  go 1

let valid_name n =
  n <> ""
  && ( match n.[0] with
    | 'a' .. 'z'
    | '_' ->
        true
    | _ -> false )
  && String.for_all
       (function
         | 'a' .. 'z'
         | 'A' .. 'Z'
         | '0' .. '9'
         | '_'
         | '\'' ->
             true
         | _ -> false )
       n

(* Extract the chosen parts of [t], each [(part, name)] with [part] one of
   [groups t]. Smaller parts first, so that a larger part is matched with
   its smaller chosen parts already named. A part with holes becomes, with
   [plumbing], a definition with one parameter per hole, used with a fresh
   hole for each ([fn(_, _) where fn('a, 'b) = 'a -> 'b]): the type keeps
   its meaning. Without, it is used as a plain name ([fn where
   fn = _ -> _]): all its copies share the same holes. *)
(* Records only part of some record ([global]) become top-level types, first:
   the result is the new type and their declarations. [types]: the names of
   the file's types, which a top-level type cannot reuse. *)
let extract_chosen ~plumbing ~types t chosen =
  let t = strip t in
  let body, old_defs =
    match t with
    | TWhere (b, d) -> (b, d)
    | b -> (b, [])
  in
  let taken = names t in
  let globals, chosen = List.partition (fun (g, _) -> global g t) chosen in
  match
    List.find_opt
      (fun (g, n) ->
         (not (valid_name n))
         || List.mem n taken
         || (global g t && List.mem n types)
         || List.length (List.filter (fun (_, m) -> m = n) (globals @ chosen)) > 1 )
      (globals @ chosen)
  with
  | Some (_, n) -> Error (Printf.sprintf "%S cannot be used as a name here." n)
  | None -> (
      (* Top-level record types, before the [where] parts: their
         declarations cannot use the names those define. *)
      let body, old_defs, decls =
        List.fold_left
          (fun (body, old_defs, decls) (g, name) ->
             let params = tvars_of g in
             let use =
               if params = [] then
                 TCustom name
               else
                 TApp (name, List.map (fun p -> TVar (KNoInfer, p)) params)
             in
             let rec rw t =
               match record_rest g t with
               | Some [] -> use
               | Some rest -> TRecUpd (use, List.map (fun (l, f) -> (l, rw f)) rest)
               | None -> map_children rw t
             in
             let head =
               if params = [] then
                 name
               else
                 name ^ "(" ^ String.concat ", " (List.map (( ^ ) "'") params) ^ ")"
             in
             let decl = Option.map (fun s -> "type " ^ head ^ " = " ^ s) (checked_print g) in
             (rw body, List.map (fun (m, ps, t) -> (m, ps, rw t)) old_defs, decls @ [decl]) )
          (body, old_defs, []) globals
      in
      if List.mem None decls then
        Error "A shared record cannot be printed back as the same type."
      else
        let decls = List.map Option.get decls in
        let tvars =
          ref
            (List.filter_map
               (function
                 | TVar (_, n) -> Some n
                 | _ -> None )
               (subterms t) )
        in
        let fresh_tvar () =
          let rec go i =
            let n =
              String.make 1 (Char.chr (97 + (i mod 26)))
              ^ if i < 26 then "" else string_of_int (i / 26)
            in
            if List.mem n !tvars then go (i + 1) else n
          in
          let n = go 0 in
          tvars := n :: !tvars ;
          n
        in
        let chosen = List.sort (fun (a, _) (b, _) -> compare (size a) (size b)) chosen in
        let _, body, old_defs, new_defs =
          List.fold_left
            (fun (applied, body, old_defs, new_defs) (g, name) ->
               let g = applied g in
               let k = holes_in g in
               let params, def, by =
                 if plumbing && k > 0 then
                   let params = List.init k (fun _ -> fresh_tvar ()) in
                   let rest = ref params in
                   let rec number t =
                     if is_hole t then
                       match
                         !rest
                       with
                       | p :: tl ->
                           rest := tl ;
                           TVar (KNoInfer, p)
                       | [] -> t
                     else
                       map_children number t
                   in
                   let def = number g in
                   (params, def, TApp (name, List.init k (fun _ -> TVar (KInfer, "_hole"))))
                 else
                   ([], g, TCustom name)
               in
               let sub = replace ~what:g ~by in
               let sub_def (m, ps, t) = (m, ps, sub t) in
               ( (fun t -> sub (applied t)),
                 sub body,
                 List.map sub_def old_defs,
                 List.map sub_def new_defs @ [(name, params, def)] ) )
            (Fun.id, body, old_defs, []) chosen
        in
        let defs = old_defs @ new_defs in
        match checked_print (if defs = [] then body else TWhere (body, defs)) with
        | Some s -> Ok (s, decls)
        | None -> Error "The result cannot be printed back as the same type." )

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
