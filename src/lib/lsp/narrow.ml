(* The toolkit's "Narrow": replacing parts of [val] lines' types with
   narrower types, the mirror of Widen.

   The narrower types of a part (each is a subtype of the part, and the part
   is not a subtype of it), widest first:
   - each declared type contained in the part, a declared type with
     parameters applied to the widest arguments that keep it contained
     ([tree('a)] in [x1 where x1 = Leaf | Node(x1, int, 'a, any)]), found by
     tallying ([Type_parts.applied]);
   - when the part is a union, written or once its declared type or [where]
     name is unfolded ([tree(string)] is [Leaf | Node(tree(string), int,
     string, tree(string))]), each member alone and, with more than two
     members, each union of all members but one. A sequence type counts as
     the union of its cases ([[ t* ]] is [[ t+ ] | []], see [members]).
   No narrower types are offered for literals, type variables, [empty],
   [any] (too many to choose from) or on a line with wildcards.

   A narrower type is a guess: in a result it promises more, in an argument
   it accepts fewer inputs, which a recursive call may still need. So each
   is checked: the line with the part replaced, the definition's other
   [val] lines unchanged, is checked against the definition as
   [mlsem/checkSignature] does, and the option is labelled "checks" or
   "fails", with the first error. That is one typecheck of the source up to
   the definition per option: past a time [budget], the remaining options
   are labelled "unchecked", and [mlsem/narrowCheck] checks one on demand.
   Options in a [where] definition are not offered: narrowing one narrows
   every use of its name. *)

open Mlsem_app
open Mlsem_types
open TyExpr
open Yojson.Safe.Util

(* The type definitions of [program]: name, parameters, body. *)
let type_defs (program : PAst.program) =
  List.concat_map
    (fun (_, elem) ->
       match elem with
       | PAst.Types ts -> ts
       | _ -> [] )
    program

(* [t] with the parameters [ps] replaced by [args]. *)
let rec instantiate ps args t =
  match t with
  | TVar (_, n) when List.mem_assoc n (List.combine ps args) -> List.assoc n (List.combine ps args)
  | t -> Type_refactor.map_children (instantiate ps args) t

(* [t] with its outer name unfolded, as long as it is one: a [where] name of
   the line, or a declared type of the file. *)
let rec unfold wheres decls fuel t =
  let def n args =
    match List.find_opt (fun (n', ps, _) -> n' = n && List.length ps = List.length args) wheres with
    | Some (_, ps, body) -> Some (instantiate ps args body)
    | None -> (
        match
          List.find_opt (fun (n', ps, _) -> n' = n && List.length ps = List.length args) decls
        with
        | Some (_, ps, body) -> Some (instantiate ps args body)
        | None -> None )
  in
  let named =
    match t with
    | TCustom n -> def n []
    | TApp (n, args) -> def n args
    | _ -> None
  in
  match named with
  | Some t' when fuel > 0 -> unfold wheres decls (fuel - 1) (Type_refactor.strip t')
  | _ -> t

(* The members of a union, a sequence type counting as the union of its
   cases: [[ t* ]] is [[ t+ ] | []], [[ t? ]] is [[ t ] | []], and a sequence
   of alternatives is the union of a sequence of each. The non-empty case
   comes first, as it is the one that more often checks. *)
let rec members = function
  | TCup (a, b) -> members a @ members b
  | TSList (Star r) -> [TSList (Plus r); TSList Epsilon]
  | TSList (Option r) -> [TSList r; TSList Epsilon]
  | TSList (Union rs) -> List.map (fun r -> TSList r) rs
  | t -> [t]

let union = function
  | [] -> TBase TEmpty
  | t :: ts -> List.fold_left (fun acc t -> TCup (acc, t)) t ts

(* Parts nothing is offered for: literals, variables, [empty], and those
   with too many narrower types to choose from, [any], a negation and the
   types of all functions, tuples, records, tags or enums. *)
let skipped = function
  | TDyn
  | TVar _
  | TRowVar _
  | TOption _
  | TNeg _
  | TBase
      ( TInt (Some _, _)
      | TInt (_, Some _)
      | TCharInt _ | TSString _ | TTrue | TFalse | TUnit | TNil | TAny | TEmpty | TArrowAny
      | TTupleAny | TTupleN _ | TRecordAny | TTagAny | TEnumAny ) ->
      true
  | _ -> false

let has_wildcards line =
  List.exists
    (function
      | TVar (KInfer, _) -> true
      | _ -> false )
    (Type_refactor.subterms (Type_refactor.strip line))

(* Whether [p] is in a [where] definition of [line]: narrowing it would
   narrow every use of the name, and the definition itself where it
   recurses (a narrower [x1] in [x1 = Leaf | Node(x1)] can make it empty,
   which checks for nothing). *)
let in_definition line (p : Type_refactor.part) =
  let s, e = p.span in
  Type_refactor.located_parts line
  |> List.exists (fun (d : Type_refactor.part) ->
    match d.parent with
    | Some (TWhere _, i) when i >= 1 -> fst d.span <= s && e <= snd d.span
    | _ -> false )

(* The narrower types of the located part [p] of the [val] type [line], as
   written in its place; [decls] are the file's type definitions. *)
let narrower decls envs line (p : Type_refactor.part) =
  let node = Type_refactor.strip p.node in
  if skipped node || has_wildcards line || in_definition line p then
    None
  else
    let build = Type_parts.build envs line in
    match build p.node with
    | exception _ -> None
    | part_ty when Ty.is_empty part_ty || Ty.is_any part_ty -> None
    | part_ty -> (
        let wheres =
          match Type_refactor.strip line with
          | TWhere (_, defs) -> defs
          | _ -> []
        in
        let declared = Type_parts.declared ~narrower:true envs build part_ty in
        let ms = members (unfold wheres decls 10 node) in
        let ms = List.fold_left (fun acc m -> if List.mem m acc then acc else acc @ [m]) [] ms in
        let unions =
          match ms with
          | []
          | [_] ->
              []
          | [_; _] -> ms
          | _ -> ms @ List.mapi (fun i _ -> union (List.filteri (fun j _ -> j <> i) ms)) ms
        in
        (* Declared types first: of two equivalent candidates the first is
           kept, and a declared name reads better than its structure. *)
        let narrower =
          List.fold_left
            (fun acc c ->
               match build c with
               | c_ty
                 when Ty.non_empty c_ty && Ty.leq c_ty part_ty
                      && (not (Ty.leq part_ty c_ty))
                      && not (List.exists (fun (_, t) -> Ty.equiv t c_ty) acc) ->
                   acc @ [(c, c_ty)]
               | _ -> acc
               | exception _ -> acc )
            [] (declared @ unions)
        in
        (* Widest first: by how many of the others are narrower. *)
        let wider_than (_, t) = List.length (List.filter (fun (_, t') -> Ty.leq t' t) narrower) in
        let sorted = List.stable_sort (fun a b -> compare (wider_than b) (wider_than a)) narrower in
        match List.map (fun (c, _) -> (Type_parts.written p c, None)) sorted with
        | [] -> None
        | os -> Some os )

(* The first error, on one line and short, for a label. *)
let short = function
  | `String s :: _ ->
      let s = String.concat " " (String.split_on_char '\n' s) in
      if String.length s > 160 then String.sub s 0 157 ^ "..." else s
  | _ -> ""

(* The label of [o_text] written at [s, e) of [text] (in a [val] line of
   [t]): the definition of [t] checked with it, the other [val] lines
   unchanged. *)
let label text (t : Signature_check.target) (s, e) o_text =
  let decls =
    t.vals
    |> List.map (fun (vs, ve) ->
      if vs <= s && e <= ve then
        String.sub text vs (s - vs) ^ o_text ^ String.sub text e (ve - e)
      else
        String.sub text vs (ve - vs) )
    |> String.concat ""
  in
  match Signature_check.check_decls text t decls with
  | _, [] -> [("label", `String "checks")]
  | _, errors -> [("label", `String "fails"); ("error", `String (short errors))]

(* Seconds of checking for one request: the options after that are labelled
   "unchecked", for [mlsem/narrowCheck] to check one when it is picked. A
   check typechecks the source up to the definition, which in a long file
   can take seconds. *)
let budget = 2.0

(* Each option of [result] (from [Type_parts.lines]) labelled by checking
   the definition of [t] with the option in place of its part. *)
let check text offsets (t : Signature_check.target) result =
  let until = Unix.gettimeofday () +. budget in
  let check_option line_start (s, e) o =
    let o_text = o |> member "text" |> to_string in
    let fields =
      if Unix.gettimeofday () < until then
        label text t (line_start + s, line_start + e) o_text
      else
        [("label", `String "unchecked")]
    in
    `Assoc (("text", `String o_text) :: fields)
  in
  let part line_start p =
    let span = (p |> member "start" |> to_int, p |> member "end" |> to_int) in
    match p with
    | `Assoc fields ->
        `Assoc
          (List.map
             (function
               | "options", `List os ->
                   ("options", `List (List.map (check_option line_start span) os))
               | f -> f )
             fields )
    | p -> p
  in
  let line l =
    match l with
    | `Assoc fields ->
        let line_start = offsets.(l |> member "line" |> to_int) in
        `Assoc
          (List.map
             (function
               | "parts", `List ps -> ("parts", `List (List.map (part line_start) ps))
               | f -> f )
             fields )
    | l -> l
  in
  match result with
  | `Assoc fields ->
      `Assoc
        (List.map
           (function
             | "lines", `List ls -> ("lines", `List (List.map line ls))
             | f -> f )
           fields )
  | r -> r

(* mlsem/narrow: the [val] lines of the definition at [position] with their
   narrowable parts and the narrower types of each ([Type_parts.lines]),
   each labelled "checks" or "fails" (with an [error]) against the
   definition; unlabelled for [val] lines without a definition. *)
let parts params : Yojson.Safe.t =
  let decls =
    match Main.parse (`String (params |> member "text" |> to_string)) with
    | PSuccess program -> type_defs program
    | PFailure _
    | (exception _) ->
        []
  in
  match Type_parts.lines params (narrower decls) with
  | `Assoc (("ok", `Bool true) :: _) as result ->
      Signature_check.with_target params (fun text offsets t ->
        if t.body then check text offsets t result else result )
  | result -> result

(* mlsem/narrowCheck: the label of one option, [option] written at columns
   [start, end) of line [line], as [mlsem/narrow] gives it. *)
let check_one params : Yojson.Safe.t =
  Signature_check.with_target params (fun text offsets t ->
    if not t.body then
      Signature_check.error "No definition here, only val lines."
    else
      let line_start = offsets.(params |> member "line" |> to_int) in
      let span =
        ( line_start + (params |> member "start" |> to_int),
          line_start + (params |> member "end" |> to_int) )
      in
      `Assoc (("ok", `Bool true) :: label text t span (params |> member "option" |> to_string)) )
