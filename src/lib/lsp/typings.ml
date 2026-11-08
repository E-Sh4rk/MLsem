(* Types of variables and calls, as plain data for hover and highlighting.

   While a definition is checked, [Main] records the type of every variable
   occurrence, binder and call ([Main.typing]), once per derivation: a
   definition with several [val] lines is checked once per line. This module
   turns those records into plain data (byte ranges, strings, integers) that
   the supervisor keeps and answers from, without the engine.

   Type variables are identified by integers, assigned per document. The
   builder shares variable objects by name, so ['a] in two [val] lines can be
   one variable: highlighting matches by this identity, within one
   definition. Only rigid variables (written by the user) get ids: those are
   the ones a [val] line can show. *)

open Mlsem_common
open Mlsem_types
open Mlsem_app

(* A variable occurrence or binder, with its hover text: one line per
   derivation when they differ (labelled with their [val]), then their
   union. *)
type hover = {
  h_start: int;
  h_end: int;
  lines: string list;
}

(* An occurrence of a type variable in a [val] line. *)
type tvar_occ = {
  o_start: int;
  o_end: int;
  tvar: int;
}

(* Something in a definition's body whose type mentions type variables: a
   variable occurrence or binder, a call argument, or a whole call. *)
type mark = {
  m_start: int;
  m_end: int;
  tvars: int list;
}

type defn = {
  tvar_occs: tvar_occ list;
  marks: mark list;
}

(* A call site [f a1 ... an]. [group] is shared by the calls of the same
   callee whose argument types are equivalent. *)
type call = {
  c_start: int;
  c_end: int;
  callee: (string * int) option;  (** name and definition offset *)
  sig_index: int option;
  args: (int * int * string) list;
  result: string;
  instance: (string * string) list;  (** callee's type variable := type *)
  group: int;
}

type t = {
  hovers: hover list;
  defns: defn list;
  calls: call list;
}

let empty = {hovers = []; defns = []; calls = []}

(* Per-document state while converting. *)
type ctx = {
  text: string;
  mutable tvar_ids: (TVar.t * int) list;
  mutable groups: ((string * int) option * GTy.t list * int) list;
  mutable acc: t;
}

let create text = {text; tvar_ids = []; groups = []; acc = empty}
let finish ctx = ctx.acc

let tvar_id ctx v =
  match List.find_opt (fun (v', _) -> TVar.equal v v') ctx.tvar_ids with
  | Some (_, i) -> i
  | None ->
      let i = List.length ctx.tvar_ids in
      ctx.tvar_ids <- (v, i) :: ctx.tvar_ids ;
      i

let tvar_ids ctx ty =
  GTy.fv ty |> MVarSet.proj1 |> TVarSet.elements
  |> List.filter (TVar.has_kind KNoInfer)
  |> List.map (tvar_id ctx)
  |> List.sort_uniq compare

let var_name v = TVar.prefix v ^ TVar.name v

let range pos =
  if pos = Position.dummy then
    None
  else
    Some
      ( Position.offset (Position.start_of_position pos),
        Position.offset (Position.end_of_position pos) )

(* The range of a variable occurrence or binder, if it is the variable's name
   in the source: generated code (pattern matching, loops, mutable variables
   are desugared) either has no location or points at something else. *)
let name_range ctx v pos =
  match (range pos, Variable.get_name v) with
  | Some (s, e), Some name
    when e - s = String.length name
         && e <= String.length ctx.text
         && String.sub ctx.text s (e - s) = name ->
      Some (s, e)
  | _ -> None

let is_word c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '\''

(* A binder's location spans its whole binding ([f x = ...] for the parameter
   [x], [y :: rest -> ...] for a pattern variable); the binder is the first
   whole-word occurrence of its name in there. *)
let binder_range ctx v =
  match (range (Variable.get_location v), Variable.get_name v) with
  | Some (s, e), Some name when name <> "" ->
      let text = ctx.text in
      let n = String.length name and e = min e (String.length text) in
      let rec find i =
        if i + n > e then
          None
        else if
          String.sub text i n = name
          && (i = 0 || not (is_word text.[i - 1]))
          && (i + n >= String.length text || not (is_word text.[i + n]))
        then
          Some (i, i + n)
        else
          find (i + 1)
      in
      find s
  | _ -> None

(* Type variable tokens (['a], ['_a]) in [text] from [start], skipping
   character literals (['a']). *)
let tvar_tokens text start stop =
  let is_id c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'
  in
  let rec scan i acc =
    if i >= stop then
      List.rev acc
    else if text.[i] = '\'' && i + 1 < stop && is_id text.[i + 1] then (
      let j = ref (i + 1) in
      while !j < stop && is_id text.[!j] do
        incr j
      done ;
      if !j < stop && text.[!j] = '\'' then
        scan (!j + 1) acc
      else
        scan !j ((i, !j, String.sub text i (!j - i)) :: acc) )
    else
      scan (i + 1) acc
  in
  scan start []

(* Occurrences of the overload's type variables in its [val] line. *)
let val_tvar_occs ctx overload (s, e) =
  let vars = Signature.to_gty overload |> GTy.fv |> MVarSet.proj1 |> TVarSet.elements in
  let colon =
    try String.index_from ctx.text s ':' with
    | Not_found -> e
  in
  tvar_tokens ctx.text (min colon e) e
  |> List.filter_map (fun (o_start, o_end, name) ->
    List.find_opt (fun v -> var_name v = name) vars
    |> Option.map (fun v -> {o_start; o_end; tvar = tvar_id ctx v}) )

let group ctx callee args =
  let same (c, args', _) =
    c = callee && List.length args = List.length args' && List.for_all2 GTy.equiv args args'
  in
  match (callee, List.find_opt same ctx.groups) with
  | Some _, Some (_, _, g) -> g
  | _ ->
      let g = List.length ctx.groups in
      ctx.groups <- (callee, args, g) :: ctx.groups ;
      g

let add ctx envs (b : Main.inferred) =
  let display = Main.display_gty envs in
  let sig_texts = Main.signature envs b.sigs in
  let label = function
    | Some i when b.declared -> (
        match (Variable.get_name b.var, List.nth_opt sig_texts i) with
        | Some name, Some t -> Some ("val " ^ name ^ " : " ^ t)
        | _ -> None )
    | _ -> None
  in
  (* Hover types: per position, per derivation, the union of the types seen
     there (a node can be typed several times in one derivation). *)
  let at = Hashtbl.create 16 in
  let note (s, e) sig_index ty =
    let per_sig = Option.value (Hashtbl.find_opt at (s, e)) ~default:[] in
    let per_sig =
      match List.assoc_opt sig_index per_sig with
      | Some ty' -> (sig_index, GTy.cup ty ty') :: List.remove_assoc sig_index per_sig
      | None -> (sig_index, ty) :: per_sig
    in
    Hashtbl.replace at (s, e) per_sig
  in
  let marks = ref [] in
  let mark (m_start, m_end) ty =
    match tvar_ids ctx ty with
    | [] -> ()
    | tvars -> marks := {m_start; m_end; tvars} :: !marks
  in
  let calls = ref [] in
  b.derivations
  |> List.iter (fun (d : Main.derivation) ->
    d.typings
    |> List.iter (function
      | Main.TUse (pos, v, ty) ->
          Option.iter (fun r -> note r d.sig_index ty ; mark r ty) (name_range ctx v pos)
      | TBinder (v, ty) when not (Variable.equal v b.var) ->
          Option.iter (fun r -> note r d.sig_index ty ; mark r ty) (binder_range ctx v)
      | TBinder _ -> ()
      | TCall {pos; callee; args; result} -> (
          match range pos with
          | None -> ()
          | Some (c_start, c_end) ->
              mark (c_start, c_end) result ;
              let args =
                List.filter_map
                  (fun (p, ty) -> Option.map (fun r -> mark r ty ; (r, ty)) (range p))
                  args
              in
              let callee_key =
                Option.bind callee (fun (v, _) ->
                  match (Variable.get_name v, range (Variable.get_location v)) with
                  | Some name, Some (s, _) -> Some (name, s)
                  | Some name, None -> Some (name, -1)
                  | None, _ -> None )
              in
              let instance =
                match callee with
                | None -> []
                | Some (_, s) ->
                    Subst.bindings1 s |> List.map (fun (v, ty) -> (var_name v, display (GTy.mk ty)))
              in
              calls :=
                {
                  c_start;
                  c_end;
                  callee = callee_key;
                  sig_index = d.sig_index;
                  args = List.map (fun ((s, e), ty) -> (s, e, display ty)) args;
                  result = display result;
                  instance;
                  group = group ctx callee_key (List.map snd args);
                }
                :: !calls ) ) ) ;
  let hovers =
    Hashtbl.fold
      (fun (h_start, h_end) per_sig hovers ->
         let per_sig = List.sort (fun (i, _) (j, _) -> compare i j) per_sig in
         let texts = List.map (fun (i, ty) -> (i, display ty)) per_sig in
         let lines =
           match List.sort_uniq compare (List.map snd texts) with
           | [t] -> [t]
           | _ ->
               let per =
                 List.map
                   (fun (i, t) ->
                      match label i with
                      | Some l -> t ^ "    — " ^ l
                      | None -> t )
                   texts
               in
               per @ ["union: " ^ display (GTy.disj (List.map snd per_sig))]
         in
         {h_start; h_end; lines} :: hovers )
      at []
  in
  (* The defined name itself: its final type, as in the lens. *)
  let hovers =
    match (binder_range ctx b.var, Variable.get_name b.var) with
    | Some (h_start, h_end), Some name ->
        {h_start; h_end; lines = [name ^ " : " ^ Main.display envs b.ty]} :: hovers
    | _ -> hovers
  in
  let tvar_occs =
    if b.declared then
      let locs = Variable.get_sig_locations b.var |> List.rev |> List.filter_map range in
      List.concat
        (List.mapi
           (fun i loc ->
              match List.nth_opt b.sigs i with
              | Some o -> val_tvar_occs ctx o loc
              | None -> [] )
           locs )
    else
      []
  in
  ctx.acc <-
    {
      hovers = hovers @ ctx.acc.hovers;
      defns = {tvar_occs; marks = !marks} :: ctx.acc.defns;
      calls = List.rev_append !calls ctx.acc.calls;
    }
