(* Cache of the most recent typecheck result per document, populated on
   didOpen and didSave and read back by [textDocument/codeLens].

   Each entry also remembers the digest of the source it was computed from,
   so a save with unchanged content can skip re-running the typechecker. *)

module Uri = Lsp.Types.DocumentUri

type entry = {
  digest: Digest.t;
  result: Typecheck.result;
}

let entries : (Uri.t, entry) Hashtbl.t = Hashtbl.create 16

let digest_of_text = Digest.string

(* Returns [true] iff [text] matches what we last typechecked for [uri]. *)
let is_cached uri text =
  match Hashtbl.find_opt entries uri with
  | Some e -> Digest.equal e.digest (digest_of_text text)
  | None -> false

let set uri ~text ~result = Hashtbl.replace entries uri {digest = digest_of_text text; result}

let result uri =
  match Hashtbl.find_opt entries uri with
  | Some e -> e.result
  | None -> Typecheck.empty

let remove uri = Hashtbl.remove entries uri
