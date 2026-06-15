(* Handlers for the custom [mlsem/*] JSON-RPC requests backing the VS Code
   overload-merge webview. Each takes the request's [params] as raw JSON and
   returns the JSON result; [Server] wires them into the dispatch loop.

   Merge operates on the cached [Signature.t] values (not the rendered
   strings), so every handler re-resolves the binding under the cursor from
   [Store] and renders/merges through the typecheck's cached [envs]. Ambient
   global config may have been mutated by an intervening typecheck of another
   document, so we [Config.restore_all ()] before rendering or merging — the
   cached [envs]/[Signature.t] are immutable and safe to reuse, the global
   config is not. *)

open Mlsem_app
open Yojson.Safe.Util

(* {textDocument:{uri}, position} → (uri, position), via the standard LSP
   decoders so we inherit their tolerance and uri normalisation. *)
let decode_target params =
  let td = params |> member "textDocument" |> Lsp.Types.TextDocumentIdentifier.t_of_yojson in
  let pos = params |> member "position" |> Lsp.Types.Position.t_of_yojson in
  (td.uri, pos)

let decode_indices params = params |> member "indices" |> to_list |> List.map to_int

(* The client's current buffer text, sent with mutating/preview requests so the
   server acts on exactly what the editor shows. Absent for cursor-follow. *)
let decode_text params =
  match member "text" params with
  | `String s -> Some s
  | _ -> None

(* Bring the cache in line with the request before resolving the binding:
   prefer the client-sent text (race-free), else re-typecheck the cached
   buffer if it has drifted since the last typecheck. *)
let sync params uri =
  match decode_text params with
  | Some text -> Store.sync_text uri text
  | None -> Store.ensure_fresh uri

let not_found =
  `Assoc
    [
      ("found", `Bool false); ("name", `String ""); ("declared", `Bool false);
      ("overloads", `List []);
    ]

let error msg = `Assoc [("ok", `Bool false); ("error", `String msg)]

(* Render and merge a chosen subset of overloads. [None] for an empty
   selection — [Signature.merge []] is ill-defined. *)
let merge_selected envs (sigs : Signature.t) indices : string option =
  Config.restore_all () ;
  match List.filter_map (List.nth_opt sigs) indices with
  | [] -> None
  | selected -> (
      match Main.signature envs [Signature.merge selected] with
      | [s] -> Some s
      | _ -> assert false (* one overload in → exactly one rendered string *) )

(* mlsem/overloads: the binding under the cursor and its overloads, each
   rendered to [val]-compatible surface syntax with its index. *)
let overloads params : Yojson.Safe.t =
  try
    let uri, pos = decode_target params in
    Store.ensure_fresh uri ;
    match (Store.find_binding_at uri pos, Store.envs_of uri) with
    | Some b, Some envs ->
        Config.restore_all () ;
        let texts = Main.signature envs b.sigs in
        `Assoc
          [
            ("found", `Bool true); ("name", `String b.name); ("declared", `Bool b.declared);
            ( "overloads",
              `List (List.mapi (fun i t -> `Assoc [("index", `Int i); ("text", `String t)]) texts)
            );
          ]
    | _ -> not_found
  with
  | _ -> not_found

(* mlsem/mergePreview: the merged type of the selected overloads as the
   one-line [val name : ...] the apply would write. *)
let merge_preview params : Yojson.Safe.t =
  try
    let uri, pos = decode_target params in
    let indices = decode_indices params in
    sync params uri ;
    match (Store.find_binding_at uri pos, Store.envs_of uri) with
    | Some b, Some envs -> (
        match merge_selected envs b.sigs indices with
        | None -> error "Select at least one overload."
        | Some merged ->
            `Assoc [("ok", `Bool true); ("text", `String ("val " ^ b.name ^ " : " ^ merged))] )
    | _ -> error "No binding at this position."
  with
  | e -> error (Printexc.to_string e)

(* mlsem/applyMerge: the workspace edit that writes the merged declaration —
   replacing existing [val] line(s) or inserting above the binder. Returned
   for the extension to apply via [vscode.workspace.applyEdit]. *)
let apply_merge params : Yojson.Safe.t =
  try
    let uri, pos = decode_target params in
    let indices = decode_indices params in
    sync params uri ;
    match (Store.find_binding_at uri pos, Store.envs_of uri) with
    | Some b, Some envs -> (
        match merge_selected envs b.sigs indices with
        | None -> error "Select at least one overload."
        | Some merged ->
            let range, newText = Store.merge_edit uri b ~name:b.name ~merged in
            `Assoc
              [
                ("ok", `Bool true);
                ( "edit",
                  `Assoc
                    [
                      ("uri", Lsp.Types.DocumentUri.yojson_of_t uri);
                      ("range", Lsp.Types.Range.yojson_of_t range); ("newText", `String newText);
                    ] );
              ] )
    | _ -> error "No binding at this position."
  with
  | e -> error (Printexc.to_string e)
