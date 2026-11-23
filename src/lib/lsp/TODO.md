# To do

- Wildcard locations: the parser locates every part of a type (`as_typ`),
  so a `_` is a single `TLoc (loc, TVar (KInfer, "_holeN"))`.
  `tyexpr_wildcards` and `tyexpr_tvars` in `typings.ml` should match that
  shape only, not also reach it through a second `TLoc` around it.
- Wildcards in type arguments: `mlsem/wildcards` finds none in
  `val f : int -> t(_)`.
- The toolkit's printer (`type_refactor.ml`) is trusted only through a
  runtime check: a rewrite is offered when its printed text parses back to
  the same type, and silently dropped otherwise. Nothing shows the printer
  agrees with the parser. Either prove it (a printer correct by
  construction, with parentheses from precedence, and the round trip tested
  on the fixtures and on generated types), or handle a disagreement: report
  it (log it, tell the user the rewrite failed) instead of dropping the
  rewrite without a word. Also decide whether the rewrites should work on
  the syntax tree directly, with one printer at the end.
- `lsp` and `jsonrpc` are capped below 1.24 (`dune-project`): later versions
  make `TextDocumentContentChangeEvent` a variant
  (`` `TextDocumentContentChangePartial `` with a range,
  `` `TextDocumentContentChangeWholeDocument ``), which `Doc.apply_change`
  does not handle yet. Port and lift the cap.
