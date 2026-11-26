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
- Hovers of a recursive function inside its own body show the open type the
  recursion was checked with, besides the overloads: in
  `paper/running/1_top_down_find.mlsem` (MLsem-dummy-project), `find` in
  `find l k` is `(Node(x1, int, 'a, x1) -> int -> Nil | 'a) & (Leaf -> any
  -> Nil) | 'b & (x1 -> int -> Nil | 'a) where ...`. Hide the `'b & (...)`
  part, or explain it in the hover.
- Print declared types by name: the same hover writes the tree as
  `x1 where x1 = Leaf | Node(x1, int, 'a, x1)` though the file declares
  `type tree('a) = Leaf | Node(tree('a), int, 'a, tree('a))`. Hovers and
  code lenses should fold it to `tree('a)` automatically, as the toolkit's
  Simplify offers it ("declared type").
