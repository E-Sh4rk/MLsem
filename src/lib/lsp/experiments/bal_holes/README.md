# Draft holes in `bal`: does a hole help inference?

The question: `bal` (rebalancing an AVL tree, from OCaml's `Map`) does not
typecheck in time without a signature. Does a draft signature, the full one
with some parts replaced by `_`, let MLsem infer the rest in time, and what do
the holes resolve to?

- `bal.mlsem`: the program. The LSP tests' fixture `fixtures/bal.ml`, without
  its commented-out signatures.
- `run.py`: replaces the `val bal` line by each variant below, runs
  `native.exe` on it with a 60 s limit, and with `--lsp` asks the language
  server for the Draft holes (`mlsem/holes`) of the variants that finish.

```
python src/lib/lsp/experiments/bal_holes/run.py --lsp
```

## Results

Measured on 2026-11-15, `lsp` at 6466408 (core unchanged since 153bbe9),
on the author's laptop. Times are wall clock, including startup.

The full signature, for reference:

```
val bal : t('a) -> Key -> 'a -> t('a) -> t('a)
```

| `val bal :` | native | type | LSP holes |
|---|---|---|---|
| `t('a) -> Key -> 'a -> t('a) -> t('a)` | 0.40 s | full | (no holes) |
| (no `val` line) | timeout | | |
| `t('a) -> Key -> 'a -> t('a) -> _` | 0.41–0.51 s | poor | `Node()` |
| `_ -> Key -> 'a -> t('a) -> t('a)` | timeout | | |
| `t('a) -> _ -> 'a -> t('a) -> t('a)` | 0.70–0.81 s | full | `Key` |
| `t('a) -> Key -> _ -> t('a) -> t('a)` | 0.71–0.81 s | full | `'a` |
| `t('a) -> Key -> 'a -> _ -> t('a)` | timeout | | |
| `t('a) -> Key -> _ -> t('a) -> _` | 0.50 s | poor | `any`, `Node()` |
| `_ -> _ -> _ -> _ -> t('a)` | timeout | | |
| `_ -> _ -> _ -> _ -> _` | timeout | | |

- **full**: `∀'a. t('a) -> Key -> 'a -> t('a) -> t('a)`, the full signature.
- **poor**: `x1 -> Key -> any -> x1 -> Node()` where
  `x1 = Nil | Node(x1, Key, any, x1, int)`. Valid, but `'a` is lost, and the
  arguments are trees of `any`.
- **timeout**: `native.exe` did not finish in 60 s. The LSP was not asked.

In the LSP, each typecheck took 0.6–0.9 s and each Draft holes request under
0.01 s.

## Findings

- **A hole helps only where it is cheap to infer:** `Key` or the data `'a`.
  Inference then takes about twice as long as with the full signature.
- **A hole in a tree argument (`l` or `r`) is as bad as no signature:** it
  times out, alone or with every domain a hole.
- **A hole in the result finishes but loses precision:** the result becomes
  `Node()` and the tree arguments lose `'a`. With the result fixed to
  `t('a)` the arguments' `t('a)` survive. Open question for the core: why the
  result hole does not get `t('a)`.
- **The holes show what MLsem inferred, not what the user meant.** For the
  poor variants, Draft holes faithfully show `Node()` and `any`.

## Bug found and fixed

The first run returned no holes in the LSP for every variant. Every file
with `t('a)` in a draft line failed with *internal error:
Stdlib.Effect.Unhandled(Mlsem_types__Base.PEnv.Get)*. Building a parametric
alias performs `PEnv.Get`, which main.ml handles per element, but the LSP
built `val` types outside that handler. Fixed in 8c7e7e3:
`Typings.build_signature` builds under the handler, used by Draft holes,
widen, simplify and overload merging. The table above is after the fix.
