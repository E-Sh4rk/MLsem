"""bal.mlsem with its val line replaced by variants with wildcards: the
native typecheck's time and result for each, then, with --lsp, the language
server's Partial signatures for those the native check finishes.

Run from the repository root after `dune build`:
    python src/lib/lsp/experiments/bal_holes/run.py [--lsp] [--skip-timeouts]
--lsp needs the MLsem-lsp-tests checkout next to this repository (for its
LSP client). --skip-timeouts leaves out the variants that time out natively.
The variant files are written to a temporary directory."""

import pathlib
import re
import sys
import tempfile
import time

import sh

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[4]
SRC = (HERE / "bal.mlsem").read_text()
VAL = "val bal : t('a) -> Key -> 'a -> t('a) -> t('a)\n"
assert VAL in SRC
LIMIT = 60
native = sh.Command(str(ROOT / "_build/default/src/bin/native.exe")).bake(
    _tty_out=False, _ok_code=range(256), _timeout=LIMIT)

VARIANTS = {
    "full": VAL,
    "no val": "",
    "result _": "val bal : t('a) -> Key -> 'a -> t('a) -> _\n",
    "l _": "val bal : _ -> Key -> 'a -> t('a) -> t('a)\n",
    "x _": "val bal : t('a) -> _ -> 'a -> t('a) -> t('a)\n",
    "d _": "val bal : t('a) -> Key -> _ -> t('a) -> t('a)\n",
    "r _": "val bal : t('a) -> Key -> 'a -> _ -> t('a)\n",
    "d, result _": "val bal : t('a) -> Key -> _ -> t('a) -> _\n",
    "domains _": "val bal : _ -> _ -> _ -> _ -> t('a)\n",
    "all _": "val bal : _ -> _ -> _ -> _ -> _\n",
}
if "--skip-timeouts" in sys.argv:
    for k in ["no val", "l _", "r _", "domains _", "all _"]:
        del VARIANTS[k]

out = pathlib.Path(tempfile.mkdtemp(prefix="bal_holes_"))
results = {}
for name, line in VARIANTS.items():
    f = out / f"bal_{re.sub(r'[^a-z]+', '_', name)}.mlsem"
    f.write_text(SRC.replace(VAL, line))
    t = time.time()
    try:
        res = str(native(str(f)))
        dt = time.time() - t
        m = re.search(r"^bal: (.*?) \((checked|failed)", res, re.M)
        res = m.group(1) if m else res.strip().splitlines()[-1]
        if m and m.group(2) == "failed":
            res = "FAILED: " + res
    except sh.TimeoutException:
        dt, res = float(LIMIT), f"TIMEOUT ({LIMIT} s)"
    results[name] = (f, line, dt, res)
    print(f"{name:12} native {dt:5.2f}s  {res}", flush=True)

if "--lsp" not in sys.argv:
    sys.exit(0)

sys.path.insert(0, str(ROOT.parent / "MLsem-lsp-tests"))
from lspclient import Client, initialize, is_diag, open_doc  # noqa: E402

print()
c = Client(log_name="bal_holes.log")
try:
    initialize(c)
    for name, (f, line, dt, res) in results.items():
        if "_" not in line.split(":", 1)[-1] or res.startswith(("TIMEOUT", "FAILED")):
            continue
        uri, text = open_doc(c, str(f))
        t = time.time()
        _, d = c.wait_for(is_diag(uri), timeout=120)
        tdiag = time.time() - t
        errs = [x["message"].splitlines()[0] for x in d["params"]["diagnostics"]
                if x.get("severity", 1) == 1]
        if errs:
            print(f"{name:12} LSP errors: {errs}")
        i = text.split("\n").index(line.rstrip("\n"))
        t = time.time()
        r = c.response(c.request("mlsem/wildcards", {
            "textDocument": {"uri": uri}, "position": {"line": i, "character": 4}}), 120)
        th = time.time() - t
        if "result" not in r:
            print(f"{name:12} LSP typecheck {tdiag:5.2f}s  ERROR {r['error']['message']}")
        else:
            vals = [" / ".join(x["value"] for x in w["rows"]) for w in r["result"]["wildcards"]]
            print(f"{name:12} LSP typecheck {tdiag:5.2f}s wildcards {th:.2f}s  {vals}")
finally:
    c.cleanup()
