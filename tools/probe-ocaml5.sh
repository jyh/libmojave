#!/usr/bin/env bash
# Compile-probe libmojave against the active OCaml (bytecode type-check only).
# Copies sources to a scratch dir, compiles modules in OMake Files order
# (.mli then .ml), warnings off, and reports PASS/FAIL per file with the
# first error line. C files are compiled with the OCaml headers.
# Usage: tools/probe-ocaml5.sh OUTDIR
# LIMITS: Unix branches only; warnings off; nothing is RUN.
set -u
src=$(cd "$(dirname "$0")/.." && pwd)
out=${1:?usage: probe-ocaml5.sh OUTDIR}
mkdir -p "$out/ml" "$out/c"
for d in stdlib system util unix; do cp "$src/$d"/*.ml "$src/$d"/*.mli "$out/ml/" 2>/dev/null; done
cp "$src"/cutil/*.c "$src"/cutil/*.h "$out/c/"
listed=$(for d in stdlib system util unix; do sed -n 's/^ *\(lm_[a-z0-9_]*\) *$/\1/p' "$src/$d/Files"; done)
# dependency order from ocamldep, restricted to modules OMake builds
order=$(cd "$out/ml" && ocamldep -sort *.mli *.ml)
mods=$(for f in $order; do b=${f%.*}; echo "$listed" | command grep -qx "$b" && echo "$b"; done | awk '!seen[$0]++')
rep="$out/report.txt"; : > "$rep"
pass=0; fail=0
cd "$out/ml"
for m in $mods; do
  for f in $m.mli $m.ml; do
    [ -f "$f" ] || continue
    if ocamlfind ocamlc -c -w -a -package unix,threads.posix -thread "$f" > "$f.err" 2>&1; then
      echo "PASS $f" >> "$rep"; pass=$((pass+1))
    else
      echo "FAIL $f :: $(grep -m1 -E '^Error' "$f.err")" >> "$rep"; fail=$((fail+1))
    fi
  done
done
echo "OCAML files: pass=$pass fail=$fail" >> "$rep"
roots=$(command grep '^FAIL' "$rep" | command grep -vE 'Unbound module Lm_|Could not find the .cmi' | wc -l | tr -d ' ')
echo "OCAML root failures (not a missing Lm_ dependency): $roots" >> "$rep"
cd "$out/c"; cp=0; cf=0
inc=$(ocamlfind ocamlc -where)
for f in *.c; do
  if cc -c -I"$inc" -I. -DHAVE_SNPRINTF "$f" -o /dev/null > "$f.err" 2>&1; then
    echo "PASS $f" >> "$rep"; cp=$((cp+1))
  else
    echo "FAIL $f :: $(grep -m1 'error:' "$f.err")" >> "$rep"; cf=$((cf+1))
  fi
done
echo "C files: pass=$cp fail=$cf" >> "$rep"
echo "ocaml $(ocamlfind ocamlc -version)" >> "$rep"
tail -3 "$rep"
