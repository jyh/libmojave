#!/usr/bin/env python3
"""Print the README's table of tests, derived from test/*.ml.

For each test file: the libmojave modules it calls (Lm_* names in its
code, comments removed), and its kind: "model" when its header comment
says it checks against an oracle (a model, native ints, a bool matrix,
the C locale, ...), else "pin".
Run from the repository root:  python3 scripts/test_table.py
"""
import glob
import os
import re


def strip_comments(src):
    out, depth, i = [], 0, 0
    while i < len(src):
        if src.startswith("(*", i):
            depth += 1
            i += 2
        elif src.startswith("*)", i) and depth:
            depth -= 1
            i += 2
        else:
            if depth == 0:
                out.append(src[i])
            i += 1
    return "".join(out)


def header(src):
    m = re.match(r"\s*\(\*(.*?)\*\)", src, re.S)
    return m.group(1) if m else ""


rows = []
for path in sorted(glob.glob("test/test_*.ml")):
    src = open(path).read()
    code = strip_comments(src)
    mods = sorted(set(re.findall(r"\bLm_[a-z0-9_]+", code)))
    words = " ".join(header(src).split()).lower()
    oracle = r"\bagainst (a |an |the )?(brute-force |list |bool )?(models?|native|matrix|lm_symbol|c locale|earley)"
    kind = "model" if re.search(oracle, words) else "pin"
    rows.append((os.path.basename(path), kind, mods))

print("| test | kind | modules |")
print("|---|---|---|")
for name, kind, mods in rows:
    print(f"| `{name}` | {kind} | {', '.join('`' + m.capitalize() + '`' for m in mods)} |")
print()
print(f"{len(rows)} test files: {sum(1 for r in rows if r[1] == 'model')} model checks, "
      f"{sum(1 for r in rows if r[1] == 'pin')} pins.")
