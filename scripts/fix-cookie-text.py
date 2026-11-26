#!/usr/bin/env python3
"""Quarto post-render step: correct the cookie banner's text.

Quarto's cookie-consent library (TermsFeed) says the site uses cookies "to show
you personalized content and targeted ads". This site shows no ads and no
personalized content; its only tracker is Google Analytics. The library has no
setting for its text, so this removes that clause from the rendered copy in
site_libs. If a Quarto update changes the text, it prints a warning and leaves
the file alone, so the render still succeeds and the warning shows in the log.

Runs from _quarto.yml (project: post-render). Run by hand:
    python3 scripts/fix-cookie-text.py            # uses docs/
"""
import os
import re
from pathlib import Path

out = Path(os.environ.get("QUARTO_PROJECT_OUTPUT_DIR") or "docs")
lib = out / "site_libs" / "cookie-consent" / "cookie-consent.js"
if not lib.exists():
    raise SystemExit(0)

# The library holds two English variants (US "personalized", UK "personalised")
CLAUSE = re.compile(r"to show you personali[sz]ed content and targeted ads, ")
text = lib.read_text(encoding="utf-8")
text, count = CLAUSE.subn("", text)
if count == 0:
    if "targeted ads" in text:
        print("::warning::fix-cookie-text: the banner text changed; "
              "'targeted ads' is still in it. Update scripts/fix-cookie-text.py.")
    else:
        print("fix-cookie-text: no ads clause found (already removed)")
    raise SystemExit(0)
lib.write_text(text, encoding="utf-8")
print(f"fix-cookie-text: removed the ads clause ({count} copies)")
