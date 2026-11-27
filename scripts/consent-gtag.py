#!/usr/bin/env python3
"""Quarto post-render step: load Google Analytics only after consent.

With `cookie-consent: true`, Quarto holds back the inline gtag('config', ...)
block until the visitor accepts tracking cookies, but it still loads the
gtag.js library itself on every page, before the visitor answers the banner.
gtag.js sends nothing without that config block, but the browser still
contacts www.googletagmanager.com. This marks the library tag the same way
as the config block (type="text/plain" cookie-consent="tracking"). The
cookie-consent library then adds it, with its src and async, when the
visitor accepts, and at page load for a visitor who accepted before.
Safe to rerun.

Runs from _quarto.yml (project: post-render). Run by hand:
    python3 scripts/consent-gtag.py               # uses docs/
"""
import os
import re
from pathlib import Path

out = Path(os.environ.get("QUARTO_PROJECT_OUTPUT_DIR") or "docs")
TAG = re.compile(r'<script (async="" src="https://www\.googletagmanager\.com/gtag/js\?id=[^"]+")></script>')
HELD = '<script type="text/plain" cookie-consent="tracking">'

changed = skipped = 0
for page in out.rglob("*.html"):
    text = page.read_text(encoding="utf-8", errors="surrogateescape")
    if not TAG.search(text):
        continue
    # Only where the consent banner manages the config block too
    if HELD not in text or "cookieconsent.run(" not in text:
        print(f"::warning::consent-gtag: {page} has gtag.js but no consent-held "
              "config block; left it alone. Update scripts/consent-gtag.py.")
        skipped += 1
        continue
    page.write_text(TAG.sub(r'<script type="text/plain" cookie-consent="tracking" \1></script>', text),
                    encoding="utf-8", errors="surrogateescape")
    changed += 1
print(f"consent-gtag: held gtag.js until consent on {changed} pages"
      + (f", skipped {skipped}" if skipped else ""))
