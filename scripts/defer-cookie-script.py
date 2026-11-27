#!/usr/bin/env python3
"""Quarto post-render step: load the cookie banner script with `defer`.

Quarto puts <script src="site_libs/cookie-consent/cookie-consent.js"> in each
page's <head> with no `defer`, so the browser stops building the page until it
downloads and runs the 200 KB file (Lighthouse counted about 0.5 s on the home
page, November 26, 2026). The banner itself starts from Quarto's inline
`document.addEventListener('DOMContentLoaded', ... cookieconsent.run(...))`,
and a deferred script always runs before DOMContentLoaded, so `defer` changes
only when the file runs, not what it does. Safe to rerun.

Runs from _quarto.yml (project: post-render). Run by hand:
    python3 scripts/defer-cookie-script.py        # uses docs/
"""
import os
import re
from pathlib import Path

out = Path(os.environ.get("QUARTO_PROJECT_OUTPUT_DIR") or "docs")
TAG = re.compile(r'<script src="((?:\.\./)*site_libs/cookie-consent/cookie-consent\.js)"></script>')
INIT = "document.addEventListener('DOMContentLoaded', function () {\ncookieconsent.run("

changed = skipped = 0
for page in out.rglob("*.html"):
    text = page.read_text(encoding="utf-8", errors="surrogateescape")
    if not TAG.search(text):
        continue
    # Only safe while the banner still starts on DOMContentLoaded
    if INIT not in text:
        print(f"::warning::defer-cookie-script: {page} starts the banner another way; "
              "left it alone. Update scripts/defer-cookie-script.py.")
        skipped += 1
        continue
    page.write_text(TAG.sub(r'<script src="\1" defer></script>', text),
                    encoding="utf-8", errors="surrogateescape")
    changed += 1
print(f"defer-cookie-script: deferred the cookie script on {changed} pages"
      + (f", skipped {skipped}" if skipped else ""))
