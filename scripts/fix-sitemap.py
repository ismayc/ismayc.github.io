#!/usr/bin/env python3
"""Quarto post-render step: tidy the sitemap Quarto writes.

1. A page that asks search engines not to index it (a robots "noindex" meta
   tag) is removed from the sitemap. A sitemap should list only pages meant
   for search results.
2. ".../index.html" becomes ".../", so each sitemap URL equals the page's
   <link rel="canonical"> (Quarto writes the canonical without index.html).

Runs from _quarto.yml (project: post-render). Run by hand:
    python3 scripts/fix-sitemap.py            # uses docs/
"""
import os
import re
import sys
from pathlib import Path

out = Path(os.environ.get("QUARTO_PROJECT_OUTPUT_DIR") or "docs")
sitemap = out / "sitemap.xml"
if not sitemap.exists():
    sys.exit(0)

NOINDEX = re.compile(r'<meta\s+name="robots"\s+content="[^"]*noindex', re.I)
text = sitemap.read_text()
site = None
kept, dropped = [], []
for block in re.findall(r"\s*<url>.*?</url>", text, flags=re.S):
    loc = re.search(r"<loc>(.*?)</loc>", block).group(1)
    m = re.match(r"(https?://[^/]+)/(.*)", loc)
    site, rel = m.group(1), m.group(2)
    page = out / (rel if not rel.endswith("/") and rel else rel + "index.html")
    if page.exists() and NOINDEX.search(page.read_text(errors="replace")[:20000]):
        dropped.append(rel)
        continue
    if rel == "index.html" or rel.endswith("/index.html"):
        block = block.replace(loc, loc[: -len("index.html")])
    kept.append(block)

head = text[: text.index("<url>")].rstrip()
sitemap.write_text(head + "".join(kept) + "\n</urlset>\n")
print(f"fix-sitemap: kept {len(kept)}, dropped {dropped or 'none'}")
