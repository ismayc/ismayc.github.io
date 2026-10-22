"""Build the logo, favicon, and link-preview image for the two packages
(ouanalytics in R, ouanalytics-py in Python) from one design: the see-saw.

  uv run --with fonttools --with pillow python branding/make_branding.py   (from nba-over-under-2026-2027/)

Writes the sources and renders, then copies what each package serves:
  ouanalytics/pkgdown/branding/   logo.svg, favicon.svg, og-source.html, og.png, ...
  ouanalytics/man/figures/        logo.svg, logo.png        (README, pkgdown navbar)
  ouanalytics/pkgdown/favicon/    favicon.svg, favicon-32.png, favicon-96x96.png, apple-touch-icon.png, favicon.ico
  ouanalytics/pkgdown/assets/     og.png
  ouanalytics-py/branding/        the same set for the Python package
  ouanalytics/pkgdown/assets/ouanalytics-py-logo.png   the Python logo, for the site's Python article

The letters in the logo and favicon are outlines of Big Shoulders Display Black
(SIL Open Font License), so the files look the same on any machine. The font file
is downloaded once and is not committed. The link-preview image is rendered from
og-source.html by headless Chrome through ~/.claude/skills/web-page-meta.
"""
import json
import pathlib
import shutil
import subprocess
import sys
import tempfile
import urllib.request

from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen
from fontTools.ttLib import TTFont

HERE = pathlib.Path(__file__).resolve().parent
PROJECT = HERE.parent
FONT_URL = ("https://fonts.gstatic.com/s/bigshouldersdisplay/v24/"
            "fC1MPZJEZG-e9gHhdI4-NBbfd2ys3SjJCx12wPgf9g-_3F0YdSY8JF4.ttf")
FONT_PATH = HERE / ".cache" / "BigShouldersDisplay-Black.ttf"
CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
RENDER_META = pathlib.Path.home() / ".claude/skills/web-page-meta/render_meta.py"

# Colors: felt-green table for R, dark navy with a gold rim for Python
CREAM, OVER, UNDER, ORANGE = "#f4efe0", "#3a7bd5", "#d9473f", "#f08a3c"
GOLD = "#f2c14e"
PKGS = {
    "r": dict(name="ouanalytics", kind="R package", bg="#1f5a3d", rim=CREAM, chip="R",
              chip_bg=CREAM, chip_fg="#1f5a3d", url="ismayc.github.io/ouanalytics",
              sub="The calculations behind the NBA over/under pool page, with the official playoff tie-breakers",
              title_px=118),
    "py": dict(name="ouanalytics-py", kind="Python package", bg="#16324f", rim=GOLD, chip="py",
               chip_bg=GOLD, chip_fg="#16324f", url="ismayc.github.io/ouanalytics",
               sub="An independent polars recomputation that checks every table against R",
               title_px=90),
}
HEX = "100,4.5 196.2,60 196.2,171 100,226.5 3.8,171 3.8,60"
HEX_INNER = "100,13 188.5,64.5 188.5,166.5 100,218 11.5,166.5 11.5,64.5"


def load_font():
    if not FONT_PATH.exists():
        FONT_PATH.parent.mkdir(exist_ok=True)
        urllib.request.urlretrieve(FONT_URL, FONT_PATH)
    return TTFont(FONT_PATH)


FONT = None


def text_path(text, size, cx, baseline, fill, width=None):
    """Outline `text` centered on cx. With `width`, stretch it to that width."""
    global FONT
    FONT = FONT or load_font()
    gs, cmap, upm = FONT.getGlyphSet(), FONT.getBestCmap(), FONT["head"].unitsPerEm
    names = [cmap[ord(c)] for c in text]
    advance = sum(gs[n].width for n in names)
    sy = size / upm
    sx = (width / advance) if width else sy
    x = cx - advance * sx / 2
    pen = SVGPathPen(gs)
    for n in names:
        gs[n].draw(TransformPen(pen, (sx, 0, 0, -sy, x, baseline)))
        x += gs[n].width * sx
    return f'<path d="{pen.getCommands()}" fill="{fill}"/>'


def ball(cx, cy, r, fill, line, lw):
    k = 0.71 * r
    return (
        f'<g fill="none" stroke="{line}" stroke-width="{lw}" stroke-linecap="round">'
        f'<circle cx="{cx}" cy="{cy}" r="{r}" fill="{fill}"/>'
        f'<path d="M{cx-r} {cy}H{cx+r}M{cx} {cy-r}V{cy+r}"/>'
        f'<path d="M{cx-k:.1f} {cy-k:.1f}C{cx-0.3*r:.1f} {cy-0.3*r:.1f} {cx-0.3*r:.1f} {cy+0.3*r:.1f} {cx-k:.1f} {cy+k:.1f}"/>'
        f'<path d="M{cx+k:.1f} {cy-k:.1f}C{cx+0.3*r:.1f} {cy-0.3*r:.1f} {cx+0.3*r:.1f} {cy+0.3*r:.1f} {cx+k:.1f} {cy+k:.1f}"/>'
        "</g>"
    )


def chip(cx, cy, w, h, label, bg, fg):
    return (f'<rect x="{cx - w/2}" y="{cy - h/2}" width="{w}" height="{h}" rx="{h/2}" fill="{bg}"/>'
            + text_path(label, h * 0.62, cx, cy + h * 0.3, fg))


def logo_svg(p):
    """The hex sticker: a ball on a see-saw, O (Over) pan blue, U (Under) pan red."""
    return (
        '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 231" width="400" height="462" '
        f'role="img" aria-label="{p["name"]} hex sticker: a basketball on a see-saw">'
        f'<polygon points="{HEX}" fill="{p["bg"]}"/>'
        f'<polygon points="{HEX}" fill="none" stroke="{p["rim"]}" stroke-width="7" stroke-linejoin="round"/>'
        f'<polygon points="{HEX_INNER}" fill="none" stroke="{p["rim"]}" stroke-width="1.2" stroke-dasharray="3 4" opacity=".7"/>'
        '<g transform="rotate(8 100 108)">'
        f'<rect x="34" y="104" width="132" height="7" rx="3" fill="{CREAM}"/>'
        f'<rect x="34" y="84" width="34" height="16" rx="4" fill="{OVER}"/>'
        + text_path("O", 15, 51, 96.5, "#fff") +
        f'<rect x="132" y="84" width="34" height="16" rx="4" fill="{UNDER}"/>'
        + text_path("U", 15, 149, 96.5, "#fff")
        + ball(100, 80, 22, ORANGE, "#3b2410", 2.2) + "</g>"
        f'<polygon points="100,112 84,134 116,134" fill="{CREAM}"/>'
        + text_path("ouanalytics", 36, 100, 168, CREAM, width=150)
        + chip(100, 199, 38 if p["chip"] == "py" else 34, 18, p["chip"], p["chip_bg"], p["chip_fg"])
        + "</svg>"
    )


def favicon_svg(p):
    return (
        '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64" width="64" height="64">'
        f'<rect x="1.5" y="1.5" width="61" height="61" rx="14" fill="{p["bg"]}" stroke="{p["rim"]}" stroke-width="3"/>'
        '<g transform="rotate(10 32 40)">'
        f'<rect x="9" y="41" width="46" height="5" rx="2" fill="{CREAM}"/>'
        f'<rect x="9" y="31" width="13" height="8" rx="2" fill="{OVER}"/>'
        f'<rect x="42" y="31" width="13" height="8" rx="2" fill="{UNDER}"/>'
        + ball(32, 28, 10.5, ORANGE, "#3b2410", 2) + "</g>"
        f'<polygon points="32,45 24,57 40,57" fill="{CREAM}"/></svg>'
    )


def og_html(p, hex_svg):
    inline = hex_svg.replace('width="400" height="462"', 'width="440" height="508"')
    return f"""<!doctype html>
<!-- Link-preview source for {p['name']}: 1200x630.
     Re-render: uv run --with fonttools --with pillow python branding/make_branding.py
     (writes og.png with ~/.claude/skills/web-page-meta/render_meta.py) -->
<html lang="en"><head><meta charset="utf-8">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link href="https://fonts.googleapis.com/css2?family=Big+Shoulders+Display:wght@900&family=Source+Sans+3:wght@400;600;700&display=swap" rel="stylesheet">
<style>
  html, body {{ width: 1200px; height: 630px; margin: 0; overflow: hidden; }}
  body {{ background: {p['bg']}; color: {CREAM}; display: flex; align-items: center;
          gap: 64px; padding: 0 80px; box-sizing: border-box;
          font-family: 'Source Sans 3', Arial, sans-serif; }}
  svg {{ display: block; flex: none; }}
  .kind {{ font-size: 30px; font-weight: 700; letter-spacing: .14em; text-transform: uppercase; opacity: .8; }}
  .title {{ font: 900 {p['title_px']}px/1 'Big Shoulders Display', 'Arial Narrow', sans-serif;
            margin: 10px 0 20px; white-space: nowrap; }}
  .sub {{ font-size: 34px; line-height: 1.3; max-width: 560px; opacity: .92; }}
  .url {{ font-size: 28px; font-weight: 600; margin-top: 30px; opacity: .75; }}
</style></head><body>
{inline}
<div><div class="kind">{p['kind']}</div><div class="title">{p['name']}</div>
<div class="sub">{p['sub']}</div><div class="url">{p['url']}</div></div>
</body></html>
"""


def chrome_shot(url, out, w, h, transparent=False):
    out.unlink(missing_ok=True)
    flags = ["--default-background-color=00000000"] if transparent else []
    with tempfile.TemporaryDirectory() as prof:
        subprocess.run(
            ["perl", "-e", "alarm 60; exec @ARGV", CHROME, "--headless=new", "--disable-gpu",
             "--hide-scrollbars", f"--user-data-dir={prof}", f"--window-size={w},{h}",
             "--virtual-time-budget=5000", *flags, f"--screenshot={out}", url],
            check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if not out.exists():
        sys.exit(f"render failed: {out}")


def build(key, brand_dir, copies):
    p = PKGS[key]
    brand_dir.mkdir(parents=True, exist_ok=True)
    logo = logo_svg(p)
    (brand_dir / "logo.svg").write_text(logo)
    (brand_dir / "favicon.svg").write_text(favicon_svg(p))
    (brand_dir / "og-source.html").write_text(og_html(p, logo))
    subprocess.run([sys.executable, "-I", str(RENDER_META), "--dir", str(brand_dir), "--bg", p["bg"]],
                   check=True)
    # logo.png: transparent corners, 2x of the 200-unit viewBox
    wrap = brand_dir / ".logo-wrap.html"
    wrap.write_text(f'<!doctype html><html><body style="margin:0;background:transparent">{logo}</body></html>')
    chrome_shot(f"file://{wrap}", brand_dir / "logo.png", 400, 462, transparent=True)
    wrap.unlink()
    # favicon.ico for browsers that ask for /favicon.ico: 16, 32, and 48 px
    from PIL import Image
    Image.open(brand_dir / "apple-touch-icon.png").save(
        brand_dir / "favicon.ico", sizes=[(16, 16), (32, 32), (48, 48)])
    Image.open(brand_dir / "apple-touch-icon.png").resize((96, 96), Image.LANCZOS).save(
        brand_dir / "favicon-96x96.png")
    (brand_dir / "site.webmanifest").write_text(json.dumps({
        "name": p["name"], "short_name": p["name"],
        "icons": [{"src": "favicon-96x96.png", "sizes": "96x96", "type": "image/png"},
                  {"src": "apple-touch-icon.png", "sizes": "180x180", "type": "image/png"}],
        "theme_color": p["bg"], "background_color": "#fafbf7", "display": "standalone"}, indent=2) + "\n")
    for src, dest in copies:
        dest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy(brand_dir / src, dest)


def main():
    r_dir = PROJECT / "ouanalytics"
    build("r", r_dir / "pkgdown" / "branding", [
        ("logo.svg", r_dir / "man" / "figures" / "logo.svg"),
        ("logo.png", r_dir / "man" / "figures" / "logo.png"),
        # pkgdown/favicon/ is copied to the site root, and stops pkgdown from
        # building its own favicons (it calls an online service)
        ("favicon.svg", r_dir / "pkgdown" / "favicon" / "favicon.svg"),
        ("favicon-32.png", r_dir / "pkgdown" / "favicon" / "favicon-32.png"),
        ("apple-touch-icon.png", r_dir / "pkgdown" / "favicon" / "apple-touch-icon.png"),
        ("favicon.ico", r_dir / "pkgdown" / "favicon" / "favicon.ico"),
        ("favicon-96x96.png", r_dir / "pkgdown" / "favicon" / "favicon-96x96.png"),  # pkgdown's head links it
        ("site.webmanifest", r_dir / "pkgdown" / "favicon" / "site.webmanifest"),   # and this
        ("og.png", r_dir / "pkgdown" / "assets" / "og.png"),
    ])
    build("py", PROJECT / "ouanalytics-py" / "branding", [
        ("logo.png", r_dir / "pkgdown" / "assets" / "ouanalytics-py-logo.png"),  # the Python article
    ])
    print("wrote branding for both packages")


if __name__ == "__main__":
    main()
