#!/usr/bin/env python3
"""Apply the scorer's-book theme and the season strip to the over/under pages.

Run from the repo root:

  python3 nba-over-under-META/site-style/restyle_season_pages.py past
      Restyle 2021-2026-nba-over-under.html (root and docs/ copies) in place.
      The rendered charts and data are left exactly as they were.

  python3 nba-over-under-META/site-style/restyle_season_pages.py preview PATH
      Turn the preseason preview built by 08-preseason-pool-analysis.R into
      2027-nba-over-under-preview.html, and also 2027-nba-over-under.html
      until the in-season page exists (root and docs/).

  python3 nba-over-under-META/site-style/restyle_season_pages.py season 2027
      Restyle the in-season page that make_plot_html.R rendered to
      docs/2027-nba-over-under.html, then copy it to the repo root. The daily
      GitHub Actions build runs this right after the render.

  python3 nba-over-under-META/site-style/restyle_season_pages.py playoffs 2026
      Build 2026-nba-over-under-playoffs.html from playoffs-template.html and
      nba-over-under-2025-2026/playoffs/playoffs.json. Run it before `past`
      (or `season`) so the season page links to it.

Every injected block sits between OU-THEME markers, so rerunning replaces it.
"""
import csv
import html
import json
import re
import shutil
import sys
from collections import defaultdict
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
THEME = (HERE / "ou-theme.css").read_text()
SEASON_CSS = (HERE / "ou-season-pages.css").read_text()
SCROLL_JS = "<script>" + (HERE / "ou-scroll-hint.js").read_text() + "</script>"
PICKS = HERE.parent / "all_picks_6seasons.csv"

CURRENT_YEAR = 2027
PAST_YEARS = range(2021, CURRENT_YEAR)
DISPLAY = {"Mike": "Mike J"}  # how the group has always named him

# The top 4 also play a playoff version (a draft of NBA playoff teams after
# the regular season). Champions by season-ending year, confirmed by Chester.
# Add each new champion here.
PLAYOFF_WINNERS = {2021: "Jake", 2022: "Chester", 2023: "Phil",
                   2024: "Chester", 2025: "Mike J", 2026: "Phil"}

FONTS = ('<link rel="preconnect" href="https://fonts.googleapis.com">'
         '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>'
         '<link rel="stylesheet" href="https://fonts.googleapis.com/css2?'
         'family=Big+Shoulders+Display:wght@700;800&family=Source+Sans+3:'
         'wght@400;600&display=swap">')

# The season pages drew Over in forestgreen and Under in deeppink. Swap those
# for the theme's blue and red pencil, and the chart text to the page's face.
# Only colors and fonts change; the data in each chart is untouched.
PLOTLY_FONT = """<script>
window.addEventListener('load', function () {
  if (!window.Plotly) return;
  var swap = [
    [/^(forestgreen|#228b22|rgba?\\(34, ?139, ?34)/i, '#2457a6'],
    [/^(deeppink|#ff1493|rgba?\\(255, ?20, ?147)/i, '#b8322f'],
    [/^(blue$|#0000ff|rgba?\\(0, ?0, ?255)/i, '#1c2a44']
  ];
  function recolor(v) {
    if (typeof v !== 'string') return v;
    for (var i = 0; i < swap.length; i++) {
      var m = v.match(swap[i][0]);
      if (m) {
        var a = v.match(/rgba\\([^)]*,\\s*([\\d.]+)\\)/i);
        if (!a) return swap[i][1];
        var h = swap[i][1], r = parseInt(h.slice(1, 3), 16), g = parseInt(h.slice(3, 5), 16), b = parseInt(h.slice(5, 7), 16);
        return 'rgba(' + r + ',' + g + ',' + b + ',' + a[1] + ')';
      }
    }
    return v;
  }
  function walk(o) {
    if (Array.isArray(o)) { for (var i = 0; i < o.length; i++) { o[i] = typeof o[i] === 'object' && o[i] ? (walk(o[i]), o[i]) : recolor(o[i]); } return; }
    for (var k in o) { if (o[k] && typeof o[k] === 'object') walk(o[k]); else o[k] = recolor(o[k]); }
  }
  document.querySelectorAll('.js-plotly-plot').forEach(function (el) {
    try {
      (el.data || []).forEach(walk);
      el.layout.font = el.layout.font || {};
      el.layout.font.family = '"Source Sans 3", "Helvetica Neue", Arial, sans-serif';
      Plotly.redraw(el);
    } catch (e) {}
  });
});
</script>"""

# The embedded scenario explorer posts its height; size its frame to fit
EXPLORER_FRAME = """<script>
// The season page renders a placeholder (a literal iframe would be inlined
// by pandoc); make the frame here
document.querySelectorAll('.ou-explorer-slot').forEach(function (slot) {
  var f = document.createElement('iframe');
  f.className = 'ou-explorer';
  f.src = slot.getAttribute('data-src');
  f.title = 'Scenario explorer';
  f.loading = 'lazy';
  slot.replaceWith(f);
});
// A link to a tab (#scenarios, for one) opens that tab
window.addEventListener('load', function () {
  if (!location.hash || !window.jQuery) return;
  var tab = jQuery('.nav-tabs a[href="' + location.hash.replace(/"/g, '') + '"]');
  if (tab.length) { tab.tab('show'); tab[0].scrollIntoView(); }
});
window.addEventListener('message', function (e) {
  if (!e.data || !e.data.ouExplorerHeight) return;
  document.querySelectorAll('iframe.ou-explorer').forEach(function (f) {
    if (f.contentWindow === e.source) f.style.height = Math.ceil(e.data.ouExplorerHeight) + 'px';
  });
});
</script>"""

START, END = "<!-- OU-THEME:start -->", "<!-- OU-THEME:end -->"
MARK_RE = re.compile(re.escape(START) + r".*?" + re.escape(END), re.S)


def season_label(year):
    return f"{year - 1}-{str(year)[2:]}"


def winners():
    totals = defaultdict(lambda: defaultdict(int))
    with open(PICKS, newline="") as f:
        for row in csv.DictReader(f):
            totals[row["season"]][row["player"]] += int(row["points"])
    out = {}
    for season, players in totals.items():
        name, pts = max(players.items(), key=lambda kv: kv[1])
        out[int(season[:4]) + 1] = (DISPLAY.get(name, name), pts)
    return out


def masthead(current, current_note):
    won = winners()
    items = []
    for year in range(2021, CURRENT_YEAR + 1):
        if year in won:
            name, pts = won[year]
            who = (f'<span class="who">Season: <b>{html.escape(name)}</b> {pts:+d}</span>')
            if year in PLAYOFF_WINNERS:
                who += (f'<span class="who">Playoffs: '
                        f'<b>{html.escape(PLAYOFF_WINNERS[year])}</b></span>')
        else:
            who = f'<span class="who">{html.escape(current_note)}</span>'
        here = ' aria-current="page"' if year == current else ""
        items.append(
            f'<li><a href="{year}-nba-over-under.html"{here}>'
            f'<span class="yr">{season_label(year)}</span>{who}</a></li>')
    return (
        '<header class="ou-masthead"><div class="ou-masthead-inner">'
        '<p class="ou-kicker">NBA over/under pool. '
        '<a href="index.html">Chester Ismay</a> has tracked it here since '
        '2020-21 and run it since 2025-26.</p>'
        '<nav aria-label="Seasons"><ol class="ou-seasons">'
        + "".join(items) + "</ol></nav></div></header>")


def strip_old(text):
    return MARK_RE.sub("", text)


def head_block(css):
    return f"{START}{FONTS}<style>{THEME}\n{css}</style>{END}"


WIDGET_RE = re.compile(r'(<script type="application/json" data-for="[^"]+">)(.*?)(</script>)', re.S)


def nickname(team):
    return "Trail Blazers" if team.endswith("Trail Blazers") else team.split()[-1]


def wrap_title(title):
    """Break a long one-line chart title near its middle."""
    if "<br" in title or len(title) <= 40:
        return title
    mid = len(title) // 2
    spaces = [i for i, c in enumerate(title) if c == " "]
    if not spaces:
        return title
    i = min(spaces, key=lambda s: abs(s - mid))
    return title[:i] + "<br>" + title[i + 1:]


def tidy_facets(text):
    """Small-multiple charts (ggplotly facets): label team panels by nickname,
    thin the axis ticks, wrap long titles, and move a side legend below, so
    they read on a phone held upright. Every step is safe to rerun."""
    heights = dict(re.findall(r'id="(htmlwidget-[^"]+)" style="width:\d+px;height:(\d+)px;"', text))

    def fix(m):
        body = m.group(2)
        if '"annotations"' not in body or '"layout"' not in body:
            return m.group(0)
        data = json.loads(body)
        height = int(heights.get(re.search(r'data-for="([^"]+)"', m.group(1)).group(1), 960))
        layout = data.get("x", {}).get("layout", {})
        strips = [a for a in layout.get("annotations", [])
                  if a.get("yanchor") == "bottom" and a.get("xref") == "paper"
                  and a.get("xanchor") == "center" and isinstance(a.get("text"), str)]
        if len(strips) < 4:
            return m.group(0)
        many = len(strips) >= 20
        # Room for the rotated y-axis title, which ggplotly sets just past the edge
        margin = layout.setdefault("margin", {})
        margin["l"] = max(margin.get("l", 0), 44)
        if many:
            for a in strips:
                a["text"] = nickname(a["text"])
                a.setdefault("font", {})["size"] = 9
        for key, ax in layout.items():
            if not re.fullmatch(r"[xy]axis\d*", key) or not ax.get("tickvals"):
                continue
            ax["tickangle"] = 0
            vals, labels = ax["tickvals"], ax["ticktext"]
            months = not str(labels[0]).lstrip("-").replace(".", "").isdigit()
            if key.startswith("x") and months and len(vals) > 2:
                pick = slice(1, None, 3)        # Nov..Apr becomes Dec, Mar
            elif key.startswith("x") and many and len(vals) > 2:
                pick = slice(1, None, 2)        # 0..80 becomes 20, 60, clear of the panel edges
            elif key.startswith("x") and len(vals) > 3:
                pick = slice(None, None, 2)
            elif key.startswith("y") and len(vals) == 5:
                pick = slice(None, None, 2)     # 0, 50, 100
            else:
                continue
            ax["tickvals"], ax["ticktext"] = vals[pick], labels[pick]
        title = layout.get("title")
        if isinstance(title, dict) and isinstance(title.get("text"), str):
            title["text"] = wrap_title(title["text"])
            title.setdefault("font", {})["size"] = 15
            if "<br" in title["text"]:
                layout.setdefault("margin", {})["t"] = max(layout["margin"].get("t", 0), 110)
        legend = layout.get("legend") or {}
        if layout.get("showlegend") and legend.get("orientation") != "h":
            # Above the panels, under the chart title (below them it would
            # collide with the x-axis title), with its title on its own line
            margin = layout.setdefault("margin", {})
            margin["t"] = margin.get("t", 0) + 100
            plot_h = height - margin["t"] - margin.get("b", 0)
            legend.update(orientation="h", x=0, xanchor="left", y=1 + 32 / plot_h, yanchor="bottom")
            if isinstance(legend.get("title"), dict):
                legend["title"]["text"] = re.sub(r"\s*<br\s*/?>\s*", " ", legend["title"]["text"])
                legend["title"].setdefault("font", {})["size"] = 12
                legend["title"]["side"] = "top"
            layout["legend"] = legend
        out = json.dumps(data, separators=(",", ":"), ensure_ascii=False).replace("</", "<\\/")
        return m.group(1) + out + m.group(3)
    return WIDGET_RE.sub(fix, text)


def restyle_rmd(text, year, note):
    text = strip_old(text)
    text = tidy_facets(text)
    text = text.replace("</head>", head_block(SEASON_CSS) + "</head>", 1)
    text = re.sub(r"<body([^>]*)>", lambda m: _body_open(m.group(1)), text, count=1)
    body_at = re.search(r"<body[^>]*>", text).end()
    text = (text[:body_at] + START + masthead(year, note) + END + text[body_at:])
    text = text.replace("</body>", START + PLOTLY_FONT + EXPLORER_FRAME + SCROLL_JS + END + "</body>", 1)
    return text


def _body_open(attrs):
    if "ou-page" in attrs:
        return f"<body{attrs}>"
    if 'class="' in attrs:
        return "<body" + attrs.replace('class="', 'class="ou-page ', 1) + ">"
    return f'<body class="ou-page"{attrs}>'


def write_both(name, text):
    for path in (ROOT / name, ROOT / "docs" / name):
        path.write_text(text)
        print(f"wrote {path.relative_to(ROOT)} ({len(text):,} chars)")


CHANCE_NOTE = (
    "How likely each player was to finish first, or in the top 4, as of each "
    "date of the season. Rebuilt in October 2026 with the method the 2026-27 "
    "page uses: each day, the rest of the season was simulated 100,000 times "
    "from each team's record, pulled toward its Vegas line (the line counts "
    "as 20 games of evidence), with how good each team really is drawn at "
    "random in every simulation. Every simulated season is scored with the "
    "pool's rules and tiebreakers. The dotted line is an even share.")

# Draw each chart the first time its tab is visible, so plotly sizes it to
# the tab rather than to a hidden zero-width box.
CHANCE_JS = """<script>
(function () {
  var figs = __FIGS__;
  function draw() {
    if (!window.Plotly) return;
    Object.keys(figs).forEach(function (k) {
      var el = document.getElementById('ou-chance-' + k);
      if (!el || el.getAttribute('data-drawn') || el.offsetWidth === 0) return;
      var f = figs[k];
      f.layout.height = f.layout.height || 520;
      Plotly.newPlot(el, f.data, f.layout, {displayModeBar: false, responsive: true});
      el.setAttribute('data-drawn', '1');
    });
  }
  window.addEventListener('load', draw);
  if (window.jQuery) jQuery(document).on('shown.bs.tab', draw);
})();
</script>"""


def add_chances(text, year):
    """Add the Chance to win tab (built by past_chances.R) as the first tab."""
    path = HERE / "past-chances" / f"{year}.json"
    if not path.exists():
        print(f"no {path.name}; {year} page left without the chance tab")
        return text
    data = json.loads(path.read_text())
    # Month names only, so the dates don't collide on a phone
    for k in ("first", "top4"):
        data[k]["layout"]["xaxis"].update(tickformat="%b", nticks=6)
    figs = json.dumps({"first": {"data": data["first"]["data"], "layout": data["first"]["layout"]},
                       "top4": {"data": data["top4"]["data"], "layout": data["top4"]["layout"]}},
                      separators=(",", ":")).replace("</", "<\\/")
    # The new tab opens first, so the old default tab gives up "active"
    text = text.replace('<div id="projected-points-for-each-player" class="section level2 tabset active">\n'
                        '<h2 class="tabset active">',
                        '<div id="projected-points-for-each-player" class="section level2 tabset">\n'
                        '<h2 class="tabset">', 1)
    section = (
        '<div id="chance-to-win" class="section level2 tabset active">\n'
        '<h2 class="tabset active">Chance to win</h2>\n'
        f'<p>{html.escape(CHANCE_NOTE)}</p>\n'
        '<div id="chance-to-finish-first" class="section level3">\n'
        '<h3>Chance to finish first</h3>\n'
        '<div id="ou-chance-first" class="ou-chance"></div>\n</div>\n'
        '<div id="chance-to-make-the-top-4" class="section level3">\n'
        '<h3>Chance to make the top 4</h3>\n'
        '<div id="ou-chance-top4" class="ou-chance"></div>\n</div>\n</div>\n')
    anchor = '<h1 class="tabset"></h1>\n'
    at = text.index(anchor, text.index('<div id="section" class="section level1 tabset">')) + len(anchor)
    text = text[:at] + START + section + END + text[at:]
    return text.replace("</body>", START + CHANCE_JS.replace("__FIGS__", figs) + END + "</body>", 1)


EXPLORER_NOTE = (
    "Every way the last open teams could have finished, and where each "
    "player would have ended up. It opens on the first date with 20 or fewer "
    "teams still undecided; pick a later date to follow the finish. Teams far "
    "from their line start on their likely side, and clicking any team changes it.")


def add_explorer(text, year):
    """Add the Scenarios tab (built by past_explorers.R) after Chance to win."""
    name = f"{year}-nba-scenario-explorer.html"
    if not (ROOT / name).exists():
        print(f"no {name}; {year} page left without the Scenarios tab")
        return text
    section = (
        '<div id="scenarios" class="section level2">\n'
        '<h2>Scenarios</h2>\n'
        f'<p>{html.escape(EXPLORER_NOTE)}</p>\n'
        f'<iframe class="ou-explorer" src="{name}" title="Scenario explorer" loading="lazy"></iframe>\n'
        '</div>\n')
    anchor = '<div id="by-team" class="section level2 tabset">'
    at = text.index(anchor)
    return text[:at] + START + section + END + text[at:]


def note_page():
    """The old single explorer URL now points to each season's Scenarios tab."""
    links = "".join(
        f'<li><a href="{y}-nba-over-under.html#scenarios">{season_label(y)}</a></li>'
        for y in range(CURRENT_YEAR, 2020, -1))
    title = "NBA Over/Under scenario explorer"
    desc = ("The scenario explorer now sits on each season's page, in the "
            "Scenarios tab.")
    return (
        '<!doctype html><html lang="en"><head><meta charset="utf-8">'
        '<meta name="viewport" content="width=device-width, initial-scale=1">'
        f'<title>{title}</title><meta name="description" content="{html.escape(desc)}">'
        + head_meta("nba-scenario-explorer.html", title, desc)
        + head_block(NOTE_CSS) + '</head><body class="ou-page">'
        + START + masthead(None, "Current season") + END
        + '<main class="ou-note"><h1>Scenario explorer</h1>'
        '<p>The scenario explorer has moved to each season&#8217;s page, in the '
        'Scenarios tab. For 2026-27 it opens once 20 or fewer teams are still '
        'undecided, usually in mid-March. Until then, the earlier seasons show how '
        'their finishes played out.</p>'
        f'<ul class="ou-note-links">{links}</ul></main></body></html>')


NOTE_CSS = """
body.ou-page { margin: 0; background: var(--paper); color: var(--ink);
  font-family: "Source Sans 3", "Helvetica Neue", Arial, sans-serif; }
.ou-note { max-width: 40rem; margin: 2.5rem auto; padding: 1.5rem 1.25rem 2rem;
  background: var(--sheet); border: 1px solid var(--rule); }
.ou-note h1 { font-family: "Big Shoulders Display", sans-serif; font-weight: 800;
  font-size: 2.6rem; line-height: 1; margin: 0 0 1rem; }
.ou-note p { font-size: 1.1rem; line-height: 1.55; margin: 0 0 1.25rem; }
.ou-note-links { list-style: none; padding: 0; margin: 0; display: flex;
  flex-wrap: wrap; gap: 0.5rem 1.25rem; }
.ou-note-links a { font-family: "Big Shoulders Display", sans-serif;
  font-weight: 700; font-size: 1.35rem; color: var(--ink); }
@media (max-width: 600px) { .ou-note { margin: 1rem 16px; } }
"""


def do_past():
    for year in PAST_YEARS:
        name = f"{year}-nba-over-under.html"
        src = ROOT / name
        text = restyle_rmd(src.read_text(), year, "Current season")
        text = link_playoffs(text, year)
        # Each page was last built after its season ended, so "today" is
        # stale on a finished season
        text = text.replace("<hr />\n<p>No NBA games are scheduled for today.</p>\n", "", 1)
        text = add_chances(text, year)
        text = add_explorer(text, year)
        write_both(name, text)
    write_both("nba-scenario-explorer.html", note_page())


SITE = "https://ismayc.github.io"
ASSETS = f"{SITE}/nba-over-under-assets"


def head_meta(name, title, description):
    """Icons and link preview (assets rendered from nba-over-under-assets/)."""
    return (
        f'<link rel="icon" href="nba-over-under-assets/favicon.svg" type="image/svg+xml">'
        f'<link rel="icon" href="nba-over-under-assets/favicon-32.png" sizes="32x32" type="image/png">'
        f'<link rel="apple-touch-icon" href="nba-over-under-assets/apple-touch-icon.png">'
        f'<meta property="og:type" content="website">'
        f'<meta property="og:title" content="{html.escape(title)}">'
        f'<meta property="og:description" content="{html.escape(description)}">'
        f'<meta property="og:url" content="{SITE}/{name}">'
        f'<meta property="og:image" content="{ASSETS}/og.png">'
        f'<meta property="og:image:width" content="1200">'
        f'<meta property="og:image:height" content="630">'
        f'<meta name="twitter:card" content="summary_large_image">')


def do_preview(path):
    template = Path(path).read_text()
    template = template.replace("/*OU-THEME*/", THEME)
    template = template.replace("</body>", SCROLL_JS + "</body>", 1)
    template = template.replace("<!--OU-MASTHEAD-->",
                                masthead(CURRENT_YEAR, "Preseason preview"))
    desc = re.search(r'<meta name="description" content="([^"]*)"', template)
    desc = html.unescape(desc.group(1)) if desc else ""
    title = f"NBA Over/Under {season_label(CURRENT_YEAR)} preseason preview"

    def page(name):
        return template.replace("<!--OU-META-->", head_meta(name, title, desc))

    write_both(f"{CURRENT_YEAR}-nba-over-under-preview.html",
               page(f"{CURRENT_YEAR}-nba-over-under-preview.html"))
    season_page = ROOT / "docs" / f"{CURRENT_YEAR}-nba-over-under.html"
    # Until make_plot_html.R renders the in-season page, the season URL shows
    # the preview. Once it exists, leave it alone.
    if not season_page.exists() or 'class="ou-page ou-preview"' in season_page.read_text():
        write_both(f"{CURRENT_YEAR}-nba-over-under.html",
                   page(f"{CURRENT_YEAR}-nba-over-under.html"))
    else:
        print("in-season page exists; left 2027-nba-over-under.html alone")


def playoffs_name(year):
    return f"{year}-nba-over-under-playoffs.html"


def link_playoffs(text, year):
    """Link a season page to its playoffs page and rooting guide, when they
    exist: in the kicker line and as buttons under the page title."""
    links = []
    if (ROOT / playoffs_name(year)).exists():
        champ = PLAYOFF_WINNERS.get(year)
        label = f"{season_label(year)} playoffs" + (f": {champ} won" if champ else "")
        links.append((playoffs_name(year), f"{season_label(year)} playoffs", label, ""))
    guide = f"{year}-nba-rooting-guide.html"
    if (ROOT / guide).exists():
        links.append((guide, "Rooting guide", "Rooting guide", ' class="ou-link-secondary"'))
    if not links:
        return text
    text = text.replace(
        '<p class="ou-kicker">',
        '<p class="ou-kicker">' + "".join(f'<a href="{href}">{html.escape(short)}</a>. '
                                          for href, short, _, _ in links), 1)
    buttons = ('<p class="ou-playoffs-link">'
               + "".join(f'<a href="{href}"{cls}>{html.escape(label)} &rarr;</a>'
                         for href, _, label, cls in links) + '</p>')
    author = re.search(r'<h4 class="author">.*?</h4>', text)
    if author:
        text = text[:author.end()] + START + buttons + END + text[author.end():]
    return text


def do_playoffs(year):
    """Build a season's playoffs page from nba-over-under-<season>/playoffs/playoffs.json."""
    data_path = ROOT / f"nba-over-under-{year - 1}-{year}" / "playoffs" / "playoffs.json"
    data = json.loads(data_path.read_text())
    champ, team = data["finals"]["champion"], data["finals"]["nba_champion"]
    title = f"NBA Over/Under {season_label(year)} playoffs"
    desc = (f"How the {season_label(year)} NBA over/under playoffs went: the draft "
            f"of NBA playoff teams, the O/U Finals, and how {champ} won with the {team}.")
    name = playoffs_name(year)
    text = (HERE / "playoffs-template.html").read_text()
    text = text.replace("/*OU-THEME*/", THEME)
    text = text.replace("</body>", SCROLL_JS + "</body>", 1)
    text = text.replace("<!--OU-MASTHEAD-->", masthead(year, "Current season"))
    text = text.replace("<!--OU-META-->", head_meta(name, title, desc))
    text = text.replace("__TITLE__", html.escape(title)).replace("__DESCRIPTION__", html.escape(desc))
    text = text.replace("__DATA__", json.dumps(data, separators=(",", ":")).replace("</", "<\\/"))
    text = text.replace(
        '<p class="ou-kicker">',
        f'<p class="ou-kicker"><a href="{year}-nba-over-under.html">'
        f'{season_label(year)} regular season</a>. ', 1)
    write_both(name, text)


def do_season(year):
    name = f"{year}-nba-over-under.html"
    text = (ROOT / "docs" / name).read_text()
    text = restyle_rmd(text, year, "In progress")
    text = link_playoffs(text, year)
    # Keep the preseason preview one click away during the season
    text = text.replace(
        '<p class="ou-kicker">',
        f'<p class="ou-kicker"><a href="{year}-nba-over-under-preview.html">'
        'Preseason preview</a>. ', 1)
    write_both(name, text)


if __name__ == "__main__":
    mode = sys.argv[1] if len(sys.argv) > 1 else ""
    if mode == "past":
        do_past()
    elif mode == "preview" and len(sys.argv) == 3:
        do_preview(sys.argv[2])
    elif mode == "season" and len(sys.argv) == 3:
        do_season(int(sys.argv[2]))
    elif mode == "playoffs" and len(sys.argv) == 3:
        do_playoffs(int(sys.argv[2]))
    else:
        sys.exit(__doc__)
