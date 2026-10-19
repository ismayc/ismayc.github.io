"""Build analysis/chance-over-season.html from backtest/chance_over_season.csv.

One chart per season (2021-22 to 2025-26): each player's chance to win the
pool, week by week, from the in-season model. Player colors match the season
pages (09-season-graphics.R).

Run from the season folder: python3 backtest/chance_over_season_page.py
"""
import csv
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
COLORS = {"Adonis": "#2a78d6", "Andy": "#eb6834", "Chester": "#1baf7a",
          "Jake": "#eda100", "Jenelle": "#ad1457", "Mary": "#e87ba4",
          "Mike J": "#008300", "Phil": "#6250d6", "Ryan": "#e34948"}

rows = list(csv.DictReader(open(HERE / "chance_over_season.csv")))
seasons = {}
for r in rows:
    s = seasons.setdefault(r["season"], {"dates": [], "games": [], "players": {}})
    if r["date"] not in s["dates"]:
        s["dates"].append(r["date"])
        s["games"].append(round(float(r["games_played"]), 1))
    p = s["players"].setdefault(r["player"], {"p": [], "final": None})
    p["p"].append(round(float(r["p_first"]), 4))
    if r["final_rank"] not in ("", "NA"):
        p["final"] = int(r["final_rank"])

page = (HERE / "chance_over_season_template.html").read_text()
page = page.replace("__DATA__", json.dumps({"seasons": seasons, "colors": COLORS}))
out = HERE.parent / "analysis" / "chance-over-season.html"
out.write_text(page)
print("Wrote", out)
