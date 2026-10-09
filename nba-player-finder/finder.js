// Pure data logic for the NBA Player Finder: CSV parsing, filtering, sorting,
// and URL-state round-tripping. No DOM access, so it runs under `node --test`
// (see nba-over-under-2025-2026/rosters/tests/js/finder.test.mjs).

// Minimal RFC 4180 parser: handles quoted fields, doubled quotes, and CRLF.
export function parseCSV(text) {
  const rows = [];
  let row = [], field = "", inQuotes = false;
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (inQuotes) {
      if (c === '"' && text[i + 1] === '"') { field += '"'; i++; }
      else if (c === '"') inQuotes = false;
      else field += c;
    } else if (c === '"') inQuotes = true;
    else if (c === ",") { row.push(field); field = ""; }
    else if (c === "\n" || c === "\r") {
      if (c === "\r" && text[i + 1] === "\n") i++;
      row.push(field); rows.push(row); row = []; field = "";
    } else field += c;
  }
  if (field !== "" || row.length) { row.push(field); rows.push(row); }
  const [header, ...body] = rows.filter(r => r.length > 1 || r[0] !== "");
  return body.map(r => Object.fromEntries(header.map((h, j) => [h, r[j] ?? ""])));
}

const toInt = s => (s === "" || s == null || isNaN(+s) ? null : Math.round(+s));

// Turn raw CSV records into typed player objects.
export function toPlayers(records) {
  return records.map(r => ({
    player: r.player,
    team: r.team,
    conference: r.conference,
    division: r.division,
    height: toInt(r.height_total_inches),
    age: toInt(r.age),
    jersey: (r.number_jersey ?? "").trim(),
  }));
}

export const formatHeight = inches =>
  inches == null ? "" : `${Math.floor(inches / 12)}'${inches % 12}"`;

// Fold accents and case so "jokic" finds "Jokić".
export const fold = s =>
  s.normalize("NFD").replace(/[̀-ͯ]/g, "").toLowerCase();

// Default clues: everything open. Ranges are inclusive; null means unbounded.
export const emptyClues = () => ({
  name: "", jersey: "", conference: "", division: "", team: "",
  hmin: null, hmax: null, amin: null, amax: null,
});

// A player passes when every clue that is set matches. A set range clue drops
// players whose value is missing, like the Shiny app's dplyr::between() did.
// Jersey matches the number as worn, so "0" and "00" stay distinct.
export function filterPlayers(players, clues) {
  const q = fold(clues.name.trim());
  const jersey = clues.jersey.trim();
  const inRange = (v, lo, hi) =>
    (lo == null && hi == null) || (v != null && (lo == null || v >= lo) && (hi == null || v <= hi));
  return players.filter(p =>
    (!q || fold(p.player).includes(q)) &&
    (!jersey || p.jersey === jersey) &&
    (!clues.conference || p.conference === clues.conference) &&
    (!clues.division || p.division === clues.division) &&
    (!clues.team || p.team === clues.team) &&
    inRange(p.height, clues.hmin, clues.hmax) &&
    inRange(p.age, clues.amin, clues.amax));
}

// Sort keys; missing values always go last regardless of direction.
const KEYS = {
  player: p => p.player.split(" ").slice(1).join(" ") + " " + p.player,
  team: p => p.team + " " + p.player,
  jersey: p => (p.jersey === "" ? null : p.jersey === "00" ? -1 : +p.jersey),
  height: p => p.height,
  age: p => p.age,
};

export function sortPlayers(players, key = "player", dir = 1) {
  const get = KEYS[key] ?? KEYS.player;
  return [...players].sort((a, b) => {
    const x = get(a), y = get(b);
    if (x == null || y == null) return (x == null) - (y == null);
    return (typeof x === "string" ? x.localeCompare(y) : x - y) * dir ||
      a.player.localeCompare(b.player);
  });
}

const PARAMS = { name: "q", jersey: "n", conference: "conf", division: "div",
  team: "team", hmin: "hmin", hmax: "hmax", amin: "amin", amax: "amax" };
const NUMERIC = new Set(["hmin", "hmax", "amin", "amax"]);

export function cluesToQuery(clues) {
  const sp = new URLSearchParams();
  for (const [k, p] of Object.entries(PARAMS)) {
    const v = clues[k];
    if (v !== "" && v != null) sp.set(p, String(v));
  }
  return sp.toString();
}

export function queryToClues(search) {
  const sp = new URLSearchParams(search);
  const clues = emptyClues();
  for (const [k, p] of Object.entries(PARAMS)) {
    if (!sp.has(p)) continue;
    clues[k] = NUMERIC.has(k) ? toInt(sp.get(p)) : sp.get(p);
  }
  return clues;
}
