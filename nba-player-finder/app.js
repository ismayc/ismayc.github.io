import {
  parseCSV, toPlayers, filterPlayers, sortPlayers, formatHeight,
  emptyClues, cluesToQuery, queryToClues,
} from "./finder.js";
import { TEAMS, FALLBACK, DIVISIONS } from "./teams.js";

const $ = id => document.getElementById(id);
const form = $("clues"), list = $("list"), count = $("count");
const els = { name: $("name"), jersey: $("jersey"), hmin: $("hmin"), hmax: $("hmax"),
  amin: $("amin"), amax: $("amax"), team: $("team") };

let players = [];
let clues = queryToClues(location.search);
let sort = { key: "player", dir: 1 };

const esc = s => String(s).replace(/[&<>"']/g, c => `&#${c.charCodeAt(0)};`);
const opt = (value, label, sel) =>
  `<option value="${esc(String(value))}"${sel ? " selected" : ""}>${esc(label)}</option>`;

function range(lo, hi) { const out = []; for (let i = lo; i <= hi; i++) out.push(i); return out; }

function buildControls() {
  const hs = players.map(p => p.height).filter(v => v != null);
  const as = players.map(p => p.age).filter(v => v != null);
  const [hLo, hHi, aLo, aHi] = [Math.min(...hs), Math.max(...hs), Math.min(...as), Math.max(...as)];
  // The open end of each range shows the roster's extreme, so the sentence
  // reads "between 5'9" and 7'6" tall"; that option's value is "" (no clue).
  const lo = (sel, a, b, fmt) => { sel.innerHTML = range(a, b).map(v => opt(v === a ? "" : v, fmt(v))).join(""); };
  const hi = (sel, a, b, fmt) => { sel.innerHTML = range(a, b).map(v => opt(v === b ? "" : v, fmt(v), v === b)).join(""); };
  lo(els.hmin, hLo, hHi, formatHeight); hi(els.hmax, hLo, hHi, formatHeight);
  lo(els.amin, aLo, aHi, String); hi(els.amax, aLo, aHi, String);
}

// Team menu shows only teams inside the chosen conference and division.
function buildTeams() {
  const groups = {};
  for (const p of players) {
    if (clues.conference && p.conference !== clues.conference) continue;
    if (clues.division && p.division !== clues.division) continue;
    (groups[`${p.conference}ern Conference, ${p.division}`] ??= new Set()).add(p.team);
  }
  els.team.innerHTML = opt("", "any team", !clues.team) + Object.keys(groups).sort().map(g =>
    `<optgroup label="${esc(g)}">${[...groups[g]].sort().map(t => opt(t, t, t === clues.team)).join("")}</optgroup>`
  ).join("");
}

function buildDivisions() {
  const fs = $("division");
  const divs = DIVISIONS[clues.conference];
  fs.hidden = !divs;
  if (!divs) return;
  fs.innerHTML = `<legend class="sr">Division</legend>` +
    ["", ...divs].map(d => `<label><input type="radio" name="division" value="${d}"${d === clues.division ? " checked" : ""} /><span>${d || "All"}</span></label>`).join("");
}

function syncInputs() {
  els.name.value = clues.name;
  els.jersey.value = clues.jersey;
  for (const k of ["hmin", "hmax", "amin", "amax"]) els[k].value = clues[k] ?? "";
  form.querySelector(`input[name=conference][value="${clues.conference}"]`).checked = true;
  buildDivisions();
  buildTeams();
}

function tile(p) {
  const t = TEAMS[p.team] ?? FALLBACK;
  const n = p.jersey;
  return `<span class="jersey${n === "" ? " jersey-none" : ""}${n.length > 1 ? " jersey-two" : ""}" style="--bg:${t.bg};--fg:${t.fg};--trim:${t.trim}" aria-hidden="true"><span>${esc(n)}</span><small>${t.abbr}</small></span>`;
}

function row(p) {
  const num = p.jersey === "" ? "no number listed" : `number ${p.jersey}`;
  return `<li class="player">${tile(p)}<div class="who"><strong>${esc(p.player)}</strong><span class="team">${esc(p.team)}<span class="sr">, ${num}</span></span></div><span class="ht"><span class="sr">Height </span>${formatHeight(p.height)}</span><span class="age">${p.age == null ? "" : `age ${p.age}`}</span></li>`;
}

function render() {
  const hits = sortPlayers(filterPlayers(players, clues), sort.key, sort.dir);
  list.innerHTML = hits.map(row).join("");
  const n = hits.length;
  count.innerHTML = `<strong>${n}</strong> ${n === 1 ? "player matches" : "players match"}`;
  $("empty").hidden = n > 0;
  const active = cluesToQuery(clues) !== "";
  $("clear").hidden = !active;
  history.replaceState(null, "", active ? `?${cluesToQuery(clues)}` : location.pathname);
}

form.addEventListener("input", e => {
  const t = e.target;
  if (t === els.name) clues.name = t.value;
  else if (t === els.jersey) { t.value = t.value.replace(/\D/g, ""); clues.jersey = t.value; }
  else if (["hmin", "hmax", "amin", "amax"].includes(t.id)) clues[t.id] = t.value === "" ? null : +t.value;
  render();
});

form.addEventListener("change", e => {
  const t = e.target;
  if (t.name === "conference") {
    clues.conference = t.value; clues.division = "";
    if (clues.team && TEAMS[clues.team] && !teamIn(clues.team)) clues.team = "";
    buildDivisions(); buildTeams();
  } else if (t.name === "division") {
    clues.division = t.value;
    if (clues.team && !teamIn(clues.team)) clues.team = "";
    buildTeams();
  } else if (t === els.team) {
    clues.team = t.value;
  } else return;
  render();
});

// Ensure min never exceeds max: moving one end past the other drags it along.
for (const [lo, hi] of [["hmin", "hmax"], ["amin", "amax"]]) {
  els[lo].addEventListener("change", () => {
    if (clues[lo] != null && clues[hi] != null && clues[lo] > clues[hi]) { clues[hi] = clues[lo]; els[hi].value = clues[hi]; render(); }
  });
  els[hi].addEventListener("change", () => {
    if (clues[lo] != null && clues[hi] != null && clues[hi] < clues[lo]) { clues[lo] = clues[hi]; els[lo].value = clues[lo]; render(); }
  });
}

function teamIn(team) {
  const p = players.find(x => x.team === team);
  return p && (!clues.conference || p.conference === clues.conference) &&
    (!clues.division || p.division === clues.division);
}

function clear() { clues = emptyClues(); syncInputs(); render(); els.name.focus(); }
$("clear").addEventListener("click", clear);
$("clear-empty").addEventListener("click", clear);

document.querySelector(".sort").addEventListener("click", e => {
  const b = e.target.closest("button[data-sort]");
  if (!b) return;
  const key = b.dataset.sort;
  sort = { key, dir: sort.key === key ? -sort.dir : 1 };
  for (const x of document.querySelectorAll(".sort button")) {
    const on = x === b;
    x.setAttribute("aria-pressed", on);
    x.dataset.dir = on ? (sort.dir > 0 ? "asc" : "desc") : "";
  }
  render();
});

$("theme-toggle").addEventListener("click", () => {
  const root = document.documentElement;
  const dark = root.dataset.theme
    ? root.dataset.theme === "dark"
    : matchMedia("(prefers-color-scheme: dark)").matches;
  root.dataset.theme = dark ? "light" : "dark";
  try { localStorage.setItem("npf-theme", root.dataset.theme); } catch (e) {}
});

try {
  const res = await fetch("players.csv");
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  const records = parseCSV(await res.text());
  players = toPlayers(records);
  const pulled = records[0]?.date_pulled;
  if (pulled) {
    const d = new Date(pulled + "T12:00:00");
    $("pulled").textContent = `Rosters pulled ${d.toLocaleDateString("en-US", { month: "long", day: "numeric", year: "numeric" })} from the NBA and ESPN roster feeds. They refresh every Monday.`;
  }
  buildControls();
  syncInputs();
  render();
} catch (err) {
  count.textContent = `The roster file did not load (${err.message}). Reload the page to try again.`;
}
