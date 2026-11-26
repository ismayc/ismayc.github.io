// Cross-check: run the player tracker's own scoring code (copied at run time from
// nba-fantasy-draft's web/players_template.html) over the saved daily points, and
// compare each roster's strength and fantasy points a game with summary.json.
//
// Usage: node check_strength.mjs <players_template.html> <players.json> <history.json> <summary.json>
// Exit 0 when every value matches to 1e-6, 1 otherwise.
import fs from 'node:fs';

const [tplPath, metaPath, histPath, sumPath] = process.argv.slice(2);
const tpl = fs.readFileSync(tplPath, 'utf8');
const meta = JSON.parse(fs.readFileSync(metaPath, 'utf8'));
const hist = JSON.parse(fs.readFileSync(histPath, 'utf8'));
const sum = JSON.parse(fs.readFileSync(sumPath, 'utf8'));

// The page code from the fantasy-points formula through rosterTotals, and pointRows.
const cut = (from, to) => {
  const i = tpl.indexOf(from), j = tpl.indexOf(to, i);
  if (i < 0 || j < 0) throw new Error(`marker not found: ${from} / ${to}`);
  return tpl.slice(i, j);
};
const code = cut('const fp = (s)', 'function longDate')
  + cut("const PROGRESS_POINT_KEYS", "const PROGRESS_ORDER")
  + cut('function pointRows', 'function buildProgress');
const run = new Function('BALLOT', 'DRAFT_ORDER', `${code}; return { pointRows, rosterTotals };`);
const { pointRows, rosterTotals } = run(meta.ballot, meta.draft_order);

const picks = meta.results;
const byDate = new Map(hist.points.map((p) => [p.date, p]));
const fa = sum.fantasy;
let bad = 0, n = 0;
const check = (label, a, b) => {
  n++;
  if (Math.abs(a - b) > 1e-6) { bad++; console.log(`MISMATCH ${label}: page ${a} email ${b}`); }
};
const totalsOn = (date) => {
  const list = pointRows(byDate.get(date), picks);
  return Object.fromEntries(meta.draft_order.map((m) => [m, rosterTotals(list.filter((r) => r.manager === m))]));
};
const t1 = totalsOn(fa.day);
const t0 = fa.prev ? totalsOn(fa.prev) : null;
const tw = fa.week ? totalsOn(fa.week) : null;
for (const row of fa.table) {
  const m = row.manager;
  check(`${m} strength ${fa.day}`, t1[m].sumStrength, row.strength);
  check(`${m} fpts ${fa.day}`, t1[m].sumFp, row.fp);
  if (t0 && row.d_day != null) check(`${m} 1-day change`, t1[m].sumStrength - t0[m].sumStrength, row.d_day);
  if (tw) check(`${m} 7-day change`, t1[m].sumStrength - tw[m].sumStrength, row.d_week);
}
console.log(`${n - bad} of ${n} values match the tracker's code (${fa.day}, ${fa.prev}, ${fa.week})`);
process.exit(bad ? 1 : 0);
