// Unit tests for the static NBA Player Finder's data logic.
// Run from the repo root: node --test nba-over-under-2025-2026/rosters/tests/js/
import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import {
  parseCSV, toPlayers, filterPlayers, sortPlayers, formatHeight,
  emptyClues, cluesToQuery, queryToClues,
} from "../../../../nba-player-finder/finder.js";

const csv = `player,team,conference,division,height_feet,height_inches,height_total_inches,age,number_jersey,date_pulled
Player A,Los Angeles Lakers,West,Pacific,6,6,78,25,23,2026-10-05
Player B,Los Angeles Lakers,West,Pacific,7,0,84,30,6,2026-10-05
Player C,Boston Celtics,East,Atlantic,6,9,81,22,0,2026-10-05
"Jokić, Nikola",Denver Nuggets,West,Northwest,6,11,83,,00,2026-10-05
Player E,Miami Heat,East,Southeast,5,11,71,35,,2026-10-05
`;
const players = toPlayers(parseCSV(csv));
const names = ps => ps.map(p => p.player);
const clues = over => ({ ...emptyClues(), ...over });

test("parseCSV handles quoted commas and blank fields", () => {
  assert.equal(players.length, 5);
  assert.equal(players[3].player, "Jokić, Nikola");
  assert.equal(players[3].age, null);
  assert.equal(players[4].jersey, "");
});

test("no clues returns everyone", () => {
  assert.equal(filterPlayers(players, emptyClues()).length, 5);
});

test("name search folds accents and case", () => {
  assert.deepEqual(names(filterPlayers(players, clues({ name: "JOKIC" }))), ["Jokić, Nikola"]);
});

test("jersey matches as worn, so 0 and 00 differ", () => {
  assert.deepEqual(names(filterPlayers(players, clues({ jersey: "0" }))), ["Player C"]);
  assert.deepEqual(names(filterPlayers(players, clues({ jersey: "00" }))), ["Jokić, Nikola"]);
});

test("conference, division, and team narrow the list", () => {
  assert.equal(filterPlayers(players, clues({ conference: "East" })).length, 2);
  assert.equal(filterPlayers(players, clues({ division: "Pacific" })).length, 2);
  assert.deepEqual(names(filterPlayers(players, clues({ team: "Miami Heat" }))), ["Player E"]);
});

test("ranges are inclusive and a set range drops missing values", () => {
  assert.deepEqual(names(filterPlayers(players, clues({ hmin: 81, hmax: 83 }))), ["Player C", "Jokić, Nikola"]);
  assert.equal(filterPlayers(players, clues({ amin: 20 })).length, 4);
});

test("sorting puts missing values last in both directions", () => {
  assert.equal(sortPlayers(players, "age", 1).at(-1).age, null);
  assert.equal(sortPlayers(players, "age", -1).at(-1).age, null);
  assert.deepEqual(sortPlayers(players, "jersey", 1).map(p => p.jersey), ["00", "0", "6", "23", ""]);
});

test("formatHeight writes feet and inches", () => {
  assert.equal(formatHeight(83), `6'11"`);
  assert.equal(formatHeight(null), "");
});

test("clues round-trip through the URL query", () => {
  const c = clues({ name: "a b", jersey: "00", conference: "West", hmin: 80, amax: 30 });
  assert.deepEqual(queryToClues(cluesToQuery(c)), c);
  assert.equal(cluesToQuery(emptyClues()), "");
});

test("the published roster parses and satisfies the data contract", () => {
  const url = new URL("../../../../nba-player-finder/players.csv", import.meta.url);
  const records = parseCSV(readFileSync(url, "utf8"));
  assert.ok(records.length > 400, `only ${records.length} players`);
  for (const col of ["player", "team", "conference", "division", "height_total_inches", "age", "number_jersey", "date_pulled"]) {
    assert.ok(col in records[0], `missing column ${col}`);
  }
  const teams = new Set(records.map(r => r.team));
  assert.equal(teams.size, 30);
  for (const r of records) {
    if (r.height_total_inches !== "") {
      assert.equal(+r.height_feet * 12 + +r.height_inches, +r.height_total_inches, r.player);
    }
  }
});

test("every roster team has jersey colors", async () => {
  const { TEAMS } = await import("../../../../nba-player-finder/teams.js");
  const url = new URL("../../../../nba-player-finder/players.csv", import.meta.url);
  for (const t of new Set(parseCSV(readFileSync(url, "utf8")).map(r => r.team))) {
    assert.ok(TEAMS[t], `no colors for ${t}`);
  }
});
