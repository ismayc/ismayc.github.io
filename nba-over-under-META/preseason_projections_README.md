# Preseason win projections, 2021-22 to 2026-27

`preseason_projections.csv` holds published team win projections, one row per
source, season, and team: `source, season, team, proj_wins, published, url`.
Collected on October 13, 2026 to score which sources have beaten the pool's
lines. Every number was read from the page in `url`; none were estimated.

| source | seasons | notes |
|---|---|---|
| `bleacher_report` | 2021-22 to 2026-27 | August schedule-release articles. Published before camp news the October line knows. |
| `darko` | 2021-22 to 2025-26 | kmedved's Google Sheets linked from the APBRmetrics contest threads. 2021-22 is the "Preseason-Only Model" column of a sheet titled "Final Win Projections"; 2024-25 preseason timing is unverified; 2025-26 is the revised sheet after a small fix. |
| `espn_bpi` | 2023-24, 2025-26, 2026-27 | 2023-24 and 2025-26 are Wayback Machine captures one or two games into the season (Oct 28, 2023 and Oct 24, 2025). The Sept 27, 2024 capture showed the finished 2023-24 season and was left out. |
| `espn_forecast` | 2025-26, 2026-27 | ESPN's summer panel. The 2023-24 panel is behind ESPN+. |
| `fivethirtyeight_raptor`, `fivethirtyeight_elo` | 2021-22, 2022-23 | Earliest preseason entry in the archived data.json. FiveThirtyEight stopped after 2022-23. |
| `teamrankings_betiq` | 2021-22 to 2023-24 | Preseason rankings, weighted average wins. |
| `lebron` | 2021-22, 2023-24 | BBall Index. 2021-22 comes from the same kmedved sheet as DARKO, so its preseason timing is inferred. |
| `dunks_threes_epm` | 2021-22 | Same kmedved sheet; preseason timing inferred. |
| `other_bbipm` | 2023-24 | BBall Index sheet. |
| `other_craftednba` | 2023-24 to 2025-26 | Posted by its author in the APBR threads around opening night. |
| `other_knarsu_stats_only` | 2024-25, 2025-26 | A hobbyist stats-only model. |
| `kalshi` | 2026-27 | 10 West teams only, from market data as of July 29, 2026. |

Scored against the pool lines for 2021-22 to 2025-26 (150 team-seasons), no
source had a smaller average miss than the line. DARKO picked the right side of
the line 60% of the time (149 picks), the only source clearly above a coin
flip.
