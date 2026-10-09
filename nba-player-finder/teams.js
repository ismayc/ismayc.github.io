// Jersey-tile colors per team: bg is the tile, fg the number, trim the stripe.
// Approximations of each team's primary palette, chosen for number contrast.
export const TEAMS = {
  "Atlanta Hawks":          { abbr: "ATL", bg: "#E03A3E", fg: "#FFFFFF", trim: "#C1D32F" },
  "Boston Celtics":         { abbr: "BOS", bg: "#007A33", fg: "#FFFFFF", trim: "#BA9653" },
  "Brooklyn Nets":          { abbr: "BKN", bg: "#000000", fg: "#FFFFFF", trim: "#FFFFFF" },
  "Charlotte Hornets":      { abbr: "CHA", bg: "#1D1160", fg: "#FFFFFF", trim: "#00788C" },
  "Chicago Bulls":          { abbr: "CHI", bg: "#CE1141", fg: "#FFFFFF", trim: "#000000" },
  "Cleveland Cavaliers":    { abbr: "CLE", bg: "#860038", fg: "#FDBB30", trim: "#041E42" },
  "Dallas Mavericks":       { abbr: "DAL", bg: "#00538C", fg: "#FFFFFF", trim: "#B8C4CA" },
  "Denver Nuggets":         { abbr: "DEN", bg: "#0E2240", fg: "#FEC524", trim: "#8B2131" },
  "Detroit Pistons":        { abbr: "DET", bg: "#C8102E", fg: "#FFFFFF", trim: "#1D42BA" },
  "Golden State Warriors":  { abbr: "GSW", bg: "#1D428A", fg: "#FFC72C", trim: "#FFC72C" },
  "Houston Rockets":        { abbr: "HOU", bg: "#CE1141", fg: "#FFFFFF", trim: "#C4CED4" },
  "Indiana Pacers":         { abbr: "IND", bg: "#002D62", fg: "#FDBB30", trim: "#BEC0C2" },
  "Los Angeles Clippers":   { abbr: "LAC", bg: "#C8102E", fg: "#FFFFFF", trim: "#1D428A" },
  "Los Angeles Lakers":     { abbr: "LAL", bg: "#552583", fg: "#FDB927", trim: "#FDB927" },
  "Memphis Grizzlies":      { abbr: "MEM", bg: "#12173F", fg: "#FFFFFF", trim: "#5D76A9" },
  "Miami Heat":             { abbr: "MIA", bg: "#98002E", fg: "#FFFFFF", trim: "#F9A01B" },
  "Milwaukee Bucks":        { abbr: "MIL", bg: "#00471B", fg: "#EEE1C6", trim: "#0077C0" },
  "Minnesota Timberwolves": { abbr: "MIN", bg: "#0C2340", fg: "#FFFFFF", trim: "#78BE20" },
  "New Orleans Pelicans":   { abbr: "NOP", bg: "#0C2340", fg: "#C8A96A", trim: "#C8102E" },
  "New York Knicks":        { abbr: "NYK", bg: "#006BB6", fg: "#FFFFFF", trim: "#F58426" },
  "Oklahoma City Thunder":  { abbr: "OKC", bg: "#007AC1", fg: "#FFFFFF", trim: "#EF3B24" },
  "Orlando Magic":          { abbr: "ORL", bg: "#0077C0", fg: "#FFFFFF", trim: "#000000" },
  "Philadelphia 76ers":     { abbr: "PHI", bg: "#006BB6", fg: "#FFFFFF", trim: "#ED174C" },
  "Phoenix Suns":           { abbr: "PHX", bg: "#1D1160", fg: "#E56020", trim: "#E56020" },
  "Portland Trail Blazers": { abbr: "POR", bg: "#000000", fg: "#FFFFFF", trim: "#E03A3E" },
  "Sacramento Kings":       { abbr: "SAC", bg: "#5A2D81", fg: "#FFFFFF", trim: "#63727A" },
  "San Antonio Spurs":      { abbr: "SAS", bg: "#000000", fg: "#C4CED4", trim: "#C4CED4" },
  "Toronto Raptors":        { abbr: "TOR", bg: "#CE1141", fg: "#FFFFFF", trim: "#A1A1A4" },
  "Utah Jazz":              { abbr: "UTA", bg: "#3E2680", fg: "#FFFFFF", trim: "#79A3DC" },
  "Washington Wizards":     { abbr: "WAS", bg: "#002B5C", fg: "#FFFFFF", trim: "#E31837" },
};

export const FALLBACK = { abbr: "", bg: "#5D6370", fg: "#FFFFFF", trim: "#D3D7D2" };

export const DIVISIONS = {
  East: ["Atlantic", "Central", "Southeast"],
  West: ["Northwest", "Pacific", "Southwest"],
};
