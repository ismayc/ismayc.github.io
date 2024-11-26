library(tibble)
library(readr)

players <- tibble(
  name = c(
    # Guards
    "Luka Doncic, DAL", "Shai Gilgeous-Alexander, OKC", "Jalen Brunson, NYK", "Tyrese Haliburton, IND", 
    "Stephen Curry, GSW", "Tyrese Maxey, PHI", "Devin Booker, PHX", "Donovan Mitchell, CLE", 
    "Ja Morant, MEM", "Damian Lillard, MIL", "De’Aaron Fox, SAC", "Trae Young, ATL", "Kyrie Irving, DAL",
    "Jamal Murray, DEN", "Cade Cunningham, DET", "LaMelo Ball, CHA", "James Harden, LAC", "Derrick White, BOS", 
    "Darius Garland, CLE", "Jrue Holiday, BOS", "Dejounte Murray, NOP", "Jalen Green, HOU", "Fred VanVleet, HOU",
    "Coby White, CHI", "CJ McCollum, NOP", "D’Angelo Russell, LAL", "Immanuel Quickley, TOR", "Malik Monk, SAC", 
    "Cam Thomas, BKN", "Donte DiVincenzo, MIN",
    # Wings
    "Jayson Tatum, BOS", "Anthony Edwards, MIN", "Kevin Durant, PHX", "LeBron James, LAL", "Jaylen Brown, BOS",
    "Kawhi Leonard, LAC", "Paul George, PHI", "Jimmy Butler, MIA", "Scottie Barnes, TOR", "Jalen Williams, OKC", 
    "DeMar DeRozan, SAC", "Brandon Ingram, NOP", "Franz Wagner, ORL", "Desmond Bane, MEM", "Mikal Bridges, NYK",
    "OG Anunoby, NYK", "Brandon Miller, CHA", "Zach LaVine, CHI", "Austin Reaves, LAL", "Michael Porter Jr., DEN",
    "Miles Bridges, CHA", "Jalen Johnson, ATL", "Tyler Herro, MIA", "Josh Giddey, CHI", "Bradley Beal, PHX", 
    "Trey Murphy III, NOP", "Devin Vassell, SAS", "Herbert Jones, NOP", "Jaden McDaniels, MIN", "Josh Hart, NYK",
    # Posts
    "Nikola Jokic, DEN", "Giannis Antetokounmpo, MIL", "Joel Embiid, PHI", "Anthony Davis, LAL", 
    "Victor Wembanyama, SAS", "Domantas Sabonis, SAC", "Zion Williamson, NOP", "Paolo Banchero, ORL", 
    "Pascal Siakam, IND", "Bam Adebayo, MIA", "Karl-Anthony Towns, NYK", "Rudy Gobert, MIN", "Lauri Markkanen, UTA",
    "Julius Randle, MIN", "Alperen Sengun, HOU", "Chet Holmgren, OKC", "Jaren Jackson Jr., MEM", 
    "Kristaps Porzingis, BOS", "Evan Mobley, CLE", "Myles Turner, IND", "Aaron Gordon, DEN", "Jarrett Allen, CLE", 
    "Kyle Kuzma, WAS", "Jerami Grant, POR", "Keegan Murray, SAC", "Jonathan Kuminga, GSW", "Jabari Smith Jr., HOU",
    "Nikola Vucevic, CHI", "Tobias Harris, DET", "Naz Reid, MIN"
  ),
  position = c(
    rep("Guard", 30), 
    rep("Wing", 30), 
    rep("Post", 30)
  )
)

write_rds(players, "~/Desktop/repos/ismayc.github.io/nba-over-under-2024-2025/fantasy-draft/players_ballot.rds")