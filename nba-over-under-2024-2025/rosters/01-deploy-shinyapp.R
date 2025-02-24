Sys.setenv(RSCONNECT_DEPLOY_QUARTO = "0")
rsconnect::deployApp(appDir = ".", appName = "nba-player-finder")
