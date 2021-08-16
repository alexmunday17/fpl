player_names <- c("alex", "ben", "jake", "tk", "will")
n_players <- length(player_names)

fpl <- data.frame(league_pts = rep(0, n_players),
                  overall_pts = rep(0, n_players),
                  max_pts = rep(0, n_players),
                  first_wc = rep(0, n_players),
                  second_wc = rep(0, n_players),
                  triple_captain = rep(0, n_players),
                  bench_boost = rep(0, n_players),
                  free_hit = rep(0, n_players))

rownames(fpl) <- player_names


player_ids <- 1750198
league_id <- 348449

url <- paste0("https://fantasy.premierleague.com/api/leagues-h2h/", league_id, "/standings")
league <- fromJSON(url)
standings <- setDT(league$new_entries$results)




url <- paste0("https://fantasy.premierleague.com/api/entry/", player_ids, "/history/")
info_list <- fromJSON(url)






# to do

# logic for money + rounds
# set up dummy tables ready to be replaced


