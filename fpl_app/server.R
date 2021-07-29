library(shiny)
library(data.table)

league_id <- 348449
url <- paste0("https://fantasy.premierleague.com/api/leagues-h2h/", league_id, "/standings")
league <- fromJSON(url)
standings <- setDT(league$new_entries$results)
standings[, points := 0]
standings[, money_won := 0]
standings[, rounds_owed := 0]
n_players <- nrow(standings)
team_names <- standings$entry_name

shinyServer(function(input, output) {

    output$t1 <- renderTable({
        standings
    })

    output$t2 <- renderTable({
        data.table(Team = team_names,
                   `League Points` = rep(0, n_players),
                   `Overall Points` = rep(0, n_players),
                   `Highest Score` = rep(0, n_players),
                   `Lowest Score` = rep(0, n_players),
                   `First Wildcard` = rep(0, n_players),
                   `Second Wildcard` = rep(0, n_players),
                   `Triple Captain` = rep(0, n_players),
                   `Bench Boost` = rep(0, n_players),
                   `Free Hit` = rep(0, n_players))
    }, digits = 0)

    output$t3 <- renderTable({
        breakdown <- data.table(Round = c("League Points", "Overall Points", "Highest Score",
                                          "Lowest Score", "First Wildcard", "Second Wildcard",
                                          "Triple Captain", "Bench Boost", "Free Hit"),
                                Prize = c("£60 (1st), £30 (2nd)", "", "£10", "", "£10", "£10",
                                          "10", "£10", "£10"),
                                Forfeit = c("TBC", "Round", "", "Round", "Round", "Round",
                                            "Round", "Round", "Round"))

    })

})
