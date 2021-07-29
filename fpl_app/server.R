library(shiny)
library(data.table)

league_id <- 348449
url <- paste0("https://fantasy.premierleague.com/api/leagues-h2h/", league_id, "/standings")
league <- fromJSON(url)
standings <- setDT(league$new_entries$results)
standings <- standings[, .(Team = entry_name)]
n_players <- nrow(standings)
standings[, Points := runif(n_players, 30, 50)]
standings[, `Money Won` := 0]
standings[, `Rounds owed` := 0]
setorder(standings,- Points)
team_names <- standings$Team

all_data <- data.table(Team = team_names,
                       `League Points` = standings$Points,
                       `Overall Points` = runif(n_players, 1000, 2000),
                       `Highest Score` = runif(n_players, 70, 130),
                       `Lowest Score` = runif(n_players, 20, 35),
                       `First Wildcard` = runif(n_players, 25, 100),
                       `Second Wildcard` = runif(n_players, 25, 100),
                       `Triple Captain` = runif(n_players, 0, 50),
                       `Bench Boost` = runif(n_players, 40, 130),
                       `Free Hit` = runif(n_players, 30, 100))

shinyServer(function(input, output) {

    output$t1 <- renderTable({
        standings
    }, digits = 0)

    output$t2 <- renderTable({
        all_data
    }, digits = 0)

    output$t3 <- renderTable({
        breakdown <- data.table(Round = c("League Points", "Overall Points", "Highest Score",
                                          "Lowest Score", "First Wildcard", "Second Wildcard",
                                          "Triple Captain", "Bench Boost", "Free Hit"),
                                Prize = c("£60 (1st), £30 (2nd)", "", "£10", "", "£10", "£10",
                                          "£10", "£10", "£10"),
                                Forfeit = c("TBC", "Round", "", "Round", "Round", "Round",
                                            "Round", "Round", "Round"),
                                Winner = "",
                                Loser = "")
        calc_all(breakdown, all_data)
        breakdown

    })

})
