calc_win_lose <- function(x, round, win = TRUE) {
    if (round == "League Points") {
        if (win) {
            return(paste0(x[1, Team], " (1st), ", x[2, Team], " (2nd)"))
        } else {
            return("")
        }
    }
    if (round == "Lowest Score" & win) {
        return("")
    }
    if (win) {
        score <- x[, max(get(round))]
    } else {
        score <- x[, min(get(round))]
    }
    teams <- x[get(round) == score, Team]
    return(paste(teams, collapse = ", "))
}

calc_all <- function(table, x) {

    rounds <- c("League Points", "Overall Points",
                "Lowest Score", "First Wildcard", "Second Wildcard",
                "Triple Captain", "Bench Boost", "Free Hit")

    lapply(rounds, function(y) {
        table[Round == y, Winner := calc_win_lose(x, y, TRUE)]
        table[Round == y, Loser := calc_win_lose(x, y, FALSE)]
    })

    return(table)

}

calc_money_rounds <- function(summary_table, breakdown_table) {
    winners <- breakdown_table$Winner[-1]
    losers <- breakdown_table$Loser[-1]

    n_winners <- str_count(winners, ",") + 1
    n_losers <- str_count(losers, ",") + 1

    win_money <- c(15, 0, 10, 10, 10, 10, 10) / n_winners
    lose_rounds <- 1 / n_losers

    teams <- summary_table$Team

    money_won <- rep(0, length(teams))
    rounds_owed <- rep(0, length(teams))

    for (i in 1:length(teams)) {
        money_won[i] <- sum(win_money[grepl(teams[i], winners)])
        rounds_owed[i] <- sum(lose_rounds[grepl(teams[i], losers)])
    }

    money_won[1] <- money_won[1] + 50
    money_won[2] <- money_won[2] + 25

    return(list(paste0("£", round(money_won, 2)), round(rounds_owed, 1)))

}

get_gw_scores <- function(player_id) {
    url <- paste0("https://fantasy.premierleague.com/api/entry/", player_id, "/history/")
    info_list <- fromJSON(url)

    gws <- info_list$current
    chips <- info_list$chips

    if (length(chips)) {
        gws <- merge(gws, chips, by = "event", all.x = TRUE)
    } else {
        gws$name <- ""
        gws$time <- Sys.Date()
    }
    setDT(gws)

    min_score <- min(gws$points)
    tc <- max(0, gws[name == "3xc", points])
    fh <- max(0, gws[name == "fh", points])
    bb <- max(0, gws[name == "bb", points])
    wc1 <- max(0, gws[name == "wc" & time < "2022-01-01", points])
    wc2 <- max(0, gws[name == "wc" & time > "2022-01-01", points])

    return(list(`Lowest Score` = min_score,
                `First Wildcard` = wc1,
                `Second Wildcard` = wc2,
                `Triple Captain` = tc,
                `Bench Boost` = bb,
                `Free Hit` = fh))
}
