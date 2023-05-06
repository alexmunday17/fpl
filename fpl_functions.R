calc_win_lose <- function(x, round, win = TRUE) {
    if (round == "League Points") {
        if (win) {
            return(paste0(x[1, Team], " (1st), ", x[2, Team], " (2nd)"))
        } else {
            return(x[.N, Team])
        }
    }
    if (round %in% c("Lowest Score", "Overall Points") & win) {
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

    rounds <- c("League Points",
                "Overall Points",
                "Lowest Score",
                "Highest Non-Chip Score",
                "Wildcard Sum",
                "Triple Captain",
                "Bench Boost",
                "Free Hit")

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

    win_money <- c(0, 0, 10, 10, 10, 10, 10) / n_winners
    lose_rounds <- 1 / n_losers

    teams <- summary_table$Team

    money_won <- rep(0, length(teams))
    rounds_owed <- rep(0, length(teams))

    for (i in 1:length(teams)) {
        money_won[i] <- sum(win_money[grepl(teams[i], winners)])
        rounds_owed[i] <- sum(lose_rounds[grepl(teams[i], losers)])
    }

    money_won[1] <- money_won[1] + 50
    money_won[2] <- money_won[2] + 20

    money_won <- money_won - 20

    money_won <- fifelse(money_won >= 0,
                         paste0("£", round(money_won, 2)),
                         paste0("-£", abs(round(money_won, 2))))

    return(list(money_won, round(rounds_owed, 2)))

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

    # wildcard for world cup
    if (Sys.Date() < "2023-08-01") {
        gws[event == 17, name := "wildcard"]
    }

    min_score <- gws[points != 0, min(points - event_transfers_cost)]
    if (gws[name == "3xc", .N]) {
        tc <- get_tc_score(player_id, gws[name == "3xc", event])
    } else {
        tc <- 0
    }
    fh <- max(0, gws[name == "freehit", sum(points)])
    bb <- max(0, gws[name == "bboost", sum(points - event_transfers_cost)])
    wc_sum <- max(0, gws[name == "wildcard", sum(points)])
    max_non_chip <- gws[is.na(name), max(points)]

    return(list(`Lowest Score` = min_score,
                `Highest Non-Chip Score` = max_non_chip,
                `Wildcard Sum` = wc_sum,
                `Triple Captain` = tc,
                `Bench Boost` = bb,
                `Free Hit` = fh))
}

get_tc_score <- function(player_id, gw) {
    url <- paste0("https://fantasy.premierleague.com/api/entry/",
                  player_id, "/event/", gw, "/picks/")
    info_list <- fromJSON(url)

    team <- info_list$picks
    setDT(team)
    tc_id <- team[multiplier == 3, element]

    tc_url <- paste0("https://fantasy.premierleague.com/api/element-summary/", tc_id, "/")
    tc_info_list <- fromJSON(tc_url)

    tc_scores <- tc_info_list$history
    setDT(tc_scores)
    tc_score <- tc_scores[round == gw, sum(total_points)]

    return(3 * tc_score)
}

get_expected_points <- function(player_ids) {

    out <- NULL

    for (i in player_ids) {
        url <- paste0("https://fantasy.premierleague.com/api/entry/", i, "/history/")
        info_list <- fromJSON(url)
        gws <- info_list$current
        pts <- gws$points - gws$event_transfers_cost
        out <- rbind(out,
                     data.table(gw = 1:length(pts),
                                player_id = i,
                                pts = pts))
    }

    if (length(player_ids) %% 2) {
        url <- "https://fantasy.premierleague.com/api/bootstrap-static/"
        info_list <- fromJSON(url)
        average_scores <- info_list$events[, c("id", "average_entry_score")]
        setDT(average_scores)
        out <- rbind(out, average_scores[, .(gw = id, player_id = 0, pts = average_entry_score)])
    }

    out[, expected_pts := 0]

    for (i in 1:max(out$gw)) {
        for (j in unique(out$player_id)) {
            player_score <- out[player_id == j & gw == i, pts]
            other_scores <- out[player_id != j & gw == i, pts]
            exp_pts <- mean(player_score > other_scores) * 3 + mean(player_score == other_scores)
            out[player_id == j & gw == i, expected_pts := exp_pts]
        }
    }

    out <- out[, .(exp_pts = sum(expected_pts)), by = player_id]

    return(out)

}
