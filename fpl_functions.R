calc_win_lose <- function(x, round, win = TRUE) {
    if (win) {
        score <- x[, max(get(round))]
    } else {
        score <- x[, min(get(round))]
    }
    teams <- x[get(round) == score, Team]
    return(paste(teams, collapse = ", "))
}

calc_all <- function(table, x) {

    rounds <- c("League Points", "Overall Points", "Highest Score",
                "Lowest Score", "First Wildcard", "Second Wildcard",
                "Triple Captain", "Bench Boost", "Free Hit")

    lapply(rounds, function(y) {
        table[Round == y, Winner := calc_win_lose(x, y, TRUE)]
        table[Round == y, Loser := calc_win_lose(x, y, FALSE)]
    })

    return(table)

}
