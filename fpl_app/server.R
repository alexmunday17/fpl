library(shiny)
library(data.table)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    league_data <- reactive({

        league_id <- 119062
        url <- paste0("https://fantasy.premierleague.com/api/leagues-h2h/", league_id, "/standings")
        league <- fromJSON(url)


        player_names <- c("alex", "ben", "jake", "tk", "will")
        n_players <- length(player_names)
        data.table(player = player_names,
                   league_pts = rep(0, n_players),
                   overall_pts = rep(0, n_players),
                   max_pts = rep(0, n_players),
                   first_wc = rep(0, n_players),
                   second_wc = rep(0, n_players),
                   triple_captain = rep(0, n_players),
                   bench_boost = rep(0, n_players),
                   free_hit = rep(0, n_players))
    })

    output$t1 <- renderTable({
        fpl_data()
    })

    output$t2 <- renderTable({
        league_data()
    })

    output$t3 <- renderTable({
        fpl_data()
    })

    output$t4 <- renderTable({
        fpl_data()
    })

    output$t5 <- renderTable({
        fpl_data()
    })

    output$t6 <- renderTable({
        fpl_data()
    })

    output$t7 <- renderTable({
        fpl_data()
    })

    output$t8 <- renderTable({
        fpl_data()
    })

})
