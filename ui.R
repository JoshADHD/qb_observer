##  Load packages
library(tidyverse)
library(shiny)

load("/srv/shiny-server/qb_observer/data/psrs.RData") # "/srv/shiny-server/qb_observer/data/psrs.RData")
load("/srv/shiny-server/qb_observer/data/qbo_x.RData") # "/srv/shiny-server/qb_observer/data/qbo_stats.RData")

psrs_list <- psrs$player_id
names(psrs_list) <- psrs$full_name

stat_list <- qbo_x$stat
names(stat_list) <- qbo_x$name

##  Page layout
shinyUI(
    fluidPage(
        title = "FantasyADHD QB Observer App",
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "qbo.css")
        ),
        div(class = "overhead",
            h1("NFL QB Observer App"),
            h4("Brought to you by ", a("@FantasyADHD", href = "https://twitter.com/FantasyADHD", target = "_blank")),
            wellPanel(
                class = "donate_panel",
                # h5(a("Interested in joining an #MFL10? ", href = "https://www54.myfantasyleague.com/2017/public#0", target = "_blank")),
                h6("Enjoying the QB Observer App? If you'd like to donate, ", a("click here!", href = "https://paypal.me/FantasyADHD", target = "_blank")))
        ),
        tabsetPanel(
            id = "odp_pills",
            type = "pills",
            tabPanel(
                title = "Season Level",
                id = "qbo_pop_tab",
                class = "qbo_pop_tab",
                column(3, class = "input_sidecol",
                    wellPanel(
                        h5(style = "font-size: 16px; font-weight: 700; color: #337ab7", "Data Filters"),
                        selectizeInput(inputId = "qb_select", label = "Select QB", choices = psrs_list, selected = sample(psrs_list, 1, replace = TRUE)),
                        selectizeInput(inputId = "stat_select", label = "Select Stat", choices = stat_list, selected = sample(stat_list, 1, replace = TRUE)),
                        fluidRow(
                            h5(style = "font-size: 16px; font-weight: 700; color: #337ab7", "Pass Attempt Filter"),
                            column(6, numericInput(inputId = "minms_select", label = "Min", value = 100, min = 50, max = 800, step = 25, width = "100%")),
                            column(6, numericInput(inputId = "maxms_select", label = "Max", value = 800, min = 100, max = 800, step = 25, width = "100%"))
                        ),
                        actionButton(inputId = "qbo_pop_button", label = "Plot!", width = "100%")
                    )),
                column(6, class = "table_col",
                    dataTableOutput("qbo_table"),
                    uiOutput("qbo_pop_plot")
                )
            # ),
            # tabPanel(
            #     title = "Player Level",
            #     id = "qbo_player_tab",
            #     class = "pse_game_tab",
            #     column(2, class = "input_sidecol",
            #            wellPanel(
            #                h5(style = "font-size: 16px; font-weight: 700; color: #337ab7", "/srv/shiny-server/qb_observer/data Filters"),
            #                selectizeInput(inputId = "team_select_gm", label = "Select Team", choices = teams, selected = "ARI", multiple = FALSE),
            #                selectizeInput(inputId = "year_select_gm", label = "Select Season", choices = 2016:2009, selected = 2016, multiple = FALSE),
            #                uiOutput("opp_ui"),
            #                actionButton(inputId = "go_button2", label = "Plot!", width = "100%")
            #            )),
            #     column(6, class = "table_col",
            #            plotOutput("pse_plot_gm")
            #     )
            )
        )
    )
)