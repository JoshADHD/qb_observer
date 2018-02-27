library(tidyverse)
library(caTools)
library(cowplot)
library(showtext)
library(shiny)
library(DT)

quickRound <- function(
    num,
    accuracy,
    method = round
) {
    method(num / accuracy) * accuracy
}

font.add.google("Roboto", "robo")
font.add.google("Roboto Mono", "robo_mono")
showtext.auto()

load("/srv/shiny-server/qb_observer/data/players.RData") # /srv/shiny-server/qb_observer/
load("/srv/shiny-server/qb_observer/data/qb_sum2.RData")
load("/srv/shiny-server/qb_observer/data/psrs.RData")
# load("/srv/shiny-server/qb_observer/data/psr_melt.RData")
# load("/srv/shiny-server/qb_observer/data/psr_strips.RData")
load("/srv/shiny-server/qb_observer/data/qbo_x.RData") # /srv/shiny-server/qb_observer/


shinyServer(function(input, output, session) {

    qbo_reac <- eventReactive(input$qbo_pop_button, {

        qb_melt <- qb_sum %>%
            filter(pa_att >= input$minms_select & pa_att <= input$maxms_select)

        qb_obs <- nrow(qb_melt)

        qb_melt <- qb_melt %>% reshape2::melt(id = c("player_id", "team", "season_year"))

        qb_data <- qb_melt %>% filter(variable %in% input$stat_select)

        qb_line <- qb_data %>%
            filter(player_id %in% input$qb_select) %>%
            mutate(
                alpha_year = (season_year - 2007) * 1/9
            ) %>%
            arrange(-season_year, variable)

        qb_text <- qb_line %>%
            group_by(variable) %>%
            summarise(
                min = min(value, na.rm = TRUE),
                mu = mean(value, na.rm = TRUE),
                max = max(value, na.rm = TRUE)
            )

        xlab <- qbo_x %>% filter(stat == input$stat_select) %>% pull(name)

        qb_title <- paste0(
            "QB Observer  \u2014  ",
            psrs %>% filter(player_id %in% input$qb_select) %>% pull(full_name),
            "  (",
            min(qb_line$season_year), " to ",
            max(qb_line$season_year),
            ")"
        )

        qb_sub <- paste0(
            "Among All QBs 2009 to 2016  |  ",
            input$minms_select, " to ", input$maxms_select, " Passing Attempts  |  ",
            qb_obs, " Observations"
        )

        return(list(
            "qb_melt" = qb_melt,
            "qb_data" = qb_data,
            "qb_line" = qb_line,
            "qb_text" = qb_text,
            "stats" = input$stat_select,
            "qb_title" = qb_title,
            "qb_sub" = qb_sub,
            "xlab" = xlab
        ))
    })

    # output$qbo_table <- DT::renderDataTable({
    #
    #     DT::datatable(
    #         qbo_reac()$qb_text
    #     )
    #
    # })

    output$qbo_pop_render <- renderPlot({

        p <- qbo_reac()$qb_melt %>%
            filter(variable %in% qbo_reac()$stats) %>%
            ggplot() +
            geom_density(aes(x = value, fill = "1"), boundary = 0, adjust = 0.5, alpha = 0.38, size = 0.2, color = "#333333")

        pb <- ggplot_build(p)

        xr <- labeling::extended(pb$layout$panel_ranges[[1]]$x.range[1] * 0.9, pb$layout$panel_ranges[[1]]$x.range[2] * 1.1, 12)
        yr <- labeling::extended(0, pb$layout$panel_ranges[[1]]$y.range[2] * 2, 8)

        p <- p +
            geom_vline(
                data = qbo_reac()$qbo_data,
                aes(xintercept = mean(value, na.rm = TRUE)), color = "#375e97",
                size = 0.7
            ) +
            geom_vline(
                data = qbo_reac()$qb_line,
                aes(xintercept = value, alpha = alpha_year),
                size = 0.7, color = "#333333"
            ) +
            geom_label(
                data = qbo_reac()$qb_line,
                aes(x = value, y = seq(max(yr) * 0.9, max(yr) * 0.2, length.out = 8) %>% .[1:length(season_year)], label = season_year),
                size = 4, hjust = 0.5, vjust = 0.5
            ) +
            geom_label(
                data = qbo_reac()$qbo_data,
                aes(x = mean(value, na.rm = TRUE), y = max(yr), label = "AVG QB"),
                size = 4
            ) +
            scale_alpha_identity() +
            geom_text(
                data = qbo_reac()$qb_text,
                aes(
                    x = max(xr), y = max(yr),
                    label = paste(
                        "Player Data",
                        paste("Max", ifelse(max %% 1 > 0, sprintf("%.3f", max), max)),
                        paste("Avg", ifelse(mu %% 1 > 0, sprintf("%.3f", mu), mu)),
                        paste("Min", ifelse(min %% 1 > 0, sprintf("%.3f", min), min)),
                        sep = "\n"
                    )),
                size = 6, family = "robo_mono", fontface = "bold", hjust = 1, vjust = 1.1
            ) +
            scale_x_continuous(limits = c(min(xr), max(xr)), breaks = xr) +
            scale_y_continuous(limits = c(0, max(yr)), breaks = yr) +
            scale_fill_brewer(type = "qual", palette = "Set1") +
            theme_bw(base_size = 16, base_family = "robo") +
            theme(
                # text = element_text(size = 16, family = "robo"),
                plot.title = element_text(lineheight = 1.125, face = "bold"),
                plot.subtitle = element_text(lineheight = 1.125, face = "plain"),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank(),
                strip.text = element_text(face = "bold"),
                axis.title = element_text(face = "bold", size = rel(1)),
                legend.position = "none"
            ) +
            labs(
                title = qbo_reac()$qb_title,
                subtitle = paste0(
                    qbo_reac()$qb_sub, "\n",
                    "FantasyADHD.com/qb_observer/"
                ),
                x = qbo_reac()$xlab,
                y = "Occurrences"
            )

        ggdraw(p)

    })

    output$qbo_pop_plot <- renderUI({

        plotOutput("qbo_pop_render", width = 720, height = 480)

    })

})