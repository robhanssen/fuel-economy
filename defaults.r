library(tidyverse)
library(patchwork)
library(RColorBrewer)

theme_set(
    theme_light() +
    theme(
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0)
    )
)

col_fill <- "gray70"

car_colors <-
    c("2011 Nissan Quest 3.5SL" = "#36072d", "2013 Nissan Altima 2.5SV" = "#941100")



display_date <- function(date, year_now = year(today())) {
    date + years(year_now - year(date))
}

construct_quarter_date <- function(year, quarter, day = 15) {
    lubridate::ymd(paste(year, quarter * 3 - 1, day, sep ="-"))
}

annual_use_graph <- function(dat, util) {
    dat %>%
        mutate(year = as.integer(year(date))) %>%
        summarize(
            total = sum({{ util }}),
            .by = c("car_name", "year"),
        ) %>%
        ggplot(aes(x = year, y = total, fill = car_name)) +
        scale_x_continuous(breaks = scales::pretty_breaks()) +
        geom_col(alpha = .8) + 
        scale_fill_manual(values = car_colors)
}

use_over_year_graph <- function(dat, util) {
    year_range <- unique(year(dat$date))
    color_range <- c(rep("gray70", length(year_range) - 1), "black")
    alpha_range <- c(rep(.3, length(year_range) - 1), .8)

    dat %>%
        mutate(
            year = factor(year(date)),
            ddate = display_date(date)
        ) %>%
        group_by(year) %>%
        mutate(cum_total = cumsum({{ util }})) %>%
        ungroup() %>%
        ggplot(aes(x = ddate, y = cum_total, color = year, alpha = year)) +
        geom_line() +
        scale_x_date(date_labels = "%b") +
        scale_color_manual(values = color_range) +
        scale_alpha_manual(values = alpha_range)
}