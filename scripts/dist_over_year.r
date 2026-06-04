library(tidyverse)

source("defaults.r")
load("Rdata/fuel.Rdata")

use_over_year_dist <- function(dat, util) {
    year_range <- unique(year(dat$date))

    dat %>%
        mutate(
            year = factor(year(date)),
            ddate = display_date(date)
        ) %>%
        group_by(year) %>%
        mutate(cum_total = cumsum({{ util }})) %>%
        ungroup() %>%
        select(date, ddate, year, cum_total)
}

calc_date_range <- function(dat) {
    seq(
        floor_date(min(display_date(dat$date), na.rm = TRUE), unit = "1 month"),
        ceiling_date(max(display_date(dat$date), na.rm = TRUE), unit = "1 month"),
        by = "1 month"
    )[1:12]
}


distribution_over_year <- function(dat, util) {
    date_range <-
        calc_date_range(fuel)

    max_year <- max(year(fuel$date), na.rm = TRUE)

    use_over_year_dist(dat, {{ util }}) %>%
        mutate(month = month(date)) %>%
        nest(data = -year) %>%
        mutate(
            approxed_cu_util = map(
                data,
                \(d) {
                    tibble(
                        mo = date_range,
                        cu_util = approx(x = d$ddate, y = d$cum_total, xout = date_range)$y
                    )
                }
            )
        ) %>%
        unnest(approxed_cu_util) %>%
        select(year, mo, cu_util) %>%
        mutate(
            cu_util = if_else(is.na(cu_util) & month(mo) == 1, 0, cu_util)
        ) %>%
        ggplot(aes(y = mo, x = cu_util, group = mo)) +
        ggridges::geom_density_ridges(color = "gray70", alpha = 0.2, scale = 1) +
        geom_point(
            data = . %>% filter(year == max_year),
            aes(group = NA),
            color = "black"
        ) +
        geom_line(
            data = . %>% filter(year == max_year),
            aes(group = NA),
            color = "black"
        ) +
        coord_flip()
}

cost_g <-
    distribution_over_year(fuel, cost) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(
        y = NULL,
        x = "Cumulative cost (in dollars)",
    )

miles_g <-
    distribution_over_year(fuel, miles) +
    scale_x_continuous(labels = scales::comma) +
    labs(
        y = NULL,
        x = "Cumulative distance (in miles)",
    ) +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    )

gallons_g <-
    distribution_over_year(fuel, gallons) +
    scale_x_continuous(labels = scales::comma) +
    labs(
        y = NULL,
        x = "Cumulative fuel (in gallons)",
    ) +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    )


p <- miles_g / gallons_g / cost_g +
    plot_annotation(
        title = "Comparison of cumulative distributions across years to the current year",
        caption = "Current year data is highlighted in black"
    )

ggsave("graphs/distribution_over_year.png", p, width = 8, height = 10)
