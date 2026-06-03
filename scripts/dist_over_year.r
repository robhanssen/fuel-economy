library(tidyverse)

load("Rdata/fuel.Rdata")


use_over_year_dist <- function(dat, util) {
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
        select(date, ddate, year, cum_total)
    # ggplot(aes(x = ddate, y = cum_total, color = year, alpha = year)) +
    # geom_line() +
    # scale_x_date(date_labels = "%b") +
    # scale_color_manual(values = color_range) +
    # scale_alpha_manual(values = alpha_range)
}

date_range <-
    seq(
        floor_date(min(display_date(fuel$date), na.rm = TRUE), unit = "1 month"),
        ceiling_date(max(display_date(fuel$date), na.rm = TRUE), unit = "1 month"),
        by = "1 month"
    )[1:12]

max_year <- max(year(fuel$date), na.rm = TRUE)

year_dist_g <-
    use_over_year_dist(fuel, miles) %>%
    mutate(month = month(date)) %>%
    nest(data = -year) %>%
    mutate(
        approxed_cu_miles = map(
            data,
            # date_range,
            \(d) {
                tibble(
                    mo = date_range,
                    cu_miles = approx(x = d$ddate, y = d$cum_total, xout = date_range)$y
                )
            }
        )
    ) %>%
    unnest(approxed_cu_miles) %>%
    select(year, mo, cu_miles) %>%
    mutate(
        cu_miles = if_else(is.na(cu_miles) & month(mo) == 1, 0, cu_miles)
    ) %>%
    ggplot(aes(y = mo, x = cu_miles, group = mo)) +
    ggridges::geom_density_ridges(color = "gray70", alpha = 0.2, scale = 1) +
    geom_point(
        data = . %>% filter(year == max_year),
        aes(group = NA),
        color = "red"
    ) +
    geom_line(
        data = . %>% filter(year == max_year),
        aes(group = NA),
        color = "red"
    ) +
    scale_x_continuous(labels = scales::comma, limit = c(0, NA)) +
    coord_flip() +
    labs(
        title = "Comparison of cumulative distance distribution across years to the current year",
        subtitle = "Current year data is highlighted in red",
        y = NULL,
        x = "Cumulative distance (miles)",
    )

ggsave("graphs/distribution_over_year.png", year_dist_g, width = 8, height = 6)