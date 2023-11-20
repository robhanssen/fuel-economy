load("Rdata/fuel.Rdata")

fuel_altima <-
    fuel %>%
    filter(str_detect(car_name, "2011"))

this_year <- year(today())

years <- unique(na.exclude(fuel_altima$year))
years <- years[which(years != this_year)]

mean_curyear <-
    with(
        fuel_altima,
        mean(mpg[year == this_year], na.rm = TRUE)
    )

mpg_g <-
    map_df(
        years,
        ~ bind_cols(tibble(year = .x), fuel_altima %>%
            filter(year %in% c(.x, this_year)) %>%
            mutate(cur_year = ifelse(year == this_year,
                "thisyear", year
            )) %>%
            t.test(mpg ~ cur_year, data = .) %>%
            broom::tidy())
    ) %>%
    ggplot(aes(x = year)) +
    geom_point(aes(y = estimate1 - estimate2 + mean_curyear)) +
    geom_errorbar(
        aes(
            ymin = conf.high + mean_curyear,
            ymax = conf.low + mean_curyear
        ),
        width = .3
    ) +
    scale_x_continuous(breaks = seq(2000, 2100, 1)) +
    scale_y_continuous(labels = scales::label_number(),
            sec.axis = sec_axis(~ . / mean_curyear - 1, labels = scales::percent_format())) +
    geom_text(aes(
        x = year, y = mean_curyear + conf.high + .15,
        label = scales::pvalue(p.value)
    )) +
    geom_hline(yintercept = mean_curyear, linetype = "dashed") +
    labs(
        x = "", y = glue::glue("MPG"),
        title = glue::glue(
            "MPG comparison between",
            " {this_year} and other years"
        ),
    )

time_g <-
    map_df(
        years,
        ~ bind_cols(tibble(year = .x), homesales %>%
            filter(saleyear %in% c(.x, this_year)) %>%
            mutate(
                cur_year = ifelse(saleyear == this_year, "thisyear", saleyear),
                timeonmarket = as.numeric(timeonmarket)
            ) %>%
            t.test(timeonmarket ~ cur_year, data = .) %>%
            broom::tidy())
    ) %>%
    ggplot(aes(x = year)) +
    geom_point(aes(y = estimate1 - estimate2 + meantime_curyear)) +
    geom_errorbar(
        aes(
            ymin = conf.high + meantime_curyear,
            ymax = conf.low + meantime_curyear
        ),
        width = .3
    ) +
    geom_text(aes(
        x = year, y = conf.high + 8 + meantime_curyear,
        label = scales::pvalue(p.value)
    )) +
    geom_hline(yintercept = 0 + meantime_curyear, linetype = "dashed") +
    labs(
        x = "", y = glue::glue("Time on market"),
        title = glue::glue(
            "Time on market comparison between",
            " {this_year} and other years"
        ),
    )

ggsave("graphs/time-price-comparison.png",
    width = 8, height = 6,
    plot = salesprice_g / time_g
)