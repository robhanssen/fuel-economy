load("Rdata/fuel.Rdata")

ttest_mpg_comparison <- function(dat, fltr = "") {
    dat_filtered <-
        dat %>%
        filter(str_detect(car_name, fltr))

    this_year <- year(today())

    years <- unique(na.exclude(dat_filtered$year))
    years <- years[which(years != this_year)]

    mean_curyear <-
        with(
            dat_filtered,
            mean(mpg[year == this_year], na.rm = TRUE)
        )

    map_df(
        years,
        ~ bind_cols(tibble(year = .x), dat_filtered %>%
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
        scale_y_continuous(
            labels = scales::label_number(),
            sec.axis = sec_axis(~ . / mean_curyear - 1, labels = scales::percent_format())
        ) +
        geom_text(aes(
            x = year, y = mean_curyear + conf.high + .15,
            label = scales::pvalue(p.value)
        )) +
        geom_hline(yintercept = mean_curyear, linetype = "dashed")
}

ttest_g <-
    (ttest_mpg_comparison(fuel, "2011") +
        labs(
            x = "", y = "",
            title = "2011 Nissan Quest"
        )
    ) /
    (ttest_mpg_comparison(fuel, "2013") +
        labs(
            x = "", y = "",
            title = "2013 Nissan Altima"
        )
    ) +
    plot_annotation(
        title = glue::glue("T-test comparison of fuel economy comparing {year(today())} with previous years"),
        subtitle = "Showing 95% confidence interval"
    )


ggsave("graphs/ttest-comparison-mpg.png",
    width = 8, height = 8,
    plot = ttest_g
)
