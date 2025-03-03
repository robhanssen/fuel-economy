load("Rdata/fuel.Rdata")

sumry <- function(dat, yr = TRUE, qrtr = TRUE) {
    grouping <- c("car_name", "year", "quarter")[c(TRUE, yr, qrtr)]
    if (!qrtr) quarter <- 7 / 3

    dat %>%
        filter(!str_detect(car_name, "2008")) %>%
        summarize(
            miles_per_gallon = sum(miles) / sum(gallons),
            cost_per_gallon = sum(cost) / sum(gallons),
            cost_per_mile = sum(cost) / sum(miles),
            .by = all_of(grouping)
        ) %>%
        mutate(date = as.Date(unlist(map2(year, quarter, construct_quarter_date)), origin = "1970-01-01"))
}


time_series_graph <- function(dat, measure, colors = car_colors) {
    year_avs <-
        sumry(dat, qrtr = FALSE)

    quarter_avs <-
        sumry(dat)

    year_avs %>%
        ggplot(
            aes(x = date, y = {{ measure }}, color = car_name)
        ) +
        geom_line() +
        geom_point(
            data = quarter_avs,
            shape = 1
        ) +
        scale_color_manual(values = colors)
}


longterm_av <-
    fuel %>%
    filter(!str_detect(car_name, "2008")) %>%
    summarize(
        mpg_avs = sum(miles) / sum(gallons),
        .by = "car_name"
    ) %>%
    pull(mpg_avs)

miles_gallon_g <-
    time_series_graph(fuel, miles_per_gallon) +
    scale_y_continuous(
        labels = scales::number_format(),
        breaks = seq(16, 36, 2)
    ) +
    labs(x = "", y = "Fuel economy\n(in miles per gallon)") +
    geom_hline(
        yintercept = longterm_av,
        color = car_colors,
        linetype = 1,
        alpha = .3,
        linewidth = .5
    )


cost_gallon_g <-
    time_series_graph(fuel %>% mutate(car_name = last(car_name)), cost_per_gallon) +
    scale_y_continuous(
        limits = c(0, NA),
        labels = scales::dollar_format()
    ) +
    labs(x = "", y = "Gas price (in $/gallon)")

cost_mile_g <-
    time_series_graph(fuel, cost_per_mile) +
    scale_y_continuous(
        limits = c(0, NA),
        labels = scales::dollar_format()
    ) +
    labs(x = "", y = "Car cost (in $/mile)")


full_g <-
    miles_gallon_g / cost_gallon_g / cost_mile_g +
    plot_annotation(
        title = "Car stats"
    )

ggsave("graphs/usage_over_time.png",
    width = 10, height = 10,
    plot = full_g
)


miles_cost_g <-
    fuel %>%
    summarize(
        miles = sum(miles),
        cost = sum(cost),
        costpermile = cost / miles,
        .by = c("year", "month")
    ) %>%
    mutate(,
        ym = paste0(year, "-", as.numeric(month)),
        ym = zoo::as.yearmon(ym)
    ) %>%
    ggplot(
        aes(x = ym, y = costpermile)
    ) +
    geom_point() +
    geom_line() +
    zoo::scale_x_yearmon() +
    scale_y_continuous(
        label = scales::label_dollar(),
        limits = c(0, NA)
    ) +
    labs(x = "", y = "Average Monthly Cost per Mile ($/mile)")


ggsave("graphs/average_mileage_cost.png",
    width = 7, height = 5,
    plot = miles_cost_g
)
