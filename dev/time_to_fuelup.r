library(tidyverse)
library(patchwork)

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

load("Rdata/fuel.Rdata")


altima_tank_size <- 18.25 # gallon

altima <-
    fuel %>%
    filter(str_detect(car_name, "2013"))


miles_cdf <- ecdf(altima$miles)

cdf <-
    tibble(
        distance = seq(200, 700, 10),
        cdf = miles_cdf(distance)
    )

fitdata <- altima %>%
    mutate(
        fuel_left = altima_tank_size - gallons,
        miles_left = fuel_left * mpg,
        mpg_cat = cut(mpg, breaks = c(0, seq(20, 32, 3), 100))
    )

full_set_g <-
    fitdata %>%
    lm(miles_left ~ miles, data = .) %>%
    broom::augment(interval = "prediction") %>%
    ggplot(
        aes(x = miles, y = .fitted)
    ) +
    geom_line() +
    geom_line(aes(y = .lower), lty = 2) +
    geom_line(aes(y = .upper), lty = 2) +
    geom_point(
        data = fitdata,
        shape = 1,
        aes(y = miles_left, color = mpg_cat),
    ) +
    coord_cartesian(
        ylim = c(0, 300),
        xlim = c(200, 700)
    ) +
    labs(
        x = "Miles driven on one tank",
        y = "Predicted miles left before empty tank",
        caption = "Indication of 95% confidence interval"
    )


cut_mpg_g <-
    fitdata %>%
    nest(data = !mpg_cat) %>%
    mutate(
        fuelmod = map(data, ~ lm(miles_left ~ miles, data = .x)),
        milesleftdata = map(fuelmod, ~ broom::augment(.x, newdata = tibble(miles = 200:800), interval = "prediction", conf.level = .99))
    ) %>%
    unnest(milesleftdata) %>%
    ggplot(aes(x = miles, y = .lower, color = mpg_cat)) +
    geom_line(lty = 1) +
    coord_cartesian(
        ylim = c(0, 300),
        xlim = c(NA, 700)
    ) +
    geom_point(
        data = fitdata,
        shape = 1,
        aes(y = miles_left, color = mpg_cat),
    ) +
    # geom_hline(yintercept = 25, lty = 2) +
    labs(
        x = "Miles driven on one tank",
        y = "Predicted miles left before empty tank",
        caption = "Indication of 95% confidence interval; colors indicate MPG categories"
    )



max_miles_g <-
    fitdata %>%
    nest(data = !mpg_cat) %>%
    mutate(
        fuelmod = map(data, ~ lm(miles_left ~ miles, data = .x)),
        milesleftdata = map(fuelmod, ~ broom::augment(.x, newdata = tibble(miles = 200:800), interval = "prediction"))
    ) %>%
    mutate(timetotank = map(milesleftdata, ~ approx(.x$.lower, .x$miles, xout = 0))) %>%
    unnest_wider(timetotank) %>%
    arrange(mpg_cat) %>%
    ggplot(
        aes(x = y, y = mpg_cat, fill = mpg_cat)
    ) +
    geom_col() +
    geom_text(aes(x = y + 20, label = scales::number(y), fill = NULL)) +
    coord_cartesian(
        xlim = c(200, 700)
    ) +
    labs(
        x = "Miles driven on single tank before you need to refuel",
        y = "MPG category",
        caption = "Indication of lower 5% percentile"
    )


p <- (cut_mpg_g +full_set_g) / (max_miles_g + plot_spacer())  +
    plot_annotation(
        title = "When should I fill up my 2013 Nissan Altima?",
        subtitle = "When the miles counter reaches this number, you'll be fine in 95% of cases."
    )

ggsave("dev/when_to_fill_up.png",
        width = 10, height = 8,
        plot = p)