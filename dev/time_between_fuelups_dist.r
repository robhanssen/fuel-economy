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

fuel %>%
    # filter(!str_detect(car_name, "2008")) %>%
    nest(data = !car_name) %>%
    mutate(
        between_fuelup = map(data, \(dat) {
            dat %>%
                arrange(date) %>%
                mutate(time_between_fuelups = as.numeric(date - lag(date))) %>%
                drop_na(time_between_fuelups) %>%
                pull(time_between_fuelups)
        })
    ) %>%
    mutate(
        mod = map(between_fuelup, \(time) {
            e_cdf <- ecdf(time)

            est_cdf <- tibble(
                t_est = seq(min(time), max(time), 1),
                e = e_cdf(t_est)
            )

            # exponectional distribution
            # m <- nls(
            #     e ~ pexp(t_est, rate = rate0),
            #     start = list(rate0 = 0.03), data = est_cdf
            # )

            # normal distribution
            # m <- nls(
            #     e ~ pnorm(t_est, mean = mean0, sd = sd0),
            #     start = list(mean0 = 20, sd0 = 5), data = est_cdf
            # )

            # lognormal distribution
            m <- nls(
                e ~ pnorm(log(t_est + 1), mean = mean0, sd = sd0),
                start = list(mean0 = log(20), sd0 = .2), data = est_cdf
            )
        })
    ) %>%
    mutate(
        estimates = map(mod, broom::augment)
    ) %>%
    unnest(estimates) %>%
    ggplot(
        aes(x = t_est, y = e, color = car_name)
    ) +
    geom_point() +
    geom_line(aes(y = .fitted)) +
    facet_wrap(vars(car_name))
