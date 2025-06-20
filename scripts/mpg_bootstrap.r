library(tidyverse)

load("Rdata/fuel.Rdata")


fuel_new <- fuel %>%
    filter(!str_detect(car_name, "2008"))


bootstrapping <- function(dat, n = 100) {
    max <- nrow(dat)

    sample_list <- map(1:n, \(q) sample(1:max, max, replace = TRUE))

    map_dbl(sample_list, \(sample) {
        with(dat[sample, ], sum(miles) / sum(gallons))
    })
}

n_bootstrap <- 5000

means <- fuel_new %>%
    filter(year > 2013) %>%
    summarize(
        mean = sum(miles)/sum(gallons),
        .by = car_name
    )


fuel_new %>%
    filter(year > 2013) %>%
    nest(data = !c(car_name, year)) %>%
    mutate(
        mpg_dist = map(data, bootstrapping, n = n_bootstrap)
    ) %>%
    unnest_longer(mpg_dist) %>%
    ggplot(
        aes(x = mpg_dist, y = factor(year), fill = car_name)
    ) +
    ggridges::geom_density_ridges(alpha = .5) +
    geom_vline(
        data = means,
        aes(xintercept = mean, color = car_name),
        linewidth = 2, alpha = .33
    ) +
    scale_fill_manual(values = car_colors) +
    scale_color_manual(values = car_colors) +
    scale_x_continuous(
        breaks = seq(0, 100, 2)
    ) +
    geom_label(
        data = means,
        aes(x = mean, label = car_name, color = car_name), y = 0.7,
        fill = "white", label.size = NA
    ) +
    coord_cartesian(clip = "off") +
    labs(
        x = "Bootstrapped fuel economy distribution (in miles per gallon)",
        y = NULL,
        caption = glue::glue(
            "Distributions based on {n_bootstrap} bootstrap iterations. ",
            "Vertical line indicates long-term average"
        ),
        title = "Estimated fuel economy distribution by year"
    ) +
    theme(
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot"
    )

ggsave("graphs/mpg_average_bootstrap.png", width = 6, height = 8)


?geom_label
