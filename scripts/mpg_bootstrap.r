library(tidyverse)
library(patchwork)
library(furrr)
plan(multisession)

load("Rdata/fuel.Rdata")

n_bootstrap <- 10000

fuel_new <- fuel %>%
    filter(!str_detect(car_name, "2008"), year > 2013)

bootstrapping <- function(dat, n = 100) {
    max <- nrow(dat)

    sample_list <- map(1:n, \(q) sample(1:max, max, replace = TRUE))

    map_dbl(sample_list, \(sample) {
        with(dat[sample, ], sum(miles) / sum(gallons))
    })
}

real_means <- fuel_new %>%
    summarize(
        mean = sum(miles) / sum(gallons),
        .by = car_name
    )


generate_bootstraps <- function(dat, var, custom_var) {
    bootstraps <- dat %>%
        nest(data = !c(car_name, {{ var }})) %>%
        mutate(
            mpg_dist = future_map(data, bootstrapping, n = n_bootstrap, .options = furrr_options(seed = TRUE))
        )

    mean_labels <- bootstraps %>%
        mutate(meanse = map(mpg_dist, quantile, probs = c(.025, .5, 0.975))) %>%
        unnest_wider(meanse) %>%
        mutate(
            label = glue::glue("{round(`2.5%`, digits = 1)} - {round(`97.5%`, digits = 1)}"),
            year = factor({{ var }})
        )

    bootstraps %>%
        unnest_longer(mpg_dist) %>%
        ggplot(
            aes(x = mpg_dist, y = factor({{ var }}), fill = car_name)
        ) +
        ggridges::geom_density_ridges(alpha = .5) +
        geom_vline(
            data = real_means,
            aes(xintercept = mean, color = car_name),
            linewidth = 2, alpha = .33
        ) +
        scale_fill_manual(values = car_colors) +
        scale_color_manual(values = car_colors) +
        scale_x_continuous(
            breaks = seq(0, 100, 2)
        ) +
        geom_label(
            data = real_means,
            aes(x = mean, label = car_name, color = car_name), y = 0.7,
            fill = "white", label.size = NA
        ) +
        geom_text(
            data = mean_labels,
            aes(
                x = .5 + `97.5%`, y = year,
                label = label, color = car_name
            ),
            hjust = 0, vjust = -.5
        ) +
        coord_cartesian(clip = "off") +
        labs(
            x = "Bootstrapped fuel economy distribution (in miles per gallon)",
            y = NULL,
            title = glue::glue("Estimated fuel economy distribution by {custom_var}")
        ) +
        theme(
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0, size = 6),
            plot.title.position = "plot"
        )
}

p <- generate_bootstraps(fuel_new, year, "year") +
    generate_bootstraps(fuel_new, month, "month") +
    plot_annotation(
        caption = glue::glue(
            "Distributions based on {scales::number(n_bootstrap)} bootstrap iterations. ",
            "Vertical line indicates long-term average. ",
            "Number ranges indicate 95% expected range"
        )
    )

ggsave(
    filename = "graphs/mpg_average_bootstrap.png",
    width = 12, height = 8, plot = p
)
