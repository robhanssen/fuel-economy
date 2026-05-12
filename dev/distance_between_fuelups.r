library(tidyverse)
library(patchwork)
library(ggtext)

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

quantiles <- c(0, 1)

# altima_out <- boxplot(fuel$miles[str_detect(fuel$car_name, "2013")], plot = FALSE)$out
# quest_out <- boxplot(fuel$miles[str_detect(fuel$car_name, "2011")], plot = FALSE)$out

# altima_all <- fuel %>%
#     filter(str_detect(car_name, "2013")) %>%
#     mutate(outlier = miles %in% altima_out) %>%
#     filter(!outlier)

# quest_all <- fuel %>%
#     filter(str_detect(car_name, "2011")) %>%
#     mutate(outlier = miles %in% quest_out) %>%
#     filter(!outlier)

# fuel_alt <- bind_rows(altima_all, quest_all)

fuel_alt2 <-
    fuel %>%
    filter(!str_detect(car_name, "2008")) %>%
    nest(data = -car_name) %>%
    mutate(
        outliers = map(data, ~ boxplot(.x$miles, plot = FALSE)$out),
        filtered_data = map2(data, outliers, ~ filter(.x, !miles %in% .y))
    ) %>%
    select(-data, -outliers) %>%
    # do a normality test on filtered_data
    mutate(
        normality_test = map(filtered_data, ~ shapiro.test(.x$miles)),
        p_value = map_dbl(normality_test, "p.value")
    ) %>%
    unnest(filtered_data)


qqplot_g <-
    fuel_alt2 %>%
    ggplot(aes(sample = miles, color = car_name)) +
    geom_qq() +
    geom_qq_line() +
    scale_color_manual(values = car_colors) +
    geom_vline(
        data = tibble(
            car_name = rep(levels(fuel_alt2$car_name)[1:2], each = 4),
            xint = rep(c(-2, 0, 1, 2), 2)
        ),
        mapping = aes(xintercept = xint),
        color = "gray40",
        linetype = rep(c(1, 2, 1, 1), 2),
        linewidth = rep(c(2, .5, .5, 2), 2),
        alpha = rep(c(0.05, 1, 1, 0.05), 2)
    ) +
    facet_wrap(~car_name, ncol = 1, scale = "free_y") +
    labs(
        # y = "Distance (in mile)",
        x = "Normal distribution (in z-values)",
        title = "Validation of normality"
    ) +
    theme(
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 1)
    )


subtitle <- paste0(
    "Calculated at ",
    paste0("the ", round(100 * pnorm(quantiles), 0), "th ", "(<I>z=", quantiles, "</I>) ", collapse = " and "),
    " percentile"
)

dist_g <-
    fuel_alt2 %>%
    summarize(
        av_dist_0 = quantile(miles, pnorm(quantiles[1])),
        av_dist_1 = quantile(miles, pnorm(quantiles[2])),
        .by = c(car_name, year)
    ) %>%
    ggplot(aes(x = year, y = av_dist_1, fill = car_name, color = car_name)) +
    geom_line() +
    geom_line(aes(y = av_dist_0), linetype = "dashed") +
    geom_point(shape = 21, size = 3) +
    geom_point(aes(y = av_dist_0), shape = 21, size = 3, fill = "white") +
    # facet_wrap(~car_name) +
    scale_fill_manual(values = car_colors) +
    scale_color_manual(values = car_colors) +
    scale_x_continuous(breaks = seq(2010, 2030, 5)) +
    annotate(
        geom = "text", x = c(2017, 2023), y = c(310, 455),
        label = levels(fuel$car_name)[1:2], color = car_colors
    ) +
    labs(
        x = NULL, y = "Distance (in miles)",
        title = "Distance between fuel-ups",
        subtitle = subtitle
    ) +
    theme(
        plot.subtitle = element_markdown()
    )

quantiles



all_plot <-
    dist_g + qqplot_g + plot_layout(width = c(4, 3)) +
    plot_annotation(
        caption = "Note: outliers were removed prior to analysis according to 1.5 IQR rule"
    )

ggsave("graphs/distance_between_fuelup.png", width = 10, height = 8, plot = all_plot)





fuel_alt2 %>%
    filter(!str_detect(car_name, "2008")) %>%
    summarize(
        av_dist_0 = quantile(miles, pnorm(0)),
        av_dist_1 = quantile(miles, pnorm(1)),
        .by = c(car_name, year)
    ) %>%
    filter(str_detect(car_name, "2011")) %>%
    lm(av_dist_1 ~ year, data = .) %>%
    summary()
