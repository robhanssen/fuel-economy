load("Rdata/fuel.Rdata")


longterm_av <-
    fuel %>%
    filter(!str_detect(car_name, "2008")) %>%
    summarize(
        mpg_avs = sum(miles) / sum(gallons),
        .by = "car_name"
    ) %>%
    pull(mpg_avs)


comp_g <-
    fuel %>%
    filter(!str_detect(car_name, "2008")) %>%
    summarize(
        q_upper = quantile(mpg, 0.95),
        q_lower = quantile(mpg, 0.05),
        q_mean = mean(mpg, na.rm = TRUE),
        sd = sd(mpg),
        n = n(),
        .by = c(car_name, year)
    ) %>%
    mutate(
        t_lower = q_mean + qt(0.05, df = n - 1) * sd / sqrt(n),
        t_upper = q_mean + qt(0.95, df = n - 1) * sd / sqrt(n)
    ) %>%
    ggplot(aes(x = factor(year), y = q_mean, color = car_name)) +
    geom_point() +
    geom_errorbar(
        aes(ymin = t_lower, ymax = t_upper),
        width = .2
    ) +
    scale_color_manual(
        values = car_colors
    ) +
    geom_hline(
        yintercept = longterm_av,
        color = car_colors,
        linetype = 1,
        alpha = .3,
        linewidth = .5
    ) +
    labs(x = "", y = "Average Annual Fuel Economy\n(in miles per gallon)")

ggsave("graphs/fueleconomy-change.png",
    width = 6, height = 4,
    plot = comp_g
)
