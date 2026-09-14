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
            plot.caption = element_text(hjust = 0, size = 7)
        )
)

load("Rdata/fuel.Rdata")

fitdata <-
    lm(price ~ date,
        data = filter(fuel, date > "2020-04-25", date < "2022-03-01")
    ) %>%
    broom::augment()

fitdata2 <-
    lm(price ~ date,
        data = filter(fuel, date > "2022-12-25", date < "2026-02-28")
    ) %>%
    broom::augment()


fitdata3 <-
    lm(price ~ date,
        data = filter(fuel, date > "2026-02-28")
    ) %>%
    broom::augment()


fuel %>%
    filter(date > "2018-01-01") %>%
    ggplot(aes(x = date, y = price)) +
    geom_point(shape = 1, size = .5) +
    geom_vline(
        xintercept = ymd(c("2021-01-20")), alpha = .5, lty = 3
    ) +
    geom_line(
        data = fitdata,
        aes(y = .fitted)
    ) +
    geom_line(
        data = fitdata2,
        aes(y = .fitted)
    ) +
    geom_line(
        data = fitdata3,
        aes(y = .fitted)
    )


av_cost <-
    fuel %>%
    mutate(year = year(date)) %>%
    summarise(
        price = sum(cost) / sum(gallons),
        .by = year
    )


real_av_cost <-
    fuel %>%
    mutate(
        rollingprice = cumsum(cost) / cumsum(gallons),
        .by = year
    )

real_av_cost_early_2026 <-
    fuel %>%
    filter(date >= "2026-01-01", date <= "2026-02-28") %>%
    mutate(
        rollingprice = cumsum(cost) / cumsum(gallons),
        .by = year
    ) %>%
    last()

real_av_cost_late_2026 <-
    fuel %>%
    filter(date >= "2026-02-28") %>%
    mutate(
        rollingprice = cumsum(cost) / cumsum(gallons),
        .by = year
    ) 


fuel %>%
    filter(date >= "2021-01-01") %>%
    ggplot(aes(x = date, y = price)) +
    geom_point(shape = 1, size = .5) +
    geom_vline(
        xintercept = ymd(c("2026-02-28")), alpha = .5, lty = 3
    ) +
    geom_line(
        data = real_av_cost %>% filter(year > 2020),
        aes(x = date, y = rollingprice, group = year),
        color = "red"
    ) + 
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 year") +
    geom_label(
        data = real_av_cost %>% filter(year > 2020) %>% slice_max(date, by = year),
        aes(x = date, y = rollingprice, label = scales::dollar(rollingprice, 0.01)),
        vjust = -0.5, hjust = 1, color = "red"
    ) +
    geom_label(
        data = real_av_cost_early_2026,
        aes(x = date, y = rollingprice, label = scales::dollar(rollingprice, 0.01)),
        vjust = 0.5, hjust = -0.1, color = "gray30"
    ) +
    geom_line(
        data = real_av_cost_late_2026,
        aes(x = date, y = rollingprice),
        color = "gray60"
    ) +
        geom_label(
        data = real_av_cost_late_2026 %>% slice_max(date, by = year),
        aes(x = date, y = rollingprice, label = scales::dollar(rollingprice, 0.01)),
        vjust = -0.9, hjust = 1, color = "gray60"
    ) + 
    labs(
        x = NULL,
        y = "Fuel Price ($/gallon)",
        title = "Fuel Price and Rolling Average",
        caption = "Rolling average is calculated as the cumulative cost divided by the cumulative gallons purchased."
    )

ggsave("dev/cumulative_gasprice.png", width = 8, height = 5, dpi = 300)