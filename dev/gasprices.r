library(tidyverse)

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

inaug <- ymd(c(20090121, 20170121, 20210121))

fuel %>%
    ggplot(aes(x = date, y = price)) +
    geom_point() + 
    geom_vline(xintercept = inaug, color = "gray70", size = 1) + 
    scale_y_continuous(
        labels = scales::label_dollar(),
        limits = c(0, NA)
    )

fuel %>%
    filter(date > inaug[2], date < inaug[3]) %>%
    summarize(
        mean(price),
        sd(price),
        min(price),
        max(price),
    )

fuel %>%
    filter(date > inaug[2], date > inaug[3]) %>%
    summarize(
        mean(price),
        sd(price),
        min(price),
        max(price),
    )