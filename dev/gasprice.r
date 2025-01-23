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

fitdata <-
    lm(price ~ date,
        data = filter(fuel, date > "2020-04-25", date < "2022-03-01")
    ) %>%
    broom::augment()

fitdata2 <-
    lm(price ~ date,
        data = filter(fuel, date > "2022-12-25")
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
    )