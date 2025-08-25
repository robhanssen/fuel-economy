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

inaug <- ymd(c(20090121, 20170121, 20210121, 20250121))

fuel %>%
    ggplot(aes(x = date, y = price)) +
    geom_point() + 
    geom_vline(xintercept = inaug, color = "gray70", size = 1) + 
    geom_hline(yintercept = 2.56, color = "gray70", size = 1) +
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

with(fuel, sum(miles)/sum(gallons))

fuel |> 
    split(fuel$car_name) |>
    map_df(~with(., sum(miles)/sum(gallons))) %>%
    pivot_longer(everything(), names_to = "car_name", values_to = "mpg")

fuel %>%
    filter(!str_detect(car_name, "2008")) %>%
    summarize(mpg = sum(cost)/sum(gallons))        